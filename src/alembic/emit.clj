; SPDX-License-Identifier: EPL-2.0
(ns alembic.emit
  "Faust DSP source emitter for alembic patch graphs.

  (emit-faust graph) → Faust DSP source string

  The produced string is a complete Faust program — import, signal
  definitions in topological order, and a process declaration — suitable
  for passing directly to the Faust compiler (faust -lang cpp, -lang wasm,
  etc.) to produce a CLAP plugin via the faust2clap toolchain.

  Design decisions captured in ~/org/areas/music/synthesis.org §Compilation
  route and §Sample rate management."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Topological sort
;; ---------------------------------------------------------------------------

(defn- topo-sort
  "Return node ids in topological order (all dependencies before dependents).
  Edges marked :feedback? true are excluded from the ordering so that history
  cycles do not prevent the sort from terminating."
  [nodes edges]
  (let [deps     (reduce (fn [acc {:keys [from to feedback?]}]
                           (if feedback?
                             acc
                             (update acc to (fnil conj #{}) from)))
                         {}
                         edges)
        ;; Iterate keys in ascending node-index order for deterministic output.
        sorted-ids (sort-by #(try (Integer/parseInt (subs (name %) 1))
                                  (catch Exception _ Integer/MAX_VALUE))
                            (keys nodes))
        visited  (atom #{})
        order    (atom [])]
    (letfn [(visit [id]
              (when-not (contains? @visited id)
                (swap! visited conj id)
                (doseq [dep (sort-by #(try (Integer/parseInt (subs (name %) 1))
                                           (catch Exception _ Integer/MAX_VALUE))
                                     (get deps id))]
                  (visit dep))
                (swap! order conj id)))]
      (doseq [id sorted-ids]
        (visit id)))
    @order))

;; ---------------------------------------------------------------------------
;; Identifier and numeric helpers
;; ---------------------------------------------------------------------------

(defn- node-ident
  "Faust identifier for a node id keyword (:n0 → \"n0\")."
  [id]
  (name id))

(defn- fmt-num
  "Format a number as a Faust float literal.
  Uses BigDecimal plain-string form to avoid scientific notation (e.g. 1.0E-4),
  which Faust does not accept."
  [x]
  (let [s (-> (BigDecimal/valueOf (double x)) .stripTrailingZeros .toPlainString)]
    (if (str/includes? s ".") s (str s ".0"))))

;; ---------------------------------------------------------------------------
;; Param hslider declaration
;; ---------------------------------------------------------------------------

(defn- hslider
  [{:keys [name]} params-schema]
  (let [nm           (clojure.core/name name)
        sch          (get params-schema name {})
        [lo hi]      (get sch :range [0.0 1.0])
        default      (get sch :default 0.0)
        step         (max 1e-4 (/ (- (double hi) (double lo)) 1e4))]
    (format "hslider(\"%s\", %s, %s, %s, %s)"
            nm
            (fmt-num default)
            (fmt-num lo)
            (fmt-num hi)
            (fmt-num step))))

;; ---------------------------------------------------------------------------
;; Per-node right-hand side expression
;; ---------------------------------------------------------------------------

(defn- node-rhs
  "Return the Faust right-hand side string for a single node."
  [node params-schema]
  (let [i #(node-ident (get (:inputs node) %))]
    (case (:op node)
      :const    (fmt-num (:value node))
      :param    (hslider node params-schema)
      :phasor   (format "os.phasor(1.0, %s)" (i :freq))
      :sine-bi  (format "sin(2.0*ma.PI*%s)" (i :input))
      :sine-uni (format "(0.5 + 0.5*sin(2.0*ma.PI*%s))" (i :input))
      :tri      (format "(1.0 - abs(2.0*%s - 1.0))" (i :input))
      :rect     (format "float(%s < %s)" (i :input) (i :width))
      :mul      (format "(%s * %s)" (i :a) (i :b))
      :add      (format "(%s + %s)" (i :a) (i :b))
      :sub      (format "(%s - %s)" (i :a) (i :b))
      :div      (format "(%s / %s)" (i :a) (i :b))
      :history  (format "%s'" (i :input))
      :delay    (format "de.fdelay(192000, %s, %s)" (i :time) (i :input))
      :sah      (format "ba.sAndH(%s, %s)" (i :trigger) (i :input))
      :delta    (let [inp (i :input)] (format "(%s - %s')" inp inp))
      :wrap     (format "(%s + fmod((%s - %s), (%s - %s)))"
                        (i :lo) (i :input) (i :lo) (i :hi) (i :lo))
      :fold     (let [inp (i :input) lo (i :lo) hi (i :hi)]
                  ;; Triangle fold: hi - abs(fmod(inp-lo, 2*(hi-lo)) - (hi-lo))
                  (format "(%s - abs(fmod((%s - %s), 2.0*(%s - %s)) - (%s - %s)))"
                          hi inp lo hi lo hi lo))
      :clip     (format "max(%s, min(%s, %s))" (i :lo) (i :hi) (i :input))
      :smooth   (format "(%s : si.smoo)" (i :input))
      :ar-env   (format "en.ar(%s, %s, %s)" (i :attack) (i :release) (i :gate))
      ;; Level 1 filters
      ;; ve.moogLadder(normFreq, Q, x) — Zavalishin ZDF 4-pole Moog ladder.
      ;; resonance [0,1] maps to Q [0.707107, 25.0]: self-oscillates at resonance=1.
      :ladder   (format "ve.moogLadder(%s, (0.707107 + %s * 24.292893), %s)"
                        (i :cutoff) (i :resonance) (i :input))
      :faust    (:source node)
      (throw (ex-info (str "Unknown op in Faust emitter: " (:op node))
                      {:node node})))))

;; ---------------------------------------------------------------------------
;; Top-level emitter
;; ---------------------------------------------------------------------------

(defn emit-faust
  "Emit a complete Faust DSP source string from a normalised alembic patch graph.

  The graph must be the value produced by defpatch! or constructed directly
  as documented in design-notes-alembic-datamodel.md.

  The emitted program imports stdfaust.lib, defines every node in topological
  order, then declares process from the patch outputs."
  [{:keys [nodes edges params outputs] :as _graph}]
  (when (empty? outputs)
    (throw (ex-info "Cannot emit Faust: patch has no outputs" {})))
  (let [order   (topo-sort nodes edges)
        defs    (mapv (fn [id]
                        (let [node (get nodes id)]
                          (str (node-ident id) " = "
                               (node-rhs node params)
                               ";")))
                      order)
        proc    (let [sorted (sort-by :channel outputs)
                       max-ch (if (seq sorted) (:channel (last sorted)) -1)
                       by-ch  (into {} (map (fn [o] [(:channel o) (node-ident (:node o))]) sorted))]
                   (->> (range (inc max-ch))
                        (map #(get by-ch % "0.0"))
                        (str/join ", ")))
        sections [["import(\"stdfaust.lib\");"]
                  [""]
                  defs
                  [""]
                  [(str "process = " proc ";")]]]
    (str/join "\n" (apply concat sections))))
