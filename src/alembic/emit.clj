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
      :ladder      (format "ve.moogLadder(%s, (0.707107 + %s * 24.292893), %s)"
                           (i :cutoff) (i :resonance) (i :input))
      ;; fi.svf_morph(freq_hz, Q, blend, x) — ZDF SVF with continuous LP/BP/HP morph.
      ;; cutoff [0,1] → Hz (linear: 0→0, 1→Nyquist via ma.SR*0.5).
      ;; resonance [0,1] → Q [0.5, 10].
      ;; mode [0,1] → blend [0,2]: 0=LP, 0.5=BP, 1=HP.
      :svf         (format "fi.svf_morph((%s * ma.SR * 0.5), (0.5 + %s * 9.5), (%s * 2.0), %s)"
                           (i :cutoff) (i :resonance) (i :mode) (i :input))
      ;; si.smooth(c) — one-pole LP; cutoff [0,1] → pole coeff [1→0] (0=max-smooth, 1=pass-through)
      :one-pole    (format "(%s : si.smooth(max(0.0, min(0.9999, 1.0 - %s))))"
                           (i :in) (i :cutoff))
      ;; fi.dcblocker — 1-pole HP that removes DC bias
      :dc-block    (format "(%s : fi.dcblocker)"
                           (i :in))
      ;; de.apf(maxDel, delSamples, coeff) — Schroeder allpass; time in seconds → samples
      :allpass     (format "(%s : de.apf(192000, %s * ma.SR, %s))"
                           (i :in) (i :time) (i :coeff))
      ;; Linear VCA: output = in × level
      :vca         (format "(%s * %s)"
                           (i :in) (i :level))
      ;; Asymmetric first-order lag; rise/fall in seconds → RC time constants
      ;; select2(cond, rising-branch, falling-branch) ~ _ (1-sample feedback)
      :slew        (format (str "(select2(%s > _, "
                                "%s * (1.0 - exp(-1.0 / max(1.0, %s * ma.SR)))"
                                " + _ * exp(-1.0 / max(1.0, %s * ma.SR)), "
                                "%s * (1.0 - exp(-1.0 / max(1.0, %s * ma.SR)))"
                                " + _ * exp(-1.0 / max(1.0, %s * ma.SR))) ~ _)")
                           (i :in)
                           (i :in) (i :rise) (i :rise)
                           (i :in) (i :fall) (i :fall))
      ;; ba.sAndH(trigger, in) — sample and hold on rising edge of trigger
      :sample-hold (format "(ba.sAndH(%s, %s))"
                           (i :trigger) (i :in))
      ;; Threshold comparator: 1.0 when in > threshold, 0.0 otherwise
      :comparator  (format "(float(%s > %s))"
                           (i :in) (i :threshold))
      ;; no.noise — white noise uniform [−1, 1]; no inputs
      :noise       "no.noise"
      ;; no.pink_noise — 1/f pink noise; no inputs
      :pink-noise  "no.pink_noise"
      ;; ba.crossfade(pos, a, b) — linear blend; pos=0 → full a, pos=1 → full b
      :crossfade   (format "(ba.crossfade(%s, %s, %s))"
                           (i :pos) (i :a) (i :b))
      ;; Ring mod / AM: carrier × (modulator + dc); dc=0 → ring mod, dc=1 → AM
      :ring-mod    (format "(%s * (%s + %s))"
                           (i :carrier) (i :modulator) (i :dc))
      ;; Bit crusher: quantise to 2^bits levels via floor rounding
      :bitcrusher  (format "(floor(%s * pow(2.0, %s - 1.0) + 0.5) / pow(2.0, %s - 1.0))"
                           (i :in) (i :bits) (i :bits))
      ;; Padé tanh approximation: x/(1+|x|) — soft saturation, no hard limit
      :soft-clip   (format "(%s / (1.0 + abs(%s)))"
                           (i :in) (i :in))
      ;; Cubic polynomial soft-then-hard overdrive: 1.5x − 0.5x³, clamped to [−1, 1]
      :hard-clip   (format "(max(-1.0, min(1.0, 1.5 * %s - 0.5 * %s * %s * %s)))"
                           (i :in) (i :in) (i :in) (i :in))
      ;; Triangle wavefolder: maps any input to [−1, 1] by triangle-folding at ±1
      ;; Uses positive-modulo formula: floor handles negatives correctly in Faust
      :wave-fold   (format "(1.0 - 2.0 * abs((%s + 1.0 - 4.0 * floor((%s + 1.0) / 4.0)) / 2.0 - 1.0))"
                           (i :in) (i :in))
      ;; ---- ops that use compile-time :opts ----
      ;; :vco — oscillator shape selected at compile time from {:shape kw}
      ;; Uses Faust oscillators.lib bandlimited waveforms.
      ;; PolyBLEP correction: lf_saw/lf_squarewave are naive; BLEPed variants TBD.
      :vco         (let [shape (get (:opts node) :shape :saw)
                         pw    (get (:opts node) :pw 0.5)
                         freq  (i :freq)]
                     (case shape
                       :sine     (format "os.osc(%s)" freq)
                       :saw      (format "os.lf_saw(%s)" freq)
                       :square   (format "os.lf_squarewave(%s)" freq)
                       :triangle (format "os.lf_triangle(%s)" freq)
                       :pulse    (format "select2(os.phasor(1.0, %s) < %s, -1.0, 1.0)"
                                         freq (fmt-num pw))
                       (throw (ex-info (str "Unknown :vco :shape — expected :sine :saw "
                                            ":square :triangle :pulse, got: " shape)
                                       {:shape shape}))))
      ;; :counter — clock-gated integer counter; dir and wrap from compile-time opts.
      ;; Edge detection: rising edge of :clock → increment/decrement.
      ;; :carry output requires multi-output support; emits count only for now.
      :counter     (let [{:keys [max dir wrap] :or {max 16 dir :up wrap true}} (:opts node)
                         clk  (i :clock)
                         rst  (i :reset)
                         edge (str "(" clk " > 0.5) & (" clk "' <= 0.5)")]
                     (case dir
                       :up   (if wrap
                               (format "(select2(%s > 0.5, select2(%s, _, fmod(_ + 1.0, %d.0)), 0.0) ~ _)"
                                       rst edge max)
                               (format "(select2(%s > 0.5, select2(%s, _, min(_ + 1.0, %d.0)), 0.0) ~ _)"
                                       rst edge (dec max)))
                       :down (if wrap
                               (format "(select2(%s > 0.5, select2(%s, _, fmod(_ - 1.0 + %d.0, %d.0)), %d.0) ~ _)"
                                       rst edge max max (dec max))
                               (format "(select2(%s > 0.5, select2(%s, _, max(_ - 1.0, 0.0)), %d.0) ~ _)"
                                       rst edge (dec max)))
                       (throw (ex-info (str ":counter :dir :up-down requires multi-output support "
                                            "(not yet implemented)")
                                       {:dir dir}))))
      ;; :table — read-only lookup via Faust rdtable + waveform.
      ;; Index modes: :wrap (positive modulo), :clamp, :fold (triangle reflection).
      :table       (let [{:keys [data size mode] :or {mode :wrap}} (:opts node)
                         _ (when (nil? data)
                             (throw (ex-info ":table requires :data in opts (file refs not yet supported)"
                                             {:opts (:opts node)})))
                         n        (or size (count data))
                         data-str (str/join ", " (map fmt-num data))
                         idx      (i :index)
                         idx-expr (case mode
                                    :wrap  (format "int(%s - %d.0 * floor(%s / %d.0))"
                                                   idx n idx n)
                                    :clamp (format "int(max(0.0, min(%d.0, %s)))"
                                                   (dec n) idx)
                                    :fold  (let [m (dec n)]
                                             (format "int(%d.0 - abs(%d.0 - (%s - %d.0 * floor(%s / %d.0))))"
                                                     m m idx (* 2 m) idx (* 2 m)))
                                    (throw (ex-info (str "Unknown :table :mode: " mode
                                                         " — expected :wrap :clamp :fold")
                                                    {:mode mode})))]
                     (format "rdtable(%d, waveform{%s}, %s)" n data-str idx-expr))
      ;; ba.selectn(n, index, in0, in1, …) — N-to-1 mux; index is int
      :select      (let [n    (get (:opts node) :n 2)
                         sigs (str/join ", " (map #(i (keyword (str "in-" %))) (range n)))]
                     (format "ba.selectn(%d, int(%s), %s)" n (i :index) sigs))
      ;; Secondary port of :counter — pulses 1.0 at the counter's wrap point
      ;; Fires when count == wrap-target on a rising clock edge.
      ;; Non-wrapping counters have no carry; emits 0.0.
      :counter-carry
      (let [{:keys [max dir wrap] :or {max 16 dir :up wrap true}} (:opts node)
            src  (i :source)
            clk  (i :clock)
            edge (str "(" clk " > 0.5) & (" clk "' <= 0.5)")]
        (if-not wrap
          "0.0"
          (let [wrap-target (case dir :up "0.0" :down (str (dec max) ".0"))]
            (format "float((%s == %s) & %s)" src wrap-target edge))))
      ;; Secondary port of :comparator — logical inverse of the primary gate
      :comparator-inv (format "(1.0 - %s)" (i :source))
      :faust       (:source node)
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
