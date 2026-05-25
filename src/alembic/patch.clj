; SPDX-License-Identifier: EPL-2.0
(ns alembic.patch
  (:require [alembic.ops :as ops]))

;; ---------------------------------------------------------------------------
;; Named output semantics
;;
;; (output :cv expr)    → channel 0   modulator_output.cv
;; (output :aux expr)   → channel 1   modulator_output.aux
;; (output :gate expr)  → channel 2   modulator_output.gate  (float > 0.5 → true)
;; (output :gate2 expr) → channel 3   modulator_output.gate2
;; (output :out0 expr)  → channel 4
;; (output :out1 expr)  → channel 5   (etc.)
;;
;; Unnamed (output expr) assigns channels sequentially from 0 as before.
;; The reserved param name "beat" is auto-populated at block rate by the
;; faust_modulator runtime with the fractional beat phase ∈ [0, 1).
;; ---------------------------------------------------------------------------

(def ^:private semantic-channels
  {:cv 0 :aux 1 :gate 2 :gate2 3})

(defn- semantic->channel [k]
  (or (get semantic-channels k)
      (when-let [[_ n] (re-matches #"out(\d+)" (name k))]
        (+ 4 (Long/parseLong n)))
      (throw (ex-info (str "Unknown output semantic: " k
                           " — use :cv :aux :gate :gate2 :out0 :out1 ...")
                      {:semantic k}))))

(defn- next-id! [counter]
  (keyword (str "n" (let [n @counter]
                      (swap! counter inc)
                      n))))

(declare walk-expr)

(defn- walk-expr
  "Walk a patch expression at macro-expansion time. Returns node-id.
  All side effects (node/edge creation) go into the state atoms."
  [expr {:keys [counter nodes edges env params] :as state}]
  (cond
    (number? expr)
    (let [id   (next-id! counter)
          node {:id id :op :const :value expr :rate :sample}]
      (swap! nodes assoc id node)
      id)

    (symbol? expr)
    (let [v (or (get @env expr)
                (throw (ex-info (str "Unbound symbol in patch body: " expr) {:sym expr})))]
      (when (map? v)
        (throw (ex-info (str expr " is a multi-output node — use (:port-name " expr ") to select a port")
                        {:sym expr :ports (keys v)})))
      v)

    (seq? expr)
    (let [[op & args] expr]
      (cond
        (= op 'param)
        (let [pname (first args)]
          (if-let [hit (get @params pname)]
            hit
            (let [id   (next-id! counter)
                  node {:id id :op :param :name pname :rate :block}]
              (swap! nodes assoc id node)
              (swap! params assoc pname id)
              id)))

        (= op 'faust)
        (let [src-str  (first args)
              inlet-map (when (and (> (count args) 1) (map? (second args)))
                          (second args))
              _        (when-not (string? src-str)
                         (throw (ex-info "faust source must be a string literal" {:got src-str})))
              id       (next-id! counter)
              inlets   (into {} (map (fn [[k v]] [k (walk-expr v state)]) inlet-map))
              node     {:id id :op :faust :rate :sample :source src-str :inputs inlets}]
          (swap! nodes assoc id node)
          (doseq [[inlet src-id] inlets]
            (let [src-rate  (:rate (get @nodes src-id))
                  crossing? (not= src-rate :sample)]
              (swap! edges conj
                     (cond-> {:from src-id :to id :inlet inlet}
                       crossing? (assoc :rate-crossing? true)))))
          id)

        (= op 'output)
        (throw (ex-info "output must not be nested — use at top level of the patch body" {}))

        (keyword? op)
        (let [_ (when (not= 1 (count args))
                  (throw (ex-info "Port access form requires exactly one argument: (:port-name sym)" {})))
              sym      (first args)
              port-map (or (get @env sym)
                           (throw (ex-info (str "Unbound symbol: " sym) {:sym sym})))]
          (when-not (map? port-map)
            (throw (ex-info (str sym " is not a multi-output node") {:sym sym})))
          (or (get port-map op)
              (throw (ex-info (str "Unknown port " op " — available: " (keys port-map))
                              {:port op :available (keys port-map)}))))

        :else
        (let [op-kw     (keyword (name op))
              ;; Optional compile-time options map as first argument:
              ;;   (vco {:shape :saw} freq-signal)
              has-opts? (and (seq args) (map? (first args)))
              opts      (if has-opts? (first args) {})
              sig-args  (if has-opts? (rest args) args)
              inlets-spec (or (get ops/inlet-names op-kw)
                              (throw (ex-info (str "Unknown op: " op
                                                   " — add an entry to alembic.ops/inlet-names")
                                              {:op op-kw})))
              inlets    (if (fn? inlets-spec) (inlets-spec opts) inlets-spec)
              _         (when (not= (count inlets) (count sig-args))
                          (throw (ex-info (str op " expects " (count inlets)
                                              " arg(s), got " (count sig-args))
                                          {:op op :expected (count inlets) :got (count sig-args)})))
              srcs      (mapv #(walk-expr % state) sig-args)
              id        (next-id! counter)
              rate      (or (get ops/node-rate op-kw)
                            (throw (ex-info (str "No rate entry for op: " op-kw) {:op op-kw})))
              inputs    (zipmap inlets srcs)
              node      (cond-> {:id id :op op-kw :rate rate :inputs inputs}
                          (seq opts) (assoc :opts opts))]
          (swap! nodes assoc id node)
          (doseq [[inlet src-id] (map vector inlets srcs)]
            (let [src-rate  (:rate (get @nodes src-id))
                  feedback? (= op-kw :history)
                  crossing? (not= src-rate rate)]
              (swap! edges conj
                     (cond-> {:from src-id :to id :inlet inlet}
                       crossing? (assoc :rate-crossing? true)
                       feedback? (assoc :feedback? true)))))
          ;; Create secondary port nodes for multi-output ops
          (let [secondary (get ops/port-node-specs op-kw {})]
            (if (empty? secondary)
              id
              (into {:out id}
                    (map (fn [[port-name spec]]
                           (let [port-id     (next-id! counter)
                                 port-inputs (reduce (fn [acc inlet]
                                                       (assoc acc inlet
                                                              (if (= inlet :source)
                                                                id
                                                                (get inputs inlet))))
                                                     {}
                                                     (:inlets spec))
                                 port-node   (cond-> {:id   port-id
                                                      :op   (:op spec)
                                                      :rate rate
                                                      :inputs port-inputs}
                                               (seq opts) (assoc :opts opts))]
                             (swap! nodes assoc port-id port-node)
                             (doseq [[inlet src-id] port-inputs]
                               (swap! edges conj {:from src-id :to port-id :inlet inlet}))
                             [port-name port-id]))
                         secondary)))))))

    :else
    (throw (ex-info (str "Cannot compile patch expression: " (pr-str expr)) {:expr expr}))))

(defn- process-bindings! [bindings state]
  (doseq [[sym expr] (partition 2 bindings)]
    (swap! (:env state) assoc sym (walk-expr expr state))))

(defn- process-body! [body-forms state outputs channel-counter]
  (doseq [form body-forms]
    (if (and (seq? form) (= (first form) 'output))
      (let [named?  (keyword? (second form))
            sem     (when named? (second form))
            expr    (if named? (nth form 2) (second form))
            src-id  (walk-expr expr state)
            ch      (if named?
                      (semantic->channel sem)
                      (let [n @channel-counter]
                        (swap! channel-counter inc)
                        n))]
        (swap! outputs conj (cond-> {:node src-id :channel ch
                                     :name (if named? (name sem) "Main")}
                              named? (assoc :semantic sem))))
      (walk-expr form state))))

(defn- dominant-rate [nodes]
  (let [rate-rank {:beat 0 :block 1 :sample 2}
        rates     (map :rate (vals nodes))]
    (if (seq rates)
      (->> rates (sort-by rate-rank) last)
      :sample)))

(defmacro defpatch!
  "Define a named patch as a normalised signal graph value.

  patch-name  — symbol (or keyword) naming the patch; a var with this name is def'd
  opts        — literal map; :params key carries the param schema
  let-form    — (let [bindings...] body-forms...) authoring syntax

  Op names in the authoring form: phasor, sine-bi, sine-uni, tri, rect,
  mul, add, sub, div, history, delay, sah, delta, wrap, fold, clip, smooth,
  ar-env, ladder, svf, one-pole, dc-block, allpass, vca, slew, sample-hold,
  comparator, noise, pink-noise, crossfade, ring-mod, bitcrusher, soft-clip,
  hard-clip, wave-fold, naive-svf, crossover, hysteresis, damping, segment.
  Use (param kw) for block-rate plugin parameters.

  Multi-output ops return a port-map; select outputs with (:port-name sym):
    (naive-svf in cutoff res) → {:out lp-node :hp hp-node}
    (crossover in cutoff)     → {:out lp-node :hp hp-node}

  Ops that accept compile-time options as a map before signal arguments:
    (vco {:shape :saw|:sine|:square|:triangle|:pulse  :pw 0.5} freq)
    (counter {:max 16 :dir :up|:down :wrap true|false} clock reset)
    (table {:data [floats] :size N :mode :wrap|:clamp|:fold} index)

  Inline Faust expressions with named wired inlets:
    (faust \"expr with %inlet-names\" {:inlet-name expr ...})
    e.g. (faust \"os.osc(%freq)\" {:freq freq-signal})
    %inlet-name placeholders are replaced at emit time with the Faust
    identifier of the wired source node. Omit the inlet map for a
    self-contained Faust expression with no wired inlets.

  Output forms:
    (output expr)         — unnamed, assigned channel 0, 1, 2 … in declaration order
    (output :cv   expr)   — channel 0 → modulator_output.cv
    (output :aux  expr)   — channel 1 → modulator_output.aux
    (output :gate expr)   — channel 2 → modulator_output.gate (float > 0.5)
    (output :gate2 expr)  — channel 3 → modulator_output.gate2
    (output :out0 expr)   — channel 4 (extra outputs)
    (output :outN expr)   — channel 4+N

  When compiled as a faust_modulator (375 Hz), the reserved param name
  \"beat\" is auto-populated with the fractional beat phase ∈ [0, 1).

  opts must be a literal map. To construct a graph at runtime, build the
  normalised map directly — it is a plain Clojure value."
  [patch-name opts let-form]
  (let [name-sym      (if (keyword? patch-name) (symbol (name patch-name)) patch-name)
        params-schema (get opts :params {})

        counter  (atom 0)
        nodes    (atom {})
        edges    (atom [])
        env      (atom {})
        params   (atom {})
        outputs  (atom [])
        chan-ctr (atom 0)
        state    {:counter counter :nodes nodes :edges edges :env env :params params}]

    (let [[let-sym bindings & body-forms] let-form]
      (assert (= let-sym 'let)
              (str "defpatch! body must be a (let [...] ...) form, got: " let-sym))
      (assert (vector? bindings)
              "defpatch! let bindings must be a vector")
      (assert (even? (count bindings))
              "defpatch! let bindings must have an even number of forms")
      (process-bindings! bindings state)
      (process-body! body-forms state outputs chan-ctr))

    (let [graph {:nodes   @nodes
                 :edges   @edges
                 :params  params-schema
                 :outputs @outputs
                 :rate    (dominant-rate @nodes)}]
      `(def ~name-sym ~graph))))
