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
    (or (get @env expr)
        (throw (ex-info (str "Unbound symbol in patch body: " expr) {:sym expr})))

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

        (= op 'output)
        (throw (ex-info "output must not be nested — use at top level of the patch body" {}))

        :else
        (let [op-kw  (keyword (name op))
              inlets (or (get ops/inlet-names op-kw)
                         (throw (ex-info (str "Unknown op: " op
                                              " — add an entry to alembic.ops/inlet-names")
                                         {:op op-kw})))
              _      (when (not= (count inlets) (count args))
                       (throw (ex-info (str op " expects " (count inlets)
                                            " arg(s), got " (count args))
                                       {:op op :expected (count inlets) :got (count args)})))
              srcs   (mapv #(walk-expr % state) args)
              id     (next-id! counter)
              rate   (or (get ops/node-rate op-kw)
                         (throw (ex-info (str "No rate entry for op: " op-kw) {:op op-kw})))
              inputs (zipmap inlets srcs)
              node   {:id id :op op-kw :rate rate :inputs inputs}]
          (swap! nodes assoc id node)
          (doseq [[inlet src-id] (map vector inlets srcs)]
            (let [src-rate  (:rate (get @nodes src-id))
                  feedback? (= op-kw :history)
                  crossing? (not= src-rate rate)]
              (swap! edges conj
                     (cond-> {:from src-id :to id :inlet inlet}
                       crossing? (assoc :rate-crossing? true)
                       feedback? (assoc :feedback? true)))))
          id)))

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
  ar-env. Use (param kw) for block-rate plugin parameters.

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
