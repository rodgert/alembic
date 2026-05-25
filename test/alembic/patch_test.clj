; SPDX-License-Identifier: EPL-2.0
(ns alembic.patch-test
  (:require [clojure.test :refer [deftest is testing]]
            [alembic.patch :refer [defpatch!]]))

;; ---------------------------------------------------------------------------
;; Helper queries
;; ---------------------------------------------------------------------------

(defn- nodes-by-op [graph op]
  (filter #(= op (:op %)) (vals (:nodes graph))))

(defn- edges-where [graph pred]
  (filter pred (:edges graph)))

;; ---------------------------------------------------------------------------
;; Literal promotion
;; ---------------------------------------------------------------------------

(defpatch! literal-test {}
  (let [osc (phasor 440.0)]
    (output osc)))

(deftest literal-promotion-test
  (testing "numeric literal becomes :const node"
    (is (= 1 (count (nodes-by-op literal-test :const)))))
  (testing ":const node carries the literal value"
    (is (= 440.0 (:value (first (nodes-by-op literal-test :const))))))
  (testing ":const node is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op literal-test :const)))))))

;; ---------------------------------------------------------------------------
;; Param deduplication
;; ---------------------------------------------------------------------------

(defpatch! param-dedup-p {}
  (let [x (add (param :freq) (param :freq))]
    (output x)))

(deftest param-dedup-test
  (testing "same param referenced twice → single :param node"
    (is (= 1 (count (nodes-by-op param-dedup-p :param)))))
  (testing "both inputs of :add reference the same node id"
    (let [plus (first (nodes-by-op param-dedup-p :add))]
      (is (= (get-in plus [:inputs :a])
             (get-in plus [:inputs :b]))))))

;; ---------------------------------------------------------------------------
;; Rate crossing
;; ---------------------------------------------------------------------------

(defpatch! rate-crossing-p {}
  (let [osc (phasor (param :freq))]
    (output osc)))

(deftest rate-crossing-test
  (testing "one crossing edge from :param to :phasor"
    (is (= 1 (count (edges-where rate-crossing-p :rate-crossing?)))))
  (testing "the crossing edge connects :block source to :sample destination"
    (let [edge  (first (edges-where rate-crossing-p :rate-crossing?))
          nodes (:nodes rate-crossing-p)]
      (is (= :param  (:op (get nodes (:from edge)))))
      (is (= :phasor (:op (get nodes (:to edge))))))))

;; ---------------------------------------------------------------------------
;; history feedback marking
;; ---------------------------------------------------------------------------

(defpatch! feedback-p {}
  (let [h (history (sine-bi (phasor 1.0)))]
    (output h)))

(deftest feedback-test
  (testing "exactly one :feedback? edge"
    (is (= 1 (count (edges-where feedback-p :feedback?)))))
  (testing "the feedback edge points into the :history node"
    (let [edge    (first (edges-where feedback-p :feedback?))
          hist    (first (nodes-by-op feedback-p :history))]
      (is (some? hist))
      (is (= (:id hist) (:to edge))))))

;; ---------------------------------------------------------------------------
;; :delay — opts-aware delay line
;; ---------------------------------------------------------------------------

(defpatch! delay-default-test {}
  (let [sig (sine-bi (phasor 440.0))
        out (delay sig 0.1)]
    (output out)))

(defpatch! delay-smooth-false-test {}
  (let [sig (sine-bi (phasor 440.0))
        out (delay {:max-time 0.5 :smooth false :interp :linear} sig 0.1)]
    (output out)))

(defpatch! delay-time-cv-test {}
  (let [sig (sine-bi (phasor 440.0))
        cv  (mul (phasor 1.0) 0.02)
        out (delay {:max-time 0.05 :time-cv true} sig cv)]
    (output out)))

(deftest delay-node-default-test
  (testing "creates exactly one :delay node"
    (is (= 1 (count (nodes-by-op delay-default-test :delay)))))
  (let [node (first (nodes-by-op delay-default-test :delay))]
    (testing "default inlets are :in and :time"
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :time))
      (is (not (contains? (:inputs node) :time-cv))))
    (testing ":delay is :sample rate"
      (is (= :sample (:rate node))))))

(deftest delay-node-opts-stored-test
  (let [node (first (nodes-by-op delay-smooth-false-test :delay))]
    (testing "opts stored on node"
      (is (false?  (get-in node [:opts :smooth])))
      (is (= 0.5   (get-in node [:opts :max-time])))
      (is (= :linear (get-in node [:opts :interp]))))))

(deftest delay-time-cv-inlets-test
  (let [node (first (nodes-by-op delay-time-cv-test :delay))]
    (testing "time-cv true → :in and :time-cv inlets (no :time)"
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :time-cv))
      (is (not (contains? (:inputs node) :time))))))

;; ---------------------------------------------------------------------------
;; output descriptor
;; ---------------------------------------------------------------------------

(defpatch! output-desc-test {}
  (let [x (phasor 1.0)]
    (output x)))

(deftest output-descriptor-test
  (testing "single :outputs entry"
    (is (= 1 (count (:outputs output-desc-test)))))
  (testing "channel 0, name Main"
    (let [out (first (:outputs output-desc-test))]
      (is (= 0      (:channel out)))
      (is (= "Main" (:name out)))))
  (testing "output :node is a valid node id in :nodes"
    (let [out (first (:outputs output-desc-test))]
      (is (contains? (:nodes output-desc-test) (:node out))))))

;; ---------------------------------------------------------------------------
;; Multi-channel outputs
;; ---------------------------------------------------------------------------

(defpatch! stereo-test {}
  (let [left  (phasor 440.0)
        right (phasor 441.0)]
    (output left)
    (output right)))

(deftest multi-channel-output-test
  (testing "two output calls → two entries in :outputs"
    (is (= 2 (count (:outputs stereo-test)))))
  (testing "channels are 0 and 1"
    (is (= #{0 1} (set (map :channel (:outputs stereo-test)))))))

;; ---------------------------------------------------------------------------
;; Level 1 filters — :ladder
;; ---------------------------------------------------------------------------

(defpatch! ladder-test {}
  (let [osc (phasor 220.0)
        sig (sine-bi osc)
        out (ladder sig 0.3 0.5)]
    (output out)))

(deftest ladder-node-test
  (testing "has exactly one :ladder node"
    (is (= 1 (count (nodes-by-op ladder-test :ladder)))))
  (testing ":ladder node has :input :cutoff :resonance inlets"
    (let [node (first (nodes-by-op ladder-test :ladder))]
      (is (contains? (:inputs node) :input))
      (is (contains? (:inputs node) :cutoff))
      (is (contains? (:inputs node) :resonance))))
  (testing ":ladder node is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op ladder-test :ladder))))))
  (testing "patch rate is :sample"
    (is (= :sample (:rate ladder-test)))))

(defpatch! svf-test {}
  (let [osc (phasor 220.0)
        sig (sine-bi osc)
        out (svf sig 0.3 0.5 0.0)]
    (output out)))

(deftest svf-node-test
  (testing "has exactly one :svf node"
    (is (= 1 (count (nodes-by-op svf-test :svf)))))
  (testing ":svf node has all four inlets"
    (let [node (first (nodes-by-op svf-test :svf))]
      (is (contains? (:inputs node) :input))
      (is (contains? (:inputs node) :cutoff))
      (is (contains? (:inputs node) :resonance))
      (is (contains? (:inputs node) :mode))))
  (testing ":svf node is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op svf-test :svf)))))))

;; ---------------------------------------------------------------------------
;; Named outputs
;; ---------------------------------------------------------------------------

(defpatch! named-cv-test {}
  (let [x (phasor 1.0)]
    (output :cv x)))

(deftest named-output-cv-test
  (testing ":cv output gets channel 0 with :semantic :cv"
    (let [out (first (:outputs named-cv-test))]
      (is (= 0   (:channel out)))
      (is (= :cv (:semantic out)))
      (is (= "cv" (:name out))))))

(defpatch! named-gate-test {}
  (let [x (phasor 1.0)]
    (output :gate x)))

(deftest named-output-gate-test
  (testing ":gate output gets channel 2"
    (let [out (first (:outputs named-gate-test))]
      (is (= 2     (:channel out)))
      (is (= :gate (:semantic out))))))

(defpatch! named-out0-test {}
  (let [x (phasor 1.0)]
    (output :out0 x)))

(deftest named-output-out0-test
  (testing ":out0 output gets channel 4"
    (is (= 4 (:channel (first (:outputs named-out0-test)))))))

(defpatch! named-multi-test {}
  (let [cv-sig   (phasor 1.0)
        gate-sig (sine-bi cv-sig)]
    (output :cv   cv-sig)
    (output :gate gate-sig)))

(deftest named-multi-output-test
  (testing "cv on channel 0, gate on channel 2, gap at channel 1 preserved in graph"
    (let [by-sem (into {} (map (fn [o] [(:semantic o) (:channel o)])
                               (:outputs named-multi-test)))]
      (is (= 0 (by-sem :cv)))
      (is (= 2 (by-sem :gate)))))
  (testing "two output entries recorded"
    (is (= 2 (count (:outputs named-multi-test))))))

(defpatch! mixed-named-unnamed-test {}
  (let [x (phasor 1.0)
        y (phasor 2.0)]
    (output :cv x)
    (output y)))

(deftest unnamed-beside-named-test
  (testing "named output carries :semantic, unnamed does not"
    (is (some #(= :cv (:semantic %)) (:outputs mixed-named-unnamed-test)))
    (is (some #(nil? (:semantic %))  (:outputs mixed-named-unnamed-test)))))

;; ---------------------------------------------------------------------------
;; Level 1 extended — filters, signal ops, waveshapers
;; ---------------------------------------------------------------------------

(defpatch! one-pole-test {}
  (let [osc (phasor 440.0)
        out (one-pole osc 0.5)]
    (output out)))

(deftest one-pole-node-test
  (testing "has exactly one :one-pole node"
    (is (= 1 (count (nodes-by-op one-pole-test :one-pole)))))
  (testing ":one-pole has :in and :cutoff inlets"
    (let [node (first (nodes-by-op one-pole-test :one-pole))]
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :cutoff))))
  (testing ":one-pole is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op one-pole-test :one-pole)))))))

(defpatch! dc-block-test {}
  (let [osc (phasor 1.0)
        out (dc-block osc)]
    (output out)))

(deftest dc-block-node-test
  (testing "has exactly one :dc-block node"
    (is (= 1 (count (nodes-by-op dc-block-test :dc-block)))))
  (testing ":dc-block has :in inlet"
    (let [node (first (nodes-by-op dc-block-test :dc-block))]
      (is (contains? (:inputs node) :in))))
  (testing ":dc-block is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op dc-block-test :dc-block)))))))

(defpatch! allpass-test {}
  (let [osc (phasor 1.0)
        out (allpass osc 0.02 0.5)]
    (output out)))

(deftest allpass-node-test
  (testing "has exactly one :allpass node"
    (is (= 1 (count (nodes-by-op allpass-test :allpass)))))
  (testing ":allpass has :in :time :coeff inlets"
    (let [node (first (nodes-by-op allpass-test :allpass))]
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :time))
      (is (contains? (:inputs node) :coeff))))
  (testing ":allpass is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op allpass-test :allpass)))))))

(defpatch! vca-test {}
  (let [osc (phasor 440.0)
        out (vca osc 0.5)]
    (output out)))

(deftest vca-node-test
  (testing "has exactly one :vca node"
    (is (= 1 (count (nodes-by-op vca-test :vca)))))
  (testing ":vca has :in and :level inlets"
    (let [node (first (nodes-by-op vca-test :vca))]
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :level))))
  (testing ":vca is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op vca-test :vca)))))))

(defpatch! slew-test {}
  (let [osc (phasor 1.0)
        out (slew osc 0.01 0.03)]
    (output out)))

(deftest slew-node-test
  (testing "has exactly one :slew node"
    (is (= 1 (count (nodes-by-op slew-test :slew)))))
  (testing ":slew has :in :rise :fall inlets"
    (let [node (first (nodes-by-op slew-test :slew))]
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :rise))
      (is (contains? (:inputs node) :fall))))
  (testing ":slew is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op slew-test :slew)))))))

(defpatch! sample-hold-test {}
  (let [osc  (phasor 440.0)
        trig (phasor 1.0)
        out  (sample-hold osc trig)]
    (output out)))

(deftest sample-hold-node-test
  (testing "has exactly one :sample-hold node"
    (is (= 1 (count (nodes-by-op sample-hold-test :sample-hold)))))
  (testing ":sample-hold has :in and :trigger inlets"
    (let [node (first (nodes-by-op sample-hold-test :sample-hold))]
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :trigger))))
  (testing ":sample-hold is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op sample-hold-test :sample-hold)))))))

(defpatch! comparator-test {}
  (let [osc (phasor 440.0)
        cmp (comparator osc 0.0)
        out (:out cmp)]
    (output out)))

(deftest comparator-node-test
  (testing "has exactly one :comparator node"
    (is (= 1 (count (nodes-by-op comparator-test :comparator)))))
  (testing ":comparator has :in and :threshold inlets"
    (let [node (first (nodes-by-op comparator-test :comparator))]
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :threshold))))
  (testing ":comparator is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op comparator-test :comparator)))))))

(defpatch! noise-test {}
  (let [n (noise)]
    (output n)))

(deftest noise-node-test
  (testing "has exactly one :noise node"
    (is (= 1 (count (nodes-by-op noise-test :noise)))))
  (testing ":noise node has no inputs"
    (let [node (first (nodes-by-op noise-test :noise))]
      (is (= {} (:inputs node)))))
  (testing ":noise is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op noise-test :noise)))))))

(defpatch! pink-noise-test {}
  (let [n (pink-noise)]
    (output n)))

(deftest pink-noise-node-test
  (testing "has exactly one :pink-noise node"
    (is (= 1 (count (nodes-by-op pink-noise-test :pink-noise)))))
  (testing ":pink-noise node has no inputs"
    (let [node (first (nodes-by-op pink-noise-test :pink-noise))]
      (is (= {} (:inputs node)))))
  (testing ":pink-noise is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op pink-noise-test :pink-noise)))))))

(defpatch! crossfade-test {}
  (let [a   (phasor 440.0)
        b   (phasor 441.0)
        out (crossfade a b 0.5)]
    (output out)))

(deftest crossfade-node-test
  (testing "has exactly one :crossfade node"
    (is (= 1 (count (nodes-by-op crossfade-test :crossfade)))))
  (testing ":crossfade has :a :b :pos inlets"
    (let [node (first (nodes-by-op crossfade-test :crossfade))]
      (is (contains? (:inputs node) :a))
      (is (contains? (:inputs node) :b))
      (is (contains? (:inputs node) :pos))))
  (testing ":crossfade is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op crossfade-test :crossfade)))))))

(defpatch! ring-mod-test {}
  (let [c   (phasor 440.0)
        m   (phasor 110.0)
        out (ring-mod c m 0.0)]
    (output out)))

(deftest ring-mod-node-test
  (testing "has exactly one :ring-mod node"
    (is (= 1 (count (nodes-by-op ring-mod-test :ring-mod)))))
  (testing ":ring-mod has :carrier :modulator :dc inlets"
    (let [node (first (nodes-by-op ring-mod-test :ring-mod))]
      (is (contains? (:inputs node) :carrier))
      (is (contains? (:inputs node) :modulator))
      (is (contains? (:inputs node) :dc))))
  (testing ":ring-mod is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op ring-mod-test :ring-mod)))))))

(defpatch! bitcrusher-test {}
  (let [osc (phasor 440.0)
        out (bitcrusher osc 12.0)]
    (output out)))

(deftest bitcrusher-node-test
  (testing "has exactly one :bitcrusher node"
    (is (= 1 (count (nodes-by-op bitcrusher-test :bitcrusher)))))
  (testing ":bitcrusher has :in and :bits inlets"
    (let [node (first (nodes-by-op bitcrusher-test :bitcrusher))]
      (is (contains? (:inputs node) :in))
      (is (contains? (:inputs node) :bits))))
  (testing ":bitcrusher is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op bitcrusher-test :bitcrusher)))))))

(defpatch! soft-clip-test {}
  (let [osc (phasor 440.0)
        out (soft-clip osc)]
    (output out)))

(deftest soft-clip-node-test
  (testing "has exactly one :soft-clip node"
    (is (= 1 (count (nodes-by-op soft-clip-test :soft-clip)))))
  (testing ":soft-clip has :in inlet"
    (is (contains? (:inputs (first (nodes-by-op soft-clip-test :soft-clip))) :in)))
  (testing ":soft-clip is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op soft-clip-test :soft-clip)))))))

(defpatch! hard-clip-test {}
  (let [osc (phasor 440.0)
        out (hard-clip osc)]
    (output out)))

(deftest hard-clip-node-test
  (testing "has exactly one :hard-clip node"
    (is (= 1 (count (nodes-by-op hard-clip-test :hard-clip)))))
  (testing ":hard-clip has :in inlet"
    (is (contains? (:inputs (first (nodes-by-op hard-clip-test :hard-clip))) :in)))
  (testing ":hard-clip is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op hard-clip-test :hard-clip)))))))

(defpatch! wave-fold-test {}
  (let [osc (phasor 440.0)
        out (wave-fold osc)]
    (output out)))

(deftest wave-fold-node-test
  (testing "has exactly one :wave-fold node"
    (is (= 1 (count (nodes-by-op wave-fold-test :wave-fold)))))
  (testing ":wave-fold has :in inlet"
    (is (contains? (:inputs (first (nodes-by-op wave-fold-test :wave-fold))) :in)))
  (testing ":wave-fold is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op wave-fold-test :wave-fold)))))))

;; ---------------------------------------------------------------------------
;; Compile-time options — :vco :counter :table
;; ---------------------------------------------------------------------------

(defpatch! vco-saw-test {}
  (let [osc (vco {:shape :saw} 440.0)]
    (output osc)))

(defpatch! vco-sine-test {}
  (let [osc (vco {:shape :sine} 440.0)]
    (output osc)))

(defpatch! vco-pulse-test {}
  (let [osc (vco {:shape :pulse :pw 0.3} 440.0)]
    (output osc)))

(deftest vco-opts-stored-test
  (testing ":saw shape stored in :opts"
    (let [node (first (nodes-by-op vco-saw-test :vco))]
      (is (= :saw (get-in node [:opts :shape])))))
  (testing ":sine shape stored in :opts"
    (let [node (first (nodes-by-op vco-sine-test :vco))]
      (is (= :sine (get-in node [:opts :shape])))))
  (testing ":pulse shape and :pw stored in :opts"
    (let [node (first (nodes-by-op vco-pulse-test :vco))]
      (is (= :pulse (get-in node [:opts :shape])))
      (is (= 0.3    (get-in node [:opts :pw]))))))

(deftest vco-inlets-test
  (testing ":vco has :freq signal inlet"
    (doseq [patch [vco-saw-test vco-sine-test vco-pulse-test]]
      (let [node (first (nodes-by-op patch :vco))]
        (is (contains? (:inputs node) :freq)))))
  (testing ":vco is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op vco-saw-test :vco)))))))

(deftest vco-without-opts-defaults-test
  (testing "opts are omitted from node when not supplied to other ops"
    ;; phasor is a plain op — its node should have no :opts key
    (let [node (first (nodes-by-op vco-saw-test :const))]
      (is (nil? (:opts node))))))

(defpatch! counter-up-test {}
  (let [clk (phasor 4.0)
        rst (phasor 0.5)
        ctr (counter {:max 8 :dir :up :wrap true} clk rst)
        out (:out ctr)]
    (output out)))

(defpatch! counter-down-test {}
  (let [clk (phasor 4.0)
        rst (phasor 0.5)
        ctr (counter {:max 16 :dir :down :wrap false} clk rst)
        out (:out ctr)]
    (output out)))

(deftest counter-opts-stored-test
  (testing ":up counter opts stored correctly"
    (let [node (first (nodes-by-op counter-up-test :counter))]
      (is (= 8    (get-in node [:opts :max])))
      (is (= :up  (get-in node [:opts :dir])))
      (is (true?  (get-in node [:opts :wrap])))))
  (testing ":down counter opts stored correctly"
    (let [node (first (nodes-by-op counter-down-test :counter))]
      (is (= 16    (get-in node [:opts :max])))
      (is (= :down (get-in node [:opts :dir])))
      (is (false?  (get-in node [:opts :wrap]))))))

(deftest counter-inlets-test
  (testing ":counter has :clock and :reset signal inlets"
    (let [node (first (nodes-by-op counter-up-test :counter))]
      (is (contains? (:inputs node) :clock))
      (is (contains? (:inputs node) :reset))))
  (testing ":counter is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op counter-up-test :counter)))))))

(def ^:private triangle-data [0.0 0.25 0.5 0.75 1.0 0.75 0.5 0.25])

(defpatch! table-wrap-test {}
  (let [ph  (phasor 440.0)
        idx (mul ph 7.0)
        out (table {:data triangle-data :size 8 :mode :wrap} idx)]
    (output out)))

(defpatch! table-clamp-test {}
  (let [ph  (phasor 440.0)
        idx (mul ph 7.0)
        out (table {:data triangle-data :size 8 :mode :clamp} idx)]
    (output out)))

(deftest table-opts-stored-test
  (testing ":table opts stored correctly"
    (let [node (first (nodes-by-op table-wrap-test :table))]
      (is (= 8     (get-in node [:opts :size])))
      (is (= :wrap (get-in node [:opts :mode])))
      (is (= triangle-data (get-in node [:opts :data])))))
  (testing ":clamp mode stored"
    (let [node (first (nodes-by-op table-clamp-test :table))]
      (is (= :clamp (get-in node [:opts :mode]))))))

(deftest table-inlets-test
  (testing ":table has :index signal inlet"
    (let [node (first (nodes-by-op table-wrap-test :table))]
      (is (contains? (:inputs node) :index))))
  (testing ":table is :sample rate"
    (is (= :sample (:rate (first (nodes-by-op table-wrap-test :table)))))))

;; ---------------------------------------------------------------------------
;; Dominant rate
;; ---------------------------------------------------------------------------

(defpatch! sample-rate-test {}
  (let [x (phasor 1.0)]
    (output x)))

(deftest dominant-rate-test
  (testing "patch with :sample nodes → :rate :sample"
    (is (= :sample (:rate sample-rate-test)))))

;; ---------------------------------------------------------------------------
;; Params schema passthrough
;; ---------------------------------------------------------------------------

(defpatch! params-schema-test
  {:params {:freq {:range [20.0 20000.0] :default 440.0 :unit :hz}}}
  (let [osc (phasor (param :freq))]
    (output osc)))

(deftest params-passthrough-test
  (testing "params schema is preserved verbatim"
    (is (= {:range [20.0 20000.0] :default 440.0 :unit :hz}
           (get-in params-schema-test [:params :freq])))))

;; ---------------------------------------------------------------------------
;; Structural smoke test — two-op FM patch
;; ---------------------------------------------------------------------------

(defpatch! fm-smoke-p
  {:params {:carrier   {:range [20.0 20000.0] :default 440.0 :unit :hz}
            :mod-ratio {:range [0.1 10.0]     :default 2.0}}}
  (let [mod-p  (phasor (mul (param :carrier) (param :mod-ratio)))
        sig    (sine-bi (add (phasor (param :carrier)) (sine-bi mod-p)))]
    (output sig)))

(deftest fm-smoke-test
  (testing "has nodes for all expected ops"
    (is (= 2 (count (nodes-by-op fm-smoke-p :param))))
    (is (= 2 (count (nodes-by-op fm-smoke-p :phasor))))
    (is (= 1 (count (nodes-by-op fm-smoke-p :mul))))
    (is (= 1 (count (nodes-by-op fm-smoke-p :add))))
    (is (= 2 (count (nodes-by-op fm-smoke-p :sine-bi)))))
  (testing "all rate-crossing edges connect :block to :sample"
    (doseq [edge (edges-where fm-smoke-p :rate-crossing?)]
      (let [nodes (:nodes fm-smoke-p)]
        (is (= :block  (:rate (get nodes (:from edge)))))
        (is (= :sample (:rate (get nodes (:to edge))))))))
  (testing "patch rate is :sample"
    (is (= :sample (:rate fm-smoke-p)))))

;; ---------------------------------------------------------------------------
;; Variable-arity inlets — :select
;; ---------------------------------------------------------------------------

(defpatch! select2-test {}
  (let [a  (phasor 110.0)
        b  (phasor 220.0)
        s  (select {:n 2} a b 0.0)]
    (output s)))

(deftest select-n2-patch-test
  (testing ":select node is created"
    (is (= 1 (count (nodes-by-op select2-test :select)))))
  (let [node (first (nodes-by-op select2-test :select))]
    (testing ":select node has :sample rate"
      (is (= :sample (:rate node))))
    (testing ":select opts store :n"
      (is (= 2 (get-in node [:opts :n]))))
    (testing ":select inputs has :in-0 :in-1 :index"
      (is (contains? (:inputs node) :in-0))
      (is (contains? (:inputs node) :in-1))
      (is (contains? (:inputs node) :index)))))

(defpatch! select4-test {}
  (let [a (phasor 55.0)
        b (phasor 110.0)
        c (phasor 220.0)
        d (phasor 440.0)
        s (select {:n 4} a b c d 0.0)]
    (output s)))

(deftest select-n4-patch-test
  (testing ":select n=4 node is created"
    (is (= 1 (count (nodes-by-op select4-test :select)))))
  (let [node (first (nodes-by-op select4-test :select))]
    (testing ":select n=4 has four signal inlets plus :index"
      (is (= #{:in-0 :in-1 :in-2 :in-3 :index} (set (keys (:inputs node)))))
      (is (= 4 (get-in node [:opts :n]))))))

;; ---------------------------------------------------------------------------
;; :buffer — read-write circular buffer
;; ---------------------------------------------------------------------------

(defpatch! buffer-test {}
  (let [ph    (phasor 1.0)
        sig   (sine-bi ph)
        wpos  (mul ph 4800.0)
        rpos  (sub wpos 240.0)
        out   (buffer {:size 4800} sig wpos rpos)]
    (output out)))

(deftest buffer-node-test
  (let [node (first (nodes-by-op buffer-test :buffer))]
    (testing "has exactly one :buffer node"
      (is (= 1 (count (nodes-by-op buffer-test :buffer)))))
    (testing ":buffer has :in :write-pos :read-pos inlets"
      (is (= #{:in :write-pos :read-pos} (set (keys (:inputs node))))))
    (testing ":buffer is :sample rate"
      (is (= :sample (:rate node))))
    (testing ":buffer stores :size in opts"
      (is (= 4800 (get-in node [:opts :size]))))))

;; ---------------------------------------------------------------------------
;; Multi-output nodes — :counter carry port
;; ---------------------------------------------------------------------------

(defpatch! counter-carry-patch {}
  (let [clk   (phasor 4.0)
        rst   (comparator clk 0.5)
        ctr   (counter {:max 8} clk (:out rst))
        cnt   (:out ctr)
        carry (:carry ctr)]
    (output cnt)
    (output carry)))

(deftest counter-multi-output-test
  (testing ":counter creates a primary :counter node"
    (is (= 1 (count (nodes-by-op counter-carry-patch :counter)))))
  (testing ":counter creates a secondary :counter-carry node"
    (is (= 1 (count (nodes-by-op counter-carry-patch :counter-carry)))))
  (let [ctr-node   (first (nodes-by-op counter-carry-patch :counter))
        carry-node (first (nodes-by-op counter-carry-patch :counter-carry))]
    (testing ":counter-carry :source points to the :counter node"
      (is (= (:id ctr-node) (get-in carry-node [:inputs :source]))))
    (testing ":counter-carry :clock points to the clock signal (same as :counter)"
      (is (= (get-in ctr-node [:inputs :clock])
             (get-in carry-node [:inputs :clock]))))
    (testing ":counter-carry inherits :opts from :counter"
      (is (= {:max 8} (:opts carry-node)))))
  (testing "both ports appear in patch outputs"
    (is (= 2 (count (:outputs counter-carry-patch))))))

;; ---------------------------------------------------------------------------
;; Multi-output nodes — :comparator inv-gate port
;; ---------------------------------------------------------------------------

(defpatch! comparator-inv-patch {}
  (let [sig  (phasor 1.0)
        cmp  (comparator sig 0.5)
        gate (:out cmp)
        inv  (:inv-gate cmp)]
    (output gate)
    (output inv)))

(deftest comparator-multi-output-test
  (testing ":comparator creates a primary :comparator node"
    (is (= 1 (count (nodes-by-op comparator-inv-patch :comparator)))))
  (testing ":comparator creates a secondary :comparator-inv node"
    (is (= 1 (count (nodes-by-op comparator-inv-patch :comparator-inv)))))
  (let [cmp-node (first (nodes-by-op comparator-inv-patch :comparator))
        inv-node (first (nodes-by-op comparator-inv-patch :comparator-inv))]
    (testing ":comparator-inv :source points to the :comparator node"
      (is (= (:id cmp-node) (get-in inv-node [:inputs :source])))))
  (testing "both ports appear in patch outputs"
    (is (= 2 (count (:outputs comparator-inv-patch))))))

;; ---------------------------------------------------------------------------
;; Multi-output — error cases
;; ---------------------------------------------------------------------------

(defn- cause-msg-matches? [e pattern]
  (loop [ex e]
    (cond (nil? ex) false
          (re-find pattern (str (.getMessage ex))) true
          :else (recur (.getCause ex)))))

(deftest multi-output-direct-use-error-test
  (testing "using a multi-output node directly throws a clear error"
    (is (try
          (macroexpand
            '(alembic.patch/defpatch! bad-patch {}
               (let [clk (alembic.patch/phasor 1.0)
                     rst (alembic.patch/comparator clk 0.5)
                     ctr (alembic.patch/counter {:max 4} clk (:out rst))]
                 (output ctr))))
          false
          (catch Exception e (cause-msg-matches? e #"multi-output node"))))))

(deftest unknown-port-error-test
  (testing "accessing an unknown port throws a clear error"
    (is (try
          (macroexpand
            '(alembic.patch/defpatch! bad-patch2 {}
               (let [clk (alembic.patch/phasor 1.0)
                     rst (alembic.patch/comparator clk 0.5)
                     ctr (alembic.patch/counter {:max 4} clk (:out rst))
                     bad (:nonexistent ctr)]
                 (output bad))))
          false
          (catch Exception e (cause-msg-matches? e #"Unknown port"))))))

;; ---------------------------------------------------------------------------
;; Level 1 gap ops — :naive-svf (multi-output)
;; ---------------------------------------------------------------------------

(defpatch! naive-svf-test {}
  (let [ph (phasor 440.0)
        f  (naive-svf ph 0.3 0.5)
        lp (:out f)
        hp (:hp f)]
    (output lp)
    (output hp)))

(deftest naive-svf-node-test
  (testing "creates primary :naive-svf node"
    (is (= 1 (count (nodes-by-op naive-svf-test :naive-svf)))))
  (testing "creates secondary :naive-svf-hp node"
    (is (= 1 (count (nodes-by-op naive-svf-test :naive-svf-hp)))))
  (let [node (first (nodes-by-op naive-svf-test :naive-svf))]
    (testing ":naive-svf has :in :cutoff :resonance inlets"
      (is (= #{:in :cutoff :resonance} (set (keys (:inputs node))))))
    (testing ":naive-svf is :sample rate"
      (is (= :sample (:rate node)))))
  (let [hp-node (first (nodes-by-op naive-svf-test :naive-svf-hp))
        lp-node (first (nodes-by-op naive-svf-test :naive-svf))]
    (testing ":naive-svf-hp :in matches primary :in"
      (is (= (get-in lp-node [:inputs :in])
             (get-in hp-node [:inputs :in]))))
    (testing ":naive-svf-hp :cutoff matches primary :cutoff"
      (is (= (get-in lp-node [:inputs :cutoff])
             (get-in hp-node [:inputs :cutoff])))))
  (testing "both ports appear in patch outputs"
    (is (= 2 (count (:outputs naive-svf-test))))))

;; ---------------------------------------------------------------------------
;; Level 1 gap ops — :crossover (multi-output)
;; ---------------------------------------------------------------------------

(defpatch! crossover-test {}
  (let [ph (phasor 440.0)
        xo (crossover ph 0.3)
        lp (:out xo)
        hp (:hp xo)]
    (output lp)
    (output hp)))

(deftest crossover-node-test
  (testing "creates primary :crossover node"
    (is (= 1 (count (nodes-by-op crossover-test :crossover)))))
  (testing "creates secondary :crossover-hp node"
    (is (= 1 (count (nodes-by-op crossover-test :crossover-hp)))))
  (let [node (first (nodes-by-op crossover-test :crossover))]
    (testing ":crossover has :in :cutoff inlets"
      (is (= #{:in :cutoff} (set (keys (:inputs node))))))
    (testing ":crossover is :sample rate"
      (is (= :sample (:rate node)))))
  (let [hp-node (first (nodes-by-op crossover-test :crossover-hp))
        lp-node (first (nodes-by-op crossover-test :crossover))]
    (testing ":crossover-hp :in matches primary :in"
      (is (= (get-in lp-node [:inputs :in])
             (get-in hp-node [:inputs :in]))))
    (testing ":crossover-hp :cutoff matches primary :cutoff"
      (is (= (get-in lp-node [:inputs :cutoff])
             (get-in hp-node [:inputs :cutoff])))))
  (testing "both ports appear in patch outputs"
    (is (= 2 (count (:outputs crossover-test))))))

;; ---------------------------------------------------------------------------
;; Level 1 gap ops — :hysteresis
;; ---------------------------------------------------------------------------

(defpatch! hysteresis-test {}
  (let [ph (phasor 440.0)
        hy (hysteresis ph 0.5 0.1)]
    (output hy)))

(deftest hysteresis-node-test
  (testing "has exactly one :hysteresis node"
    (is (= 1 (count (nodes-by-op hysteresis-test :hysteresis)))))
  (let [node (first (nodes-by-op hysteresis-test :hysteresis))]
    (testing ":hysteresis has :in :threshold :width inlets"
      (is (= #{:in :threshold :width} (set (keys (:inputs node))))))
    (testing ":hysteresis is :sample rate"
      (is (= :sample (:rate node))))))

;; ---------------------------------------------------------------------------
;; Level 1 gap ops — :damping
;; ---------------------------------------------------------------------------

(defpatch! damping-test {}
  (let [ph (phasor 440.0)
        d  (damping ph 0.9)]
    (output d)))

(deftest damping-node-test
  (testing "has exactly one :damping node"
    (is (= 1 (count (nodes-by-op damping-test :damping)))))
  (let [node (first (nodes-by-op damping-test :damping))]
    (testing ":damping has :in :coeff inlets"
      (is (= #{:in :coeff} (set (keys (:inputs node))))))
    (testing ":damping is :sample rate"
      (is (= :sample (:rate node))))))

;; ---------------------------------------------------------------------------
;; :segment — morphable slope waveshaper
;; ---------------------------------------------------------------------------

(defpatch! segment-triangle-test {}
  (let [ph  (phasor 1.0)
        out (segment ph 0.5 0.5)]
    (output out)))

(defpatch! segment-audio-shape-test {}
  (let [ph    (phasor 2.0)
        shape (sine-uni (phasor 0.1))
        out   (segment ph shape 0.5)]
    (output out)))

(deftest segment-node-test
  (testing "has exactly one :segment node"
    (is (= 1 (count (nodes-by-op segment-triangle-test :segment)))))
  (let [node (first (nodes-by-op segment-triangle-test :segment))]
    (testing ":segment has :phase :shape :curve inlets"
      (is (= #{:phase :shape :curve} (set (keys (:inputs node))))))
    (testing ":segment is :sample rate"
      (is (= :sample (:rate node))))))

(deftest segment-audio-rate-shape-test
  (testing "shape inlet accepts an audio-rate signal node"
    (let [node  (first (nodes-by-op segment-audio-shape-test :segment))
          nodes (:nodes segment-audio-shape-test)
          shape-id (get-in node [:inputs :shape])]
      (is (= :sample (:rate (get nodes shape-id)))))))

;; ---------------------------------------------------------------------------
;; (faust ...) inline authoring form
;; ---------------------------------------------------------------------------

(defpatch! faust-no-inlets-test {}
  (let [out (faust "os.osc(440.0)")]
    (output out)))

(deftest faust-node-no-inlets-test
  (testing "has exactly one :faust node"
    (is (= 1 (count (nodes-by-op faust-no-inlets-test :faust)))))
  (let [node (first (nodes-by-op faust-no-inlets-test :faust))]
    (testing ":faust node carries :source string"
      (is (= "os.osc(440.0)" (:source node))))
    (testing ":faust node has empty :inputs"
      (is (empty? (:inputs node))))
    (testing ":faust node is :sample rate"
      (is (= :sample (:rate node))))))

(defpatch! faust-wired-inlet-test {}
  (let [freq (param :freq)
        out  (faust "os.osc(%freq)" {:freq freq})]
    (output out)))

(deftest faust-node-wired-inlet-test
  (testing "has exactly one :faust node"
    (is (= 1 (count (nodes-by-op faust-wired-inlet-test :faust)))))
  (let [node (first (nodes-by-op faust-wired-inlet-test :faust))]
    (testing ":faust node has :freq inlet"
      (is (contains? (:inputs node) :freq)))
    (testing ":freq inlet points to a valid node id"
      (is (contains? (:nodes faust-wired-inlet-test) (get-in node [:inputs :freq]))))
    (testing ":source string preserved verbatim on node"
      (is (= "os.osc(%freq)" (:source node))))))

(deftest faust-rate-crossing-test
  (testing "block-rate inlet wired to :faust node creates rate-crossing edge"
    (let [crossings  (filter :rate-crossing? (:edges faust-wired-inlet-test))
          faust-node (first (nodes-by-op faust-wired-inlet-test :faust))]
      (is (some #(= (:id faust-node) (:to %)) crossings)))))

;; ---------------------------------------------------------------------------
;; Beat-domain ops — :beat-phase :beat-bpm :beat-trigger
;; ---------------------------------------------------------------------------

(defpatch! beat-phase-patch {}
  (let [ph  (beat-phase)
        out (sine-bi ph)]
    (output out)))

(deftest beat-phase-node-test
  (testing "has exactly one :beat-phase node"
    (is (= 1 (count (nodes-by-op beat-phase-patch :beat-phase)))))
  (let [node (first (nodes-by-op beat-phase-patch :beat-phase))]
    (testing ":beat-phase has no inlets"
      (is (empty? (:inputs node))))
    (testing ":beat-phase is :beat rate"
      (is (= :beat (:rate node))))))

(deftest beat-phase-rate-crossing-test
  (testing ":beat-phase → :sine-bi creates rate-crossing edge"
    (let [crossings  (filter :rate-crossing? (:edges beat-phase-patch))
          sine-node  (first (nodes-by-op beat-phase-patch :sine-bi))]
      (is (some #(= (:id sine-node) (:to %)) crossings)))))

(defpatch! beat-bpm-patch {}
  (let [bpm (beat-bpm)
        hz  (div bpm 60.0)
        out (phasor hz)]
    (output out)))

(deftest beat-bpm-node-test
  (testing "has exactly one :beat-bpm node"
    (is (= 1 (count (nodes-by-op beat-bpm-patch :beat-bpm)))))
  (let [node (first (nodes-by-op beat-bpm-patch :beat-bpm))]
    (testing ":beat-bpm has no inlets"
      (is (empty? (:inputs node))))
    (testing ":beat-bpm is :block rate"
      (is (= :block (:rate node)))))
  (testing ":beat-bpm → :div creates rate-crossing edge"
    (let [crossings (filter :rate-crossing? (:edges beat-bpm-patch))]
      (is (seq crossings)))))

(defpatch! beat-trigger-patch {}
  (let [ph   (beat-phase)
        trig (beat-trigger ph)]
    (output trig)))

(deftest beat-trigger-node-test
  (testing "has exactly one :beat-trigger node"
    (is (= 1 (count (nodes-by-op beat-trigger-patch :beat-trigger)))))
  (let [node (first (nodes-by-op beat-trigger-patch :beat-trigger))]
    (testing ":beat-trigger has :phase inlet"
      (is (contains? (:inputs node) :phase)))
    (testing ":beat-trigger is :sample rate"
      (is (= :sample (:rate node))))))

(deftest beat-trigger-rate-crossing-test
  (testing ":beat-phase → :beat-trigger creates rate-crossing edge"
    (let [crossings  (filter :rate-crossing? (:edges beat-trigger-patch))
          trig-node  (first (nodes-by-op beat-trigger-patch :beat-trigger))]
      (is (some #(= (:id trig-node) (:to %)) crossings)))))

(defpatch! beat-only-patch {}
  (let [ph (beat-phase)]
    (output ph)))

(deftest beat-dominant-rate-test
  (testing "patch with only :beat-phase has dominant rate :beat"
    (is (= :beat (:rate beat-only-patch))))
  (testing "adding sample-rate op promotes dominant rate to :sample"
    (is (= :sample (:rate beat-phase-patch)))))

