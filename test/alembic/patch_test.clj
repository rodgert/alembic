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
