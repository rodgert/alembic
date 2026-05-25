; SPDX-License-Identifier: EPL-2.0
(ns alembic.viz-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [alembic.patch :refer [defpatch!]]
            [alembic.viz :refer [dot html render-svg]]))

;; ---------------------------------------------------------------------------
;; Shared patches
;; ---------------------------------------------------------------------------

(defpatch! fm-viz-patch
  {:params {:carrier   {:range [20.0 20000.0] :default 440.0 :unit :hz}
            :mod-ratio {:range [0.1 10.0]     :default 2.0}}}
  (let [mod-ph (phasor (mul (param :carrier) (param :mod-ratio)))
        sig    (sine-bi (add (phasor (param :carrier)) (sine-bi mod-ph)))]
    (output sig)))

(defpatch! feedback-viz-patch {}
  (let [ph (phasor 1.0)
        h  (history ph)]
    (output h)))

(defpatch! crossing-viz-patch {}
  (let [osc (phasor (param :freq))]
    (output :cv osc)))

(defpatch! beat-viz-patch {}
  (let [ph   (beat-phase)
        trig (beat-trigger ph)
        ctr  (counter {:max 8} trig 0.0)
        out  (:out ctr)]
    (output out)))

;; ---------------------------------------------------------------------------
;; dot — structure
;; ---------------------------------------------------------------------------

(deftest dot-starts-with-digraph-test
  (testing "DOT output starts with digraph declaration"
    (is (str/starts-with? (dot fm-viz-patch) "digraph patch {"))))

(deftest dot-ends-with-brace-test
  (testing "DOT output ends with closing brace"
    (is (str/ends-with? (str/trimr (dot fm-viz-patch)) "}"))))

(deftest dot-contains-all-nodes-test
  (testing "every node id appears in DOT output"
    (let [src   (dot fm-viz-patch)
          nodes (:nodes fm-viz-patch)]
      (doseq [[id _] nodes]
        (is (str/includes? src (name id))
            (str "missing node " id))))))

(deftest dot-op-names-in-labels-test
  (let [src (dot fm-viz-patch)]
    (testing "param op name appears in output"
      (is (str/includes? src "param")))
    (testing "phasor op name appears in output"
      (is (str/includes? src "phasor")))
    (testing "sine-bi op name appears in output"
      (is (str/includes? src "sine-bi")))))

;; ---------------------------------------------------------------------------
;; dot — rate colours
;; ---------------------------------------------------------------------------

(deftest dot-rate-colours-test
  (let [src (dot crossing-viz-patch)]
    (testing "block-rate nodes use yellow fill"
      (is (str/includes? src "#fff5b1")))
    (testing "sample-rate nodes use white fill"
      (is (str/includes? src "#ffffff")))))

(deftest dot-beat-colour-test
  (let [src (dot beat-viz-patch)]
    (testing "beat-rate nodes use blue fill"
      (is (str/includes? src "#b3d9ff")))))

;; ---------------------------------------------------------------------------
;; dot — edge annotations
;; ---------------------------------------------------------------------------

(deftest dot-rate-crossing-edges-test
  (let [src (dot crossing-viz-patch)]
    (testing "rate-crossing edges are dashed and red"
      (is (str/includes? src "style=dashed"))
      (is (str/includes? src "color=red")))))

(deftest dot-feedback-edges-test
  (let [src (dot feedback-viz-patch)]
    (testing "feedback edges are dotted and blue"
      (is (str/includes? src "style=dotted"))
      (is (str/includes? src "color=blue")))))

;; ---------------------------------------------------------------------------
;; dot — output sinks
;; ---------------------------------------------------------------------------

(deftest dot-output-sink-test
  (let [src (dot fm-viz-patch)]
    (testing "output sink node appears as octagon"
      (is (str/includes? src "shape=octagon")))
    (testing "output sink has pale green fill"
      (is (str/includes? src "#c8f0c8")))
    (testing "_out_0 sink id is present"
      (is (str/includes? src "_out_0")))))

(deftest dot-named-output-channel-test
  (let [src (dot crossing-viz-patch)]
    (testing "named :cv output maps to channel 0 sink"
      (is (str/includes? src "_out_0")))
    (testing "output name 'cv' appears in sink label"
      (is (str/includes? src "cv")))))

;; ---------------------------------------------------------------------------
;; dot — node shapes
;; ---------------------------------------------------------------------------

(deftest dot-param-shape-test
  (let [src (dot crossing-viz-patch)]
    (testing "param nodes use parallelogram shape"
      (is (str/includes? src "parallelogram")))))

(deftest dot-faust-double-border-test
  (let [graph {:nodes   {:n0 {:id :n0 :op :faust :source "os.osc(440.0)" :rate :sample}}
               :edges   []
               :params  {}
               :outputs [{:node :n0 :channel 0 :name "Main"}]
               :rate    :sample}]
    (testing ":faust nodes get peripheries=2 (double border)"
      (is (str/includes? (dot graph) "peripheries=2")))))

;; ---------------------------------------------------------------------------
;; dot — determinism
;; ---------------------------------------------------------------------------

(deftest dot-deterministic-test
  (testing "two calls with the same graph produce identical DOT output"
    (is (= (dot fm-viz-patch) (dot fm-viz-patch)))))

;; ---------------------------------------------------------------------------
;; html — structure
;; ---------------------------------------------------------------------------

(deftest html-structure-test
  (let [src (html fm-viz-patch)]
    (testing "HTML output is a valid HTML document"
      (is (str/includes? src "<!DOCTYPE html>")))
    (testing "embeds the DOT source"
      (is (str/includes? src "digraph patch {")))
    (testing "references viz.js"
      (is (str/includes? src "viz-standalone.js")))
    (testing "has a #graph div"
      (is (str/includes? src "id=\"graph\"")))
    (testing "contains Viz.instance() call"
      (is (str/includes? src "Viz.instance()")))))

;; ---------------------------------------------------------------------------
;; render-svg — requires Graphviz; skipped if `dot` not on PATH
;; ---------------------------------------------------------------------------

(defn- graphviz-available? []
  (try (zero? (:exit (clojure.java.shell/sh "dot" "-V")))
       (catch Exception _ false)))

(deftest render-svg-test
  (when (graphviz-available?)
    (let [svg (render-svg fm-viz-patch)]
      (testing "render-svg returns a non-empty SVG string"
        (is (str/includes? svg "<svg")))
      (testing "SVG contains node elements"
        (is (str/includes? svg "<g")))
      (testing "SVG is well-formed (has closing svg tag)"
        (is (str/includes? svg "</svg>"))))))
