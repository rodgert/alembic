; SPDX-License-Identifier: EPL-2.0
(ns alembic.emit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [alembic.emit :refer [emit-faust]]
            [alembic.patch :refer [defpatch!]]))

;; ---------------------------------------------------------------------------
;; Smoke — minimal patch
;; ---------------------------------------------------------------------------

(defpatch! phasor-patch {}
  (let [osc (phasor 440.0)]
    (output osc)))

(deftest minimal-emit-test
  (let [src (emit-faust phasor-patch)]
    (testing "starts with stdfaust import"
      (is (str/starts-with? src "import(\"stdfaust.lib\");")))
    (testing "contains the freq literal"
      (is (str/includes? src "440.0")))
    (testing "uses os.phasor"
      (is (str/includes? src "os.phasor(1.0,")))
    (testing "ends with a process declaration"
      (is (re-find #"process = n\d+;" src)))
    (testing "const node appears before phasor node (topo order)"
      (let [lines      (str/split-lines src)
            idx-of     (fn [pred] (first (keep-indexed #(when (pred %2) %1) lines)))
            const-line  (idx-of #(re-find #"= 440\.0" %))
            phasor-line (idx-of #(re-find #"os\.phasor" %))]
        (is (< const-line phasor-line))))))

;; ---------------------------------------------------------------------------
;; Param → hslider
;; ---------------------------------------------------------------------------

(defpatch! param-patch
  {:params {:freq {:range [20.0 20000.0] :default 440.0}}}
  (let [osc (phasor (param :freq))]
    (output osc)))

(deftest param-hslider-test
  (let [src (emit-faust param-patch)]
    (testing "hslider declaration emitted"
      (is (str/includes? src "hslider(")))
    (testing "param name in hslider"
      (is (str/includes? src "\"freq\"")))
    (testing "default value"
      (is (str/includes? src "440.0")))
    (testing "range bounds"
      (is (str/includes? src "20.0"))
      (is (str/includes? src "20000.0")))))

;; ---------------------------------------------------------------------------
;; Param with no schema entry falls back to [0.0 1.0]
;; ---------------------------------------------------------------------------

(defpatch! param-no-schema-patch {}
  (let [osc (phasor (param :rate))]
    (output osc)))

(deftest param-default-range-test
  (let [src (emit-faust param-no-schema-patch)]
    (testing "falls back to [0.0 1.0] range"
      (is (str/includes? src "hslider(\"rate\", 0.0, 0.0, 1.0,")))))

;; ---------------------------------------------------------------------------
;; Stereo — two outputs produce two signals in process
;; ---------------------------------------------------------------------------

(defpatch! stereo-patch {}
  (let [left  (phasor 440.0)
        right (phasor 441.0)]
    (output left)
    (output right)))

(deftest stereo-process-test
  (let [src (emit-faust stereo-patch)]
    (testing "process lists two signals separated by comma"
      (is (re-find #"process = n\d+, n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; FM voice — structural smoke
;; ---------------------------------------------------------------------------

(defpatch! fm-patch
  {:params {:carrier   {:range [20.0 20000.0] :default 440.0}
            :mod-ratio {:range [0.1 10.0]     :default 2.0}}}
  (let [mod-ph (phasor (mul (param :carrier) (param :mod-ratio)))
        sig    (sine-bi (add (phasor (param :carrier)) (sine-bi mod-ph)))]
    (output sig)))

(deftest fm-voice-emit-test
  (let [src (emit-faust fm-patch)]
    (testing "imports stdfaust"
      (is (str/includes? src "import(\"stdfaust.lib\");")))
    (testing "emits os.phasor at least twice"
      (is (<= 2 (count (re-seq #"os\.phasor" src)))))
    (testing "emits sin formula"
      (is (str/includes? src "sin(2.0*ma.PI*")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))
    (testing "hsliders for both params"
      (is (str/includes? src "\"carrier\""))
      (is (str/includes? src "\"mod-ratio\"")))
    (testing "all definitions precede process line"
      (let [lines     (str/split-lines src)
            proc-line (first (keep-indexed #(when (str/starts-with? %2 "process") %1) lines))
            def-lines (keep-indexed #(when (re-find #"^n\d+ = " %2) %1) lines)]
        (is (every? #(< % proc-line) def-lines))))))

;; ---------------------------------------------------------------------------
;; Arithmetic ops
;; ---------------------------------------------------------------------------

(defpatch! arith-patch {}
  (let [a (phasor 1.0)
        b (phasor 2.0)
        s (add a b)
        d (sub s b)
        p (mul d a)
        q (div p b)]
    (output q)))

(deftest arithmetic-emit-test
  (let [src (emit-faust arith-patch)]
    (is (re-find #"\(n\d+ \+ n\d+\)" src) "add")
    (is (re-find #"\(n\d+ - n\d+\)" src) "sub")
    (is (re-find #"\(n\d+ \* n\d+\)" src) "mul")
    (is (re-find #"\(n\d+ / n\d+\)" src) "div")))

;; ---------------------------------------------------------------------------
;; Fold / wrap / clip — use numeric literals for lo/hi
;; ---------------------------------------------------------------------------

(defpatch! shapers-patch {}
  (let [ph (phasor 1.0)
        w  (wrap ph 0.0 1.0)
        f  (fold ph 0.0 1.0)
        c  (clip ph 0.0 1.0)]
    (output w)
    (output f)
    (output c)))

(deftest shaper-emit-test
  (let [src (emit-faust shapers-patch)]
    (is (str/includes? src "fmod(") "wrap/fold use fmod")
    (is (re-find #"max\(.+min\(" src) "clip uses max/min")))

;; ---------------------------------------------------------------------------
;; History / delta — Faust ' operator
;; ---------------------------------------------------------------------------

(defpatch! history-patch {}
  (let [ph  (phasor 1.0)
        h   (history ph)
        dlt (delta ph)]
    (output h)
    (output dlt)))

(deftest history-emit-test
  (let [src (emit-faust history-patch)]
    (testing "history emits Faust ' operator"
      (is (re-find #"n\d+'" src)))
    (testing "delta subtracts delayed copy"
      (is (re-find #"\(n\d+ - n\d+'\)" src)))))

;; ---------------------------------------------------------------------------
;; Raw Faust passthrough — graph constructed directly (no defpatch!)
;; ---------------------------------------------------------------------------

(def raw-faust-graph
  {:nodes   {:n0 {:id :n0 :op :faust :source "os.osc(440.0)" :rate :sample}}
   :edges   []
   :params  {}
   :outputs [{:node :n0 :channel 0 :name "Main"}]
   :rate    :sample})

(deftest raw-faust-emit-test
  (let [src (emit-faust raw-faust-graph)]
    (is (str/includes? src "os.osc(440.0)"))))

;; ---------------------------------------------------------------------------
;; Error — no outputs
;; ---------------------------------------------------------------------------

(deftest no-outputs-throws-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no outputs"
        (emit-faust {:nodes {} :edges [] :params {} :outputs []}))))

;; ---------------------------------------------------------------------------
;; Error — unknown op
;; ---------------------------------------------------------------------------

(deftest unknown-op-throws-test
  (let [bad-graph {:nodes   {:n0 {:id :n0 :op :const :value 1.0 :rate :sample}
                             :n1 {:id :n1 :op :UNKNOWN :inputs {:input :n0} :rate :sample}}
                  :edges   [{:from :n0 :to :n1 :inlet :input}]
                  :params  {}
                  :outputs [{:node :n1 :channel 0 :name "Main"}]
                  :rate    :sample}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown op"
          (emit-faust bad-graph)))))
