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
;; Level 1 filters — :ladder
;; ---------------------------------------------------------------------------

(defpatch! ladder-emit-patch {}
  (let [osc (phasor 110.0)
        sig (sine-bi osc)
        out (ladder sig 0.25 0.6)]
    (output out)))

(deftest ladder-emit-test
  (let [src (emit-faust ladder-emit-patch)]
    (testing "emits ve.moogLadder call"
      (is (str/includes? src "ve.moogLadder(")))
    (testing "resonance rescaling constants present"
      (is (str/includes? src "0.707107"))
      (is (str/includes? src "24.292893")))
    (testing "cutoff and resonance nodes appear before ladder node (topo order)"
      (let [lines      (str/split-lines src)
            idx-of     (fn [pat] (first (keep-indexed #(when (re-find pat %2) %1) lines)))
            moog-line  (idx-of #"ve\.moogLadder")
            osc-line   (idx-of #"os\.phasor")]
        (is (some? moog-line))
        (is (< osc-line moog-line))))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! ladder-with-params-patch
  {:params {:cutoff    {:range [0.0 1.0] :default 0.3}
            :resonance {:range [0.0 1.0] :default 0.0}}}
  (let [osc (phasor 220.0)
        sig (sine-bi osc)
        out (ladder sig (param :cutoff) (param :resonance))]
    (output out)))

(deftest ladder-param-modulated-test
  (let [src (emit-faust ladder-with-params-patch)]
    (testing "cutoff and resonance params appear as hsliders"
      (is (str/includes? src "\"cutoff\""))
      (is (str/includes? src "\"resonance\"")))
    (testing "ve.moogLadder still emitted"
      (is (str/includes? src "ve.moogLadder(")))))

;; ---------------------------------------------------------------------------
;; Level 1 extended — :one-pole :dc-block :allpass
;; ---------------------------------------------------------------------------

(defpatch! one-pole-emit-patch {}
  (let [osc (phasor 220.0)
        out (one-pole osc 0.5)]
    (output out)))

(deftest one-pole-emit-test
  (let [src (emit-faust one-pole-emit-patch)]
    (testing "emits si.smooth call"
      (is (str/includes? src "si.smooth(")))
    (testing "cutoff clamping constants present"
      (is (str/includes? src "0.9999")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! dc-block-emit-patch {}
  (let [osc (phasor 1.0)
        out (dc-block osc)]
    (output out)))

(deftest dc-block-emit-test
  (let [src (emit-faust dc-block-emit-patch)]
    (testing "emits fi.dcblocker"
      (is (str/includes? src "fi.dcblocker")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! allpass-emit-patch {}
  (let [osc (phasor 1.0)
        out (allpass osc 0.02 0.5)]
    (output out)))

(deftest allpass-emit-test
  (let [src (emit-faust allpass-emit-patch)]
    (testing "emits de.apf call"
      (is (str/includes? src "de.apf(")))
    (testing "converts time to samples via ma.SR"
      (is (str/includes? src "ma.SR")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; Level 1 extended — :svf
;; ---------------------------------------------------------------------------

(defpatch! svf-emit-patch {}
  (let [osc (phasor 110.0)
        sig (sine-bi osc)
        out (svf sig 0.3 0.5 0.0)]
    (output out)))

(deftest svf-emit-test
  (let [src (emit-faust svf-emit-patch)]
    (testing "emits fi.svf_morph call"
      (is (str/includes? src "fi.svf_morph(")))
    (testing "cutoff scaled via ma.SR"
      (is (str/includes? src "ma.SR")))
    (testing "resonance rescaling constants present"
      (is (str/includes? src "0.5"))
      (is (str/includes? src "9.5")))
    (testing "mode scaling by 2.0 present"
      (is (str/includes? src "2.0")))
    (testing "osc node appears before svf node (topo order)"
      (let [lines      (str/split-lines src)
            idx-of     (fn [pat] (first (keep-indexed #(when (re-find pat %2) %1) lines)))
            svf-line   (idx-of #"fi\.svf_morph")
            osc-line   (idx-of #"os\.phasor")]
        (is (some? svf-line))
        (is (< osc-line svf-line))))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! svf-with-params-patch
  {:params {:cutoff    {:range [0.0 1.0] :default 0.3}
            :resonance {:range [0.0 1.0] :default 0.5}
            :mode      {:range [0.0 1.0] :default 0.0}}}
  (let [osc (phasor 220.0)
        sig (sine-bi osc)
        out (svf sig (param :cutoff) (param :resonance) (param :mode))]
    (output out)))

(deftest svf-param-modulated-test
  (let [src (emit-faust svf-with-params-patch)]
    (testing "cutoff, resonance, mode params appear as hsliders"
      (is (str/includes? src "\"cutoff\""))
      (is (str/includes? src "\"resonance\""))
      (is (str/includes? src "\"mode\"")))
    (testing "fi.svf_morph still emitted"
      (is (str/includes? src "fi.svf_morph(")))))

;; ---------------------------------------------------------------------------
;; Level 1 extended — signal ops
;; ---------------------------------------------------------------------------

(defpatch! vca-emit-patch {}
  (let [osc (phasor 440.0)
        out (vca osc 0.5)]
    (output out)))

(deftest vca-emit-test
  (let [src (emit-faust vca-emit-patch)]
    (testing "emits multiply expression"
      (is (re-find #"n\d+ \* n\d+" src)))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! slew-emit-patch {}
  (let [osc (phasor 1.0)
        out (slew osc 0.01 0.03)]
    (output out)))

(deftest slew-emit-test
  (let [src (emit-faust slew-emit-patch)]
    (testing "emits select2 with feedback loop"
      (is (str/includes? src "select2("))
      (is (str/includes? src "~ _")))
    (testing "uses ma.SR for time constant conversion"
      (is (str/includes? src "ma.SR")))
    (testing "emits exp for RC coefficient"
      (is (str/includes? src "exp(")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! sample-hold-emit-patch {}
  (let [osc  (phasor 440.0)
        trig (phasor 1.0)
        out  (sample-hold osc trig)]
    (output out)))

(deftest sample-hold-emit-test
  (let [src (emit-faust sample-hold-emit-patch)]
    (testing "emits ba.sAndH call"
      (is (str/includes? src "ba.sAndH(")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! comparator-emit-patch {}
  (let [osc (phasor 440.0)
        out (comparator osc 0.0)]
    (output out)))

(deftest comparator-emit-test
  (let [src (emit-faust comparator-emit-patch)]
    (testing "emits float(... > ...) threshold test"
      (is (re-find #"float\(.+>\s*.+\)" src)))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! noise-emit-patch {}
  (let [n (noise)]
    (output n)))

(deftest noise-emit-test
  (let [src (emit-faust noise-emit-patch)]
    (testing "emits no.noise"
      (is (str/includes? src "no.noise")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! pink-noise-emit-patch {}
  (let [n (pink-noise)]
    (output n)))

(deftest pink-noise-emit-test
  (let [src (emit-faust pink-noise-emit-patch)]
    (testing "emits no.pink_noise"
      (is (str/includes? src "no.pink_noise")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! crossfade-emit-patch {}
  (let [a   (phasor 440.0)
        b   (phasor 441.0)
        out (crossfade a b 0.5)]
    (output out)))

(deftest crossfade-emit-test
  (let [src (emit-faust crossfade-emit-patch)]
    (testing "emits ba.crossfade call"
      (is (str/includes? src "ba.crossfade(")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; Compile-time options — :vco shapes
;; ---------------------------------------------------------------------------

(defpatch! vco-saw-emit-patch {}
  (let [osc (vco {:shape :saw} 440.0)]
    (output osc)))

(defpatch! vco-sine-emit-patch {}
  (let [osc (vco {:shape :sine} 440.0)]
    (output osc)))

(defpatch! vco-square-emit-patch {}
  (let [osc (vco {:shape :square} 440.0)]
    (output osc)))

(defpatch! vco-triangle-emit-patch {}
  (let [osc (vco {:shape :triangle} 440.0)]
    (output osc)))

(defpatch! vco-pulse-emit-patch {}
  (let [osc (vco {:shape :pulse :pw 0.3} 440.0)]
    (output osc)))

(deftest vco-shape-emit-test
  (testing ":saw emits os.lf_saw"
    (is (str/includes? (emit-faust vco-saw-emit-patch) "os.lf_saw(")))
  (testing ":sine emits os.osc"
    (is (str/includes? (emit-faust vco-sine-emit-patch) "os.osc(")))
  (testing ":square emits os.lf_squarewave"
    (is (str/includes? (emit-faust vco-square-emit-patch) "os.lf_squarewave(")))
  (testing ":triangle emits os.lf_triangle"
    (is (str/includes? (emit-faust vco-triangle-emit-patch) "os.lf_triangle(")))
  (testing ":pulse emits select2 with phasor comparison"
    (let [src (emit-faust vco-pulse-emit-patch)]
      (is (str/includes? src "select2("))
      (is (str/includes? src "os.phasor("))
      ;; pw=0.3 should appear as a numeric literal in the expression
      (is (str/includes? src "0.3")))))

(deftest vco-different-shapes-differ-test
  (testing "different shapes produce different Faust output"
    (is (not= (emit-faust vco-saw-emit-patch)
              (emit-faust vco-sine-emit-patch)))))

;; ---------------------------------------------------------------------------
;; Compile-time options — :counter
;; ---------------------------------------------------------------------------

(defpatch! counter-up-emit-patch {}
  (let [clk (phasor 4.0)
        rst (phasor 0.5)
        out (counter {:max 8 :dir :up :wrap true} clk rst)]
    (output out)))

(defpatch! counter-down-emit-patch {}
  (let [clk (phasor 4.0)
        rst (phasor 0.5)
        out (counter {:max 16 :dir :down :wrap false} clk rst)]
    (output out)))

(deftest counter-up-emit-test
  (let [src (emit-faust counter-up-emit-patch)]
    (testing "emits 1-sample feedback loop"
      (is (str/includes? src "~ _")))
    (testing "emits edge detection via delayed clock comparison"
      (is (str/includes? src "> 0.5"))
      (is (str/includes? src "<= 0.5")))
    (testing "emits fmod for wrap"
      (is (str/includes? src "fmod(")))
    (testing "max value 8 appears in output"
      (is (str/includes? src "8.0")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(deftest counter-down-emit-test
  (let [src (emit-faust counter-down-emit-patch)]
    (testing "down+no-wrap emits max()"
      (is (str/includes? src "max(")))
    (testing "starts at max-1 (15) on reset"
      (is (str/includes? src "15.0")))))

(deftest counter-up-vs-down-differ-test
  (testing "different dir options produce different Faust output"
    (is (not= (emit-faust counter-up-emit-patch)
              (emit-faust counter-down-emit-patch)))))

;; ---------------------------------------------------------------------------
;; Compile-time options — :table
;; ---------------------------------------------------------------------------

(defpatch! table-wrap-emit-patch {}
  (let [ph  (phasor 440.0)
        idx (mul ph 3.0)
        out (table {:data [0.0 0.5 1.0 0.5] :size 4 :mode :wrap} idx)]
    (output out)))

(defpatch! table-clamp-emit-patch {}
  (let [ph  (phasor 440.0)
        idx (mul ph 3.0)
        out (table {:data [0.0 0.5 1.0 0.5] :size 4 :mode :clamp} idx)]
    (output out)))

(defpatch! table-fold-emit-patch {}
  (let [ph  (phasor 440.0)
        idx (mul ph 3.0)
        out (table {:data [0.0 0.5 1.0 0.5] :size 4 :mode :fold} idx)]
    (output out)))

(deftest table-emit-test
  (testing "emits rdtable call"
    (is (str/includes? (emit-faust table-wrap-emit-patch) "rdtable(")))
  (testing "emits waveform with data"
    (is (str/includes? (emit-faust table-wrap-emit-patch) "waveform{")))
  (testing "data values appear in output"
    (let [src (emit-faust table-wrap-emit-patch)]
      (is (str/includes? src "0.5"))
      (is (str/includes? src "1.0"))))
  (testing "size 4 appears in output"
    (is (str/includes? (emit-faust table-wrap-emit-patch) "4"))))

(deftest table-modes-differ-test
  (testing ":wrap uses floor-based positive modulo"
    (is (str/includes? (emit-faust table-wrap-emit-patch) "floor(")))
  (testing ":clamp uses max/min"
    (let [src (emit-faust table-clamp-emit-patch)]
      (is (str/includes? src "max("))
      (is (str/includes? src "min("))))
  (testing ":fold uses abs"
    (is (str/includes? (emit-faust table-fold-emit-patch) "abs(")))
  (testing "different modes produce different index expressions"
    (is (not= (emit-faust table-wrap-emit-patch)
              (emit-faust table-clamp-emit-patch)))))

;; ---------------------------------------------------------------------------
;; Level 1 extended — :ring-mod :bitcrusher
;; ---------------------------------------------------------------------------

(defpatch! ring-mod-emit-patch {}
  (let [c   (phasor 440.0)
        m   (phasor 110.0)
        out (ring-mod c m 0.0)]
    (output out)))

(deftest ring-mod-emit-test
  (let [src (emit-faust ring-mod-emit-patch)]
    (testing "emits carrier × (modulator + dc) expression"
      (is (re-find #"n\d+ \* \(n\d+ \+ n\d+\)" src)))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! bitcrusher-emit-patch {}
  (let [osc (phasor 440.0)
        out (bitcrusher osc 12.0)]
    (output out)))

(deftest bitcrusher-emit-test
  (let [src (emit-faust bitcrusher-emit-patch)]
    (testing "emits floor rounding expression"
      (is (str/includes? src "floor(")))
    (testing "emits pow for bit-depth scaling"
      (is (str/includes? src "pow(2.0,")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; Level 1 extended — waveshapers
;; ---------------------------------------------------------------------------

(defpatch! soft-clip-emit-patch {}
  (let [osc (phasor 440.0)
        out (soft-clip osc)]
    (output out)))

(deftest soft-clip-emit-test
  (let [src (emit-faust soft-clip-emit-patch)]
    (testing "emits Padé tanh: x/(1+|x|)"
      (is (re-find #"n\d+ / \(1\.0 \+ abs\(n\d+\)\)" src)))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! hard-clip-emit-patch {}
  (let [osc (phasor 440.0)
        out (hard-clip osc)]
    (output out)))

(deftest hard-clip-emit-test
  (let [src (emit-faust hard-clip-emit-patch)]
    (testing "emits max/min clamp"
      (is (re-find #"max\(-1\.0, min\(-?1\.0," src)))
    (testing "emits cubic polynomial coefficient 1.5"
      (is (str/includes? src "1.5")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! wave-fold-emit-patch {}
  (let [osc (phasor 440.0)
        out (wave-fold osc)]
    (output out)))

(deftest wave-fold-emit-test
  (let [src (emit-faust wave-fold-emit-patch)]
    (testing "emits floor-based positive-modulo fold"
      (is (str/includes? src "floor("))
      (is (str/includes? src "4.0")))
    (testing "emits abs for triangle shape"
      (is (str/includes? src "abs(")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; Named outputs — gap padding in process declaration
;; ---------------------------------------------------------------------------

(defpatch! named-cv-gate-patch {}
  (let [cv-sig   (phasor 1.0)
        gate-sig (sine-bi cv-sig)]
    (output :cv   cv-sig)
    (output :gate gate-sig)))

(deftest named-gap-emit-test
  (let [src (emit-faust named-cv-gate-patch)]
    (testing "gap at channel 1 (aux) padded with 0.0"
      ;; channels 0=cv, 1=gap(0.0), 2=gate → process = nX, 0.0, nY;
      (is (re-find #"process = n\d+, 0\.0, n\d+;" src)))))

(defpatch! named-all-four-patch {}
  (let [ph   (phasor 1.0)
        sig  (sine-bi ph)
        gate (tri ph)
        g2   (mul sig gate)]
    (output :cv    sig)
    (output :aux   gate)
    (output :gate  g2)
    (output :gate2 sig)))

(deftest named-all-four-emit-test
  (let [src (emit-faust named-all-four-patch)]
    (testing "four contiguous outputs, no padding needed"
      (is (re-find #"process = n\d+, n\d+, n\d+, n\d+;" src)))))

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
