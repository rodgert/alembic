; SPDX-License-Identifier: EPL-2.0
(ns alembic.emit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [alembic.emit :refer [emit-faust]]
            [alembic.compile]
            [alembic.patch :refer [defpatch!]]
            [examples.ks-string]
            [examples.shelves]))

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
;; :delay — opts-aware delay line
;; ---------------------------------------------------------------------------

(defpatch! delay-default-patch {}
  (let [sig (sine-bi (phasor 440.0))
        out (delay sig 0.1)]
    (output out)))

(defpatch! delay-raw-patch {}
  (let [sig (sine-bi (phasor 440.0))
        out (delay {:max-time 0.5 :smooth false :interp :linear} sig 0.1)]
    (output out)))

(defpatch! delay-integer-patch {}
  (let [sig (sine-bi (phasor 440.0))
        out (delay {:max-time 0.2 :smooth false :interp :none} sig 0.05)]
    (output out)))

(defpatch! delay-cv-patch {}
  (let [sig    (sine-bi (phasor 440.0))
        timecv (mul (phasor 1.0) 0.02)
        out    (delay {:max-time 0.05 :time-cv true} sig timecv)]
    (output out)))

(deftest delay-default-smooth-test
  (let [src (emit-faust delay-default-patch)]
    (testing "default emits de.sdelay (smooth)"
      (is (str/includes? src "de.sdelay(")))
    (testing "crossfade length 1024 is present"
      (is (str/includes? src "1024")))
    (testing "max buffer size uses ma.SR"
      (is (str/includes? src "ma.SR")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(deftest delay-smooth-false-test
  (let [src (emit-faust delay-raw-patch)]
    (testing "smooth false emits de.fdelay"
      (is (str/includes? src "de.fdelay(")))
    (testing "does not emit de.sdelay"
      (is (not (str/includes? src "de.sdelay("))))))

(deftest delay-interp-none-test
  (let [src (emit-faust delay-integer-patch)]
    (testing "interp :none emits de.delay"
      (is (str/includes? src "de.delay(")))
    (testing "int() wraps delay time in second arg"
      (is (re-find #"de\.delay\([^,]+, int\(" src)))))

(deftest delay-time-cv-test
  (let [src (emit-faust delay-cv-patch)]
    (testing "time-cv true emits de.fdelay driven by cv signal"
      (is (str/includes? src "de.fdelay(")))
    (testing "max-time 0.05 reflected in buffer size"
      (is (str/includes? src "0.05")))))

(deftest delay-max-time-test
  (testing "max-time 0.5 reflected in buffer constant"
    (is (str/includes? (emit-faust delay-raw-patch) "0.5")))
  (testing "different max-times produce different Faust output"
    (is (not= (emit-faust delay-default-patch)
              (emit-faust delay-raw-patch)))))

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
        cmp (comparator osc 0.0)
        out (:out cmp)]
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
    (testing "emits linear blend: (1.0 - pos) * a + pos * b"
      (is (re-find #"\(1\.0 - n\d+\) \* n\d+ \+ n\d+ \* n\d+" src)))
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
        ctr (counter {:max 8 :dir :up :wrap true} clk rst)
        out (:out ctr)]
    (output out)))

(defpatch! counter-down-emit-patch {}
  (let [clk (phasor 4.0)
        rst (phasor 0.5)
        ctr (counter {:max 16 :dir :down :wrap false} clk rst)
        out (:out ctr)]
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
;; :buffer — read-write circular buffer
;; ---------------------------------------------------------------------------

(defpatch! buffer-emit-patch {}
  (let [ph   (phasor 1.0)
        sig  (sine-bi ph)
        wpos (mul ph 4800.0)
        rpos (sub wpos 240.0)
        out  (buffer {:size 4800} sig wpos rpos)]
    (output out)))

(deftest buffer-emit-test
  (let [src (emit-faust buffer-emit-patch)]
    (testing "emits rwtable call"
      (is (str/includes? src "rwtable(")))
    (testing "size 4800 appears in output"
      (is (str/includes? src "4800")))
    (testing "int() wraps both index signals"
      (is (= 2 (count (re-seq #"int\(" src)))))
    (testing "initial value is 0.0"
      (is (str/includes? src "rwtable(4800, 0.0,")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

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

;; ---------------------------------------------------------------------------
;; Variable-arity inlets — :select emit
;; ---------------------------------------------------------------------------

(defpatch! select-emit-2 {}
  (let [a (phasor 110.0)
        b (phasor 220.0)
        s (select {:n 2} a b 0.0)]
    (output s)))

(deftest select-emit-n2-test
  (let [src (emit-faust select-emit-2)]
    (testing "emits ba.selectn"
      (is (str/includes? src "ba.selectn(2,")))
    (testing "int() wraps the index"
      (is (re-find #"ba\.selectn\(2,\s*int\(" src)))))

(defpatch! select-emit-4 {}
  (let [a (phasor 55.0)
        b (phasor 110.0)
        c (phasor 220.0)
        d (phasor 440.0)
        s (select {:n 4} a b c d 0.0)]
    (output s)))

(deftest select-emit-n4-test
  (let [src (emit-faust select-emit-4)]
    (testing "emits ba.selectn with n=4"
      (is (str/includes? src "ba.selectn(4,")))
    (testing "four signal node refs in the selectn call"
      (is (re-find #"ba\.selectn\(4,\s*int\(n\d+\),\s*n\d+,\s*n\d+,\s*n\d+,\s*n\d+" src)))))

;; ---------------------------------------------------------------------------
;; Multi-output emit — :counter-carry
;; ---------------------------------------------------------------------------

(defpatch! counter-carry-emit {}
  (let [clk   (phasor 4.0)
        rst   (comparator clk 0.5)
        ctr   (counter {:max 8 :dir :up :wrap true} clk (:out rst))
        cnt   (:out ctr)
        carry (:carry ctr)]
    (output cnt)
    (output carry)))

(deftest counter-carry-emit-test
  (let [src (emit-faust counter-carry-emit)]
    (testing "counter-carry uses float() predicate"
      (is (re-find #"float\(" src)))
    (testing "counter-carry checks == 0.0 for :up :wrap"
      (is (re-find #"== 0\.0" src)))
    (testing "counter-carry uses rising-edge detection"
      (is (re-find #"> 0\.5.*<= 0\.5" src)))
    (testing "carry node appears after counter node in topo order"
      (let [lines (str/split-lines src)
            idx   (fn [re] (first (keep-indexed #(when (re-find re %2) %1) lines)))]
        (is (< (idx #"select2") (idx #"== 0\.0")))))))

;; ---------------------------------------------------------------------------
;; Multi-output emit — :comparator-inv
;; ---------------------------------------------------------------------------

(defpatch! comparator-inv-emit {}
  (let [sig  (phasor 1.0)
        cmp  (comparator sig 0.5)
        gate (:out cmp)
        inv  (:inv-gate cmp)]
    (output gate)
    (output inv)))

(deftest comparator-inv-emit-test
  (let [src (emit-faust comparator-inv-emit)]
    (testing "comparator-inv emits 1.0 - source"
      (is (re-find #"\(1\.0 - n\d+\)" src)))
    (testing "comparator-inv node appears after comparator node in topo order"
      (let [lines (str/split-lines src)
            idx   (fn [re] (first (keep-indexed #(when (re-find re %2) %1) lines)))]
        (is (< (idx #"float\(n\d+ > n\d+\)") (idx #"1\.0 - n\d+")))))))

;; ---------------------------------------------------------------------------
;; Level 1 gap ops — :naive-svf emit
;; ---------------------------------------------------------------------------

(defpatch! naive-svf-emit-patch {}
  (let [ph (phasor 440.0)
        f  (naive-svf ph 0.3 0.5)
        lp (:out f)
        hp (:hp f)]
    (output lp)
    (output hp)))

(deftest naive-svf-emit-test
  (let [src (emit-faust naive-svf-emit-patch)]
    (testing "emits fi.svf_morph for both LP and HP"
      (is (= 2 (count (re-seq #"fi\.svf_morph" src)))))
    (testing "LP uses blend 0.0"
      (is (str/includes? src ", 0.0, ")))
    (testing "HP uses blend 2.0"
      (is (str/includes? src ", 2.0, ")))
    (testing "cutoff capped at ma.SR / 6.0"
      (is (str/includes? src "ma.SR / 6.0")))
    (testing "two process outputs"
      (is (re-find #"process = n\d+, n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; Level 1 gap ops — :crossover emit
;; ---------------------------------------------------------------------------

(defpatch! crossover-emit-patch {}
  (let [ph (phasor 440.0)
        xo (crossover ph 0.3)
        lp (:out xo)
        hp (:hp xo)]
    (output lp)
    (output hp)))

(deftest crossover-emit-test
  (let [src (emit-faust crossover-emit-patch)]
    (testing "LP emits fi.lowpass"
      (is (str/includes? src "fi.lowpass(")))
    (testing "HP emits fi.highpass"
      (is (str/includes? src "fi.highpass(")))
    (testing "two cascaded LP sections for LR4"
      (is (= 2 (count (re-seq #"fi\.lowpass" src)))))
    (testing "two cascaded HP sections for LR4"
      (is (= 2 (count (re-seq #"fi\.highpass" src)))))
    (testing "two process outputs"
      (is (re-find #"process = n\d+, n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; Level 1 gap ops — :hysteresis emit
;; ---------------------------------------------------------------------------

(defpatch! hysteresis-emit-patch {}
  (let [ph (phasor 440.0)
        hy (hysteresis ph 0.5 0.1)]
    (output hy)))

(deftest hysteresis-emit-test
  (let [src (emit-faust hysteresis-emit-patch)]
    (testing "emits select2 for threshold comparison"
      (is (str/includes? src "select2(")))
    (testing "uses 1-sample feedback state"
      (is (str/includes? src "~ _")))
    (testing "outer select tests in > threshold"
      (is (re-find #"select2\(n\d+ > n\d+" src)))
    (testing "inner select tests in < threshold - width"
      (is (re-find #"select2\(n\d+ < \(n\d+ - n\d+\)" src)))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; Level 1 gap ops — :damping emit
;; ---------------------------------------------------------------------------

(defpatch! damping-emit-patch {}
  (let [ph (phasor 440.0)
        d  (damping ph 0.9)]
    (output d)))

(deftest damping-emit-test
  (let [src (emit-faust damping-emit-patch)]
    (testing "emits 1-sample delayed term (FIR tap 1)"
      (is (re-find #"n\d+'" src)))
    (testing "emits 2-sample delayed term (FIR tap 2)"
      (is (re-find #"n\d+''" src)))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; :segment — morphable slope waveshaper
;; ---------------------------------------------------------------------------

(defpatch! segment-triangle-emit {}
  (let [ph  (phasor 1.0)
        out (segment ph 0.5 0.5)]
    (output out)))

(defpatch! segment-saw-up-emit {}
  (let [ph  (phasor 1.0)
        out (segment ph 1.0 0.5)]
    (output out)))

(defpatch! segment-saw-down-emit {}
  (let [ph  (phasor 1.0)
        out (segment ph 0.0 0.5)]
    (output out)))

(defpatch! segment-curved-emit {}
  (let [ph  (phasor 1.0)
        out (segment ph 0.5 0.0)]
    (output out)))

(deftest segment-emit-test
  (let [src (emit-faust segment-triangle-emit)]
    (testing "emits select2 for asymmetric slope"
      (is (str/includes? src "select2(")))
    (testing "emits pow for curve shaping"
      (is (str/includes? src "pow(")))
    (testing "emits max(0.0,...) to clamp before pow"
      (is (str/includes? src "max(0.0,")))
    (testing "emits max(0.001,...) to guard against zero denominator"
      (is (str/includes? src "max(0.001,")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(deftest segment-shape-variants-test
  (testing "shape=0 (falling-saw) differs from shape=0.5 (triangle)"
    (is (not= (emit-faust segment-triangle-emit)
              (emit-faust segment-saw-down-emit))))
  (testing "shape=1 (rising-saw) differs from shape=0.5 (triangle)"
    (is (not= (emit-faust segment-triangle-emit)
              (emit-faust segment-saw-up-emit)))))

(deftest segment-curve-variants-test
  (testing "curve=0 (concave) differs from curve=0.5 (linear)"
    (is (not= (emit-faust segment-triangle-emit)
              (emit-faust segment-curved-emit)))))

;; ---------------------------------------------------------------------------
;; (faust ...) emit — %inlet-name substitution
;; ---------------------------------------------------------------------------

(defpatch! faust-emit-no-inlets {}
  (let [out (faust "os.osc(440.0)")]
    (output out)))

(deftest faust-emit-no-inlets-test
  (let [src (emit-faust faust-emit-no-inlets)]
    (testing "source string appears verbatim"
      (is (str/includes? src "os.osc(440.0)")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! faust-emit-with-inlet {}
  (let [freq (param :freq)
        out  (faust "os.osc(%freq)" {:freq freq})]
    (output out)))

(deftest faust-emit-inlet-substitution-test
  (let [src (emit-faust faust-emit-with-inlet)]
    (testing "%freq placeholder replaced with a node identifier"
      (is (re-find #"os\.osc\(n\d+\)" src)))
    (testing "no %freq literal remains in emitted output"
      (is (not (str/includes? src "%freq"))))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! faust-emit-prefix-test {}
  (let [dry (phasor 1.0)
        wet (sine-bi dry)
        out (faust "(%in + %in-wet * 0.5)" {:in dry :in-wet wet})]
    (output out)))

(deftest faust-emit-prefix-collision-test
  (let [src (emit-faust faust-emit-prefix-test)]
    (testing "longer %in-wet substituted before %in — no residual placeholder"
      (is (not (str/includes? src "%in"))))
    (testing "resulting expression has two node refs"
      (is (re-find #"\(n\d+ \+ n\d+ \* 0\.5\)" src)))))

;; ---------------------------------------------------------------------------
;; Beat-domain — :beat-phase :beat-bpm :beat-trigger emit
;; ---------------------------------------------------------------------------

(defpatch! beat-phase-emit-patch {}
  (let [ph  (beat-phase)
        out (sine-bi ph)]
    (output out)))

(deftest beat-phase-emit-test
  (let [src (emit-faust beat-phase-emit-patch)]
    (testing "emits hslider with reserved \"beat\" label"
      (is (str/includes? src "hslider(\"beat\",")))
    (testing "range is [0.0, 1.0]"
      (is (str/includes? src "0.0, 0.0, 1.0,")))
    (testing "beat node defined before sine (topo order)"
      (let [lines (str/split-lines src)
            idx   (fn [re] (first (keep-indexed #(when (re-find re %2) %1) lines)))]
        (is (< (idx #"hslider.*beat") (idx #"sin\(")))))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! beat-bpm-emit-patch {}
  (let [bpm (beat-bpm)
        hz  (div bpm 60.0)
        out (phasor hz)]
    (output out)))

(deftest beat-bpm-emit-test
  (let [src (emit-faust beat-bpm-emit-patch)]
    (testing "emits hslider with reserved \"bpm\" label"
      (is (str/includes? src "hslider(\"bpm\",")))
    (testing "default value is 120.0"
      (is (str/includes? src "120.0")))
    (testing "range is [20.0, 300.0]"
      (is (str/includes? src "20.0"))
      (is (str/includes? src "300.0")))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

(defpatch! beat-trigger-emit-patch {}
  (let [ph   (beat-phase)
        trig (beat-trigger ph)
        out  (vca trig 0.5)]
    (output out)))

(deftest beat-trigger-emit-test
  (let [src (emit-faust beat-trigger-emit-patch)]
    (testing "emits float() predicate"
      (is (re-find #"float\(" src)))
    (testing "uses 1-sample delay operator"
      (is (re-find #"n\d+'" src)))
    (testing "detects backward phase jump via > 0.5"
      (is (str/includes? src "> 0.5")))
    (testing "beat-phase node defined before beat-trigger (topo order)"
      (let [lines (str/split-lines src)
            idx   (fn [re] (first (keep-indexed #(when (re-find re %2) %1) lines)))]
        (is (< (idx #"hslider.*beat") (idx #"float\(")))))
    (testing "single process output"
      (is (re-find #"process = n\d+;" src)))))

;; ---------------------------------------------------------------------------
;; :audio-in — process-level audio input
;; ---------------------------------------------------------------------------

(defpatch! audio-in-filter-patch {}
  (let [in  (audio-in)
        out (svf in 0.3 0.5 0.0)]
    (output out)))

(defpatch! audio-in-stereo-patch {}
  (let [l (audio-in)
        r (audio-in)]
    (output l)
    (output r)))

(deftest audio-in-emit-test
  (let [src (emit-faust audio-in-filter-patch)]
    (testing "audio-in node emitted as alembic_dsp function parameter"
      (is (re-find #"alembic_dsp\(n\d+\) =" src)))
    (testing "svf references the audio-in node"
      (is (str/includes? src "fi.svf_morph(")))
    (testing "process delegates to alembic_dsp"
      (is (str/includes? src "process = alembic_dsp;")))
    (testing "no bare n = _; definition emitted"
      (is (not (re-find #"n\d+ = _;" src))))))

(deftest audio-in-stereo-emit-test
  (let [src (emit-faust audio-in-stereo-patch)]
    (testing "two audio-in nodes emitted as alembic_dsp parameters"
      (is (re-find #"alembic_dsp\(n\d+, n\d+\) =" src)))
    (testing "process delegates to alembic_dsp"
      (is (str/includes? src "process = alembic_dsp;")))))

(deftest audio-in-validates-test
  (testing "audio-in filter patch produces valid Faust"
    (is (nil? (alembic.compile/validate audio-in-filter-patch)))))

;; ---------------------------------------------------------------------------
;; Utility ops — :abs :min :max :track-hold
;; ---------------------------------------------------------------------------

(defpatch! abs-emit-patch {}
  (let [in  (audio-in)
        out (abs in)]
    (output out)))

(deftest abs-emit-test
  (let [src (emit-faust abs-emit-patch)]
    (testing "emits Faust abs() call"
      (is (re-find #"abs\(n\d+\)" src)))
    (testing "process delegates to alembic_dsp (audio-in present)"
      (is (str/includes? src "process = alembic_dsp;")))))

(defpatch! min-max-emit-patch {}
  (let [a (phasor 440.0)
        b (phasor 110.0)]
    (output (min a b))
    (output (max a b))))

(deftest min-max-emit-test
  (let [src (emit-faust min-max-emit-patch)]
    (testing "emits Faust min() call"
      (is (re-find #"min\(n\d+, n\d+\)" src)))
    (testing "emits Faust max() call"
      (is (re-find #"max\(n\d+, n\d+\)" src)))
    (testing "two process outputs"
      (is (re-find #"process = n\d+, n\d+;" src)))))

(defpatch! track-hold-emit-patch {}
  (let [in   (audio-in)
        gate (param :gate)
        out  (track-hold in gate)]
    (output out)))

(deftest track-hold-emit-test
  (let [src (emit-faust track-hold-emit-patch)]
    (testing "emits select2 for level-triggered hold"
      (is (str/includes? src "select2(")))
    (testing "threshold is > 0.5"
      (is (str/includes? src "> 0.5")))
    (testing "uses 1-sample feedback loop"
      (is (str/includes? src "~ _")))
    (testing "process delegates to alembic_dsp"
      (is (str/includes? src "process = alembic_dsp;")))))

(deftest kinks-validates-test
  (testing "abs of audio-in produces valid Faust"
    (is (nil? (alembic.compile/validate abs-emit-patch))))
  (testing "min/max of phasors produces valid Faust"
    (is (nil? (alembic.compile/validate min-max-emit-patch))))
  (testing "track-hold produces valid Faust"
    (is (nil? (alembic.compile/validate track-hold-emit-patch)))))

;; ---------------------------------------------------------------------------
;; Ears — envelope follower composition: abs + slew + comparator + delta
;; ---------------------------------------------------------------------------

(defpatch! ears-emit-patch
  {:params {:gain             {:range [0.0 4.0]   :default 1.0}
            :attack           {:range [0.001 0.5]  :default 0.01}
            :release          {:range [0.01 2.0]   :default 0.1}
            :threshold        {:range [0.0 1.0]    :default 0.3}
            :hyst-width       {:range [0.0 0.2]    :default 0.05}
            :transient-thresh {:range [0.0 0.2]    :default 0.02}}}
  (let [in        (audio-in)
        gained    (vca in (param :gain))
        env       (slew (abs gained) (param :attack) (param :release))
        gate      (hysteresis env (param :threshold) (param :hyst-width))
        onset-cmp (comparator (delta env) (param :transient-thresh))
        onset     (:out onset-cmp)]
    (output env)
    (output gate)
    (output onset)))

(deftest ears-emit-test
  (let [src (emit-faust ears-emit-patch)]
    (testing "contains abs() for rectification"
      (is (re-find #"abs\(n\d+\)" src)))
    (testing "contains slew select2/exp pattern for envelope follower"
      (is (str/includes? src "select2("))
      (is (str/includes? src "exp("))
      (is (str/includes? src "ma.SR")))
    (testing "contains hysteresis select2/feedback for gate (slew + 2 nested in hysteresis)"
      (is (= 3 (count (re-seq #"select2\(" src)))))
    (testing "contains delta (n - n') for onset detection"
      (is (re-find #"n\d+ - n\d+'" src)))
    (testing "three process outputs via alembic_dsp"
      (is (re-find #"alembic_dsp\(n\d+\) = n\d+, n\d+, n\d+" src)))))

(deftest ears-validates-test
  (testing "full ears envelope follower patch produces valid Faust"
    (is (nil? (alembic.compile/validate ears-emit-patch)))))

;; ---------------------------------------------------------------------------
;; Ripples — dual audio-in, three simultaneous filter outputs
;; ---------------------------------------------------------------------------

(defpatch! ripples-emit-patch
  {:params {:freq   {:range [0.0 1.0]  :default 0.5}
            :res    {:range [0.0 1.0]  :default 0.0}
            :fm-amt {:range [-1.0 1.0] :default 0.0}
            :drive  {:range [0.0 1.0]  :default 0.0}}}
  (let [in     (audio-in)
        fm-cv  (audio-in)
        driven (wave-fold (mul in (add 1.0 (mul (param :drive) 7.0))))
        cutoff (clip (add (param :freq) (mul fm-cv (param :fm-amt))) 0.0 1.0)
        lp4    (ladder driven cutoff (param :res))
        lp2    (svf driven cutoff (param :res) 0.0)
        bp     (svf driven cutoff (param :res) 0.5)]
    (output lp4)
    (output lp2)
    (output bp)))

(deftest ripples-emit-test
  (let [src (emit-faust ripples-emit-patch)]
    (testing "two audio-in nodes as alembic_dsp parameters"
      (is (re-find #"alembic_dsp\(n\d+, n\d+\) =" src)))
    (testing "LP4 emits ve.moogLadder"
      (is (str/includes? src "ve.moogLadder(")))
    (testing "LP2 and BP emit fi.svf_morph (two instances)"
      (is (= 2 (count (re-seq #"fi\.svf_morph" src)))))
    (testing "LP2 blend=0.0 — const node 0.0 defined, mode scaled via * 2.0"
      (is (and (re-find #"n\d+ = 0\.0;" src)
               (re-find #"\* 2\.0\)" src))))
    (testing "BP blend=0.5 — const node 0.5 defined (× 2.0 = 1.0 at runtime)"
      (is (re-find #"n\d+ = 0\.5;" src)))
    (testing "three process outputs"
      (is (re-find #"alembic_dsp\(n\d+, n\d+\) = n\d+, n\d+, n\d+" src)))
    (testing "wave-fold drive stage present"
      (is (str/includes? src "floor(")))
    (testing "FM clip uses max/min"
      (is (str/includes? src "max(n")))))

(deftest ripples-validates-test
  (testing "ripples patch produces valid Faust"
    (is (nil? (alembic.compile/validate ripples-emit-patch)))))

;; ---------------------------------------------------------------------------
;; :sqrt — square root op
;; ---------------------------------------------------------------------------

(defpatch! sqrt-emit-patch {}
  (let [in  (audio-in)
        out (sqrt (abs in))]
    (output out)))

(deftest sqrt-emit-test
  (let [src (emit-faust sqrt-emit-patch)]
    (testing "sqrt emits Faust sqrt()"
      (is (str/includes? src "sqrt(")))
    (testing "abs feeds into sqrt"
      (is (re-find #"sqrt\(n\d+\)" src)))
    (testing "abs is also present"
      (is (str/includes? src "abs(")))))

(deftest sqrt-validates-test
  (testing "sqrt patch produces valid Faust"
    (is (nil? (alembic.compile/validate sqrt-emit-patch)))))

;; ---------------------------------------------------------------------------
;; Dead secondary port pruning
;; ---------------------------------------------------------------------------

(defpatch! comparator-pruned-patch {}
  (let [in    (audio-in)
        cmp   (comparator in 0.0)
        gate  (:out cmp)]
    (output gate)))

(deftest dead-secondary-port-pruned-test
  (let [src (emit-faust comparator-pruned-patch)]
    (testing "primary comparator output is present"
      (is (str/includes? src "float(")))
    (testing "unused comparator-inv secondary port is pruned from emitted Faust"
      (is (not (re-find #"1\.0 - n\d+" src))))))

;; ---------------------------------------------------------------------------
;; ks-string — KS loop Faust emit
;; ---------------------------------------------------------------------------

(deftest ks-string-emit-test
  (let [src (emit-faust examples.ks-string/ks-string)]
    (testing "de.delay present (pitch-setting delay line)"
      (is (str/includes? src "de.delay(")))
    (testing "@(1) present (1-sample lag in averaging FIR)"
      (is (str/includes? src "@(1)")))
    (testing "~ _ present (Faust feedback combinator)"
      (is (str/includes? src "~ _")))
    (testing "exciter: no.noise present"
      (is (str/includes? src "no.noise")))
    (testing "exciter: en.ar present"
      (is (str/includes? src "en.ar(")))
    (testing "fan-out/fan-in damping pattern present"
      (is (str/includes? src "<:")))))

(deftest ks-string-validates-test
  (testing "ks-string patch produces valid Faust"
    (is (nil? (alembic.compile/validate examples.ks-string/ks-string)))))

;; ---------------------------------------------------------------------------
;; :shelf-lo :shelf-hi :peak-eq emit
;; ---------------------------------------------------------------------------

(defpatch! shelf-lo-emit-patch {}
  (let [in  (audio-in)
        out (shelf-lo in 0.1 6.0)]
    (output out)))

(defpatch! shelf-hi-emit-patch {}
  (let [in  (audio-in)
        out (shelf-hi in 0.8 -6.0)]
    (output out)))

(defpatch! peak-eq-emit-patch {}
  (let [in  (audio-in)
        out (peak-eq in 0.3 3.0 0.5)]
    (output out)))

(deftest shelf-lo-emit-test
  (let [src (emit-faust shelf-lo-emit-patch)]
    (testing "fi.low_shelf present"
      (is (str/includes? src "fi.low_shelf(")))
    (testing "freq scaled to Hz (ma.SR * 0.5)"
      (is (str/includes? src "ma.SR * 0.5")))))

(deftest shelf-hi-emit-test
  (let [src (emit-faust shelf-hi-emit-patch)]
    (testing "fi.high_shelf present"
      (is (str/includes? src "fi.high_shelf(")))))

(deftest peak-eq-emit-test
  (let [src (emit-faust peak-eq-emit-patch)]
    (testing "fi.peak_eq present"
      (is (str/includes? src "fi.peak_eq(")))
    (testing "Q mapped as 0.5 + q * 9.5"
      (is (str/includes? src "0.5 + ")))))

(deftest shelves-emit-test
  (let [src (emit-faust examples.shelves/shelves)]
    (testing "low shelf present"
      (is (str/includes? src "fi.low_shelf(")))
    (testing "high shelf present"
      (is (str/includes? src "fi.high_shelf(")))
    (testing "two peak-eq sections present"
      (is (= 2 (count (re-seq #"fi\.peak_eq\(" src)))))))

(deftest shelves-validates-test
  (testing "shelves patch produces valid Faust"
    (is (nil? (alembic.compile/validate examples.shelves/shelves)))))
