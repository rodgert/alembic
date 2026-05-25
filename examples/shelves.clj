; SPDX-License-Identifier: EPL-2.0
(ns examples.shelves
  "MI Shelves-inspired 4-band parametric EQ with full CV control.

  Signal flow:
    in → low shelf → low-mid peak → high-mid peak → high shelf → out

  Four series filter sections:
    shelf-lo  — RBJ 2nd-order low-shelving EQ
    peak-eq   — RBJ 2nd-order peaking (bell) EQ  ×2
    shelf-hi  — RBJ 2nd-order high-shelving EQ

  Gain params are in dB (range ±18 dB); zero gain at default → unity pass-through.
  Frequency params are normalised [0, 1] → Hz (0 = DC, 1 = Nyquist).
  Q params normalise [0, 1] → Q [0.5, 10]; q=0 ≈ 2-octave width, q=1 ≈ 0.1 octave.

  Approximation note: The actual MI Shelves uses op-amp–based shelf sections
  whose transfer functions differ slightly from the RBJ biquad formulae.
  This example prioritises a clean alembic vocabulary exercise over hardware
  fidelity.

  Compile target: sample-rate CLAP plugin (audio-in present → :sample)."
  (:require [alembic.patch :refer [defpatch!]]))

(defpatch! shelves
  {:params {:lo-freq  {:range [0.0 1.0]   :default 0.01  :unit :hz}
            :lo-gain  {:range [-18.0 18.0] :default 0.0   :unit :db}
            :lm-freq  {:range [0.0 1.0]   :default 0.02  :unit :hz}
            :lm-gain  {:range [-18.0 18.0] :default 0.0   :unit :db}
            :lm-q     {:range [0.0 1.0]   :default 0.3}
            :hm-freq  {:range [0.0 1.0]   :default 0.15  :unit :hz}
            :hm-gain  {:range [-18.0 18.0] :default 0.0   :unit :db}
            :hm-q     {:range [0.0 1.0]   :default 0.3}
            :hi-freq  {:range [0.0 1.0]   :default 0.4   :unit :hz}
            :hi-gain  {:range [-18.0 18.0] :default 0.0   :unit :db}}}
  (let [in (audio-in)

        ;; Low shelf: boost/cut below lo-freq corner (~240 Hz @ 48 kHz at default)
        lo (shelf-lo in (param :lo-freq) (param :lo-gain))

        ;; Low-mid peak: bell EQ centred at lm-freq (~480 Hz at default)
        lm (peak-eq lo (param :lm-freq) (param :lm-gain) (param :lm-q))

        ;; High-mid peak: bell EQ centred at hm-freq (~3.6 kHz at default)
        hm (peak-eq lm (param :hm-freq) (param :hm-gain) (param :hm-q))

        ;; High shelf: boost/cut above hi-freq corner (~9.6 kHz at default)
        hi (shelf-hi hm (param :hi-freq) (param :hi-gain))]

    (output hi)))
