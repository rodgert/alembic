; SPDX-License-Identifier: EPL-2.0
(ns examples.ripples
  "Ripples-inspired resonant 4-pole filter with three simultaneous outputs.

  Signal flow:
    in → drive/fold → ──────────────────────────────── ladder(cutoff, res) → LP4
                       └─ svf(cutoff, res, 0.0) → LP2
                       └─ svf(cutoff, res, 0.5) → BP

    fm-cv × fm-amt + freq → clip [0,1] → cutoff

  Three simultaneous outputs:
    LP4 — 24 dB/oct Moog ladder lowpass (the signature Ripples voice)
    LP2 — 12 dB/oct lowpass; here via ZDF SVF (mode=0), not a ladder tap
    BP  — bandpass; ZDF SVF at mode=0.5

  Approximation note: ve.moogLadder exposes only the final 4-pole output.
  The actual Ripples hardware derives LP2 from the ladder's 2nd intermediate
  stage.  Using a separate SVF instance gives a different (but useful) 2-pole
  character.  A precise LP2 would require (faust ...) with a custom multi-tap
  ladder expression.

  FM: the fm-cv input (second audio-in) is scaled by the bipolar :fm-amt
  param [-1, 1] and added to the base cutoff, giving classic pitch-tracking
  and audio-rate FM modulation.  Result is clipped to [0, 1] so the cutoff
  stays in the normalised range.

  Drive: same wave-fold stage as Blades.  At drive=0 the signal passes
  through unaltered; at drive=1 the fold adds strong odd harmonics before
  the filters.

  Compile target: sample-rate CLAP plugin (two audio-in nodes → :sample)."
  (:require [alembic.patch :refer [defpatch!]]))

(defpatch! ripples
  {:params {:freq   {:range [0.0 1.0]  :default 0.5 :unit :hz}
            :res    {:range [0.0 1.0]  :default 0.0}
            :fm-amt {:range [-1.0 1.0] :default 0.0}
            :drive  {:range [0.0 1.0]  :default 0.0}}}
  (let [in    (audio-in)   ; main signal
        fm-cv (audio-in)   ; FM CV — accepts audio-rate or block-rate CV

        ;; Pre-filter drive: scale into wavefolder for harmonic saturation.
        ;; drive=0 → scale=1 (clean); drive=1 → scale=8 (heavy fold).
        driven (wave-fold (mul in (add 1.0 (mul (param :drive) 7.0))))

        ;; Cutoff: base frequency + scaled FM input, clipped to [0, 1].
        ;; fm-amt is bipolar: positive → cutoff rises with fm-cv,
        ;;                    negative → cutoff falls with fm-cv.
        cutoff (clip (add (param :freq)
                          (mul fm-cv (param :fm-amt)))
                     0.0 1.0)

        ;; LP4: Moog ZDF ladder — 24 dB/oct, classic resonant character.
        ;; res [0, 1] → Q [0.707, 25]; self-oscillates at res ≈ 1.
        lp4 (ladder driven cutoff (param :res))

        ;; LP2: ZDF SVF at LP mode — 12 dB/oct, softer knee than the ladder.
        lp2 (svf driven cutoff (param :res) 0.0)

        ;; BP: ZDF SVF at BP mode (blend=0.5).
        bp  (svf driven cutoff (param :res) 0.5)]

    (output lp4)
    (output lp2)
    (output bp)))
