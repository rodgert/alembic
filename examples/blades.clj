; SPDX-License-Identifier: EPL-2.0
(ns examples.blades
  "Blades-inspired dual multimode filter.

  Signal flow:
    input → drive/fold → filter A ──────────── crossfade → filter B → level → output
                             │                     ↑
                             └── cross-FM ─────────┘ (A's output → B's cutoff)

  Topology: two ZDF SVF sections (A and B) with voltage-controlled routing
  between series and parallel configurations, cross-FM from filter A into
  filter B's cutoff, a pre-filter wavefold drive stage, and an output level VCA.

  Drive: input is scaled by (1 + drive*7) then triangle-folded back to [-1,1].
    drive=0 → unity gain, clean passthrough.
    drive=0.14 → 2× gain, gentle second-harmonic character.
    drive=1.0 → 8× gain, heavy wavefold distortion.

  Routing (series/parallel crossfade):
    routing=0 → series:   input → A → B → output
    routing=1 → parallel: input → A ─┐
                          driven → B ─┴─ mix → output

  Cross-FM: filter A's output is scaled by :fm-amt and added to filter B's
  normalised cutoff [0,1], giving frequency-shifting and vowel-formant
  interactions.  At fm-amt=0 the filters are independent.

  Cutoff params are normalised [0,1] → [0, Nyquist] by the :svf emit.
  Mode [0,1]: 0=LP, 0.5=BP, 1=HP (continuously variable)."
  (:require [alembic.patch :refer [defpatch!]]))

(defpatch! blades
  {:params {:drive    {:range [0.0 1.0] :default 0.0}
            :freq-a   {:range [0.0 1.0] :default 0.5  :unit :hz}
            :res-a    {:range [0.0 1.0] :default 0.0}
            :mode-a   {:range [0.0 1.0] :default 0.0}
            :freq-b   {:range [0.0 1.0] :default 0.5  :unit :hz}
            :res-b    {:range [0.0 1.0] :default 0.0}
            :mode-b   {:range [0.0 1.0] :default 0.0}
            :routing  {:range [0.0 1.0] :default 0.0}
            :fm-amt   {:range [0.0 1.0] :default 0.0}
            :level    {:range [0.0 1.0] :default 1.0}}}
  (let [in      (audio-in)

        ;; Drive stage: scale into wavefolder for pre-filter harmonic saturation.
        ;; drive=0 → scale=1 (signal stays inside fold threshold, clean).
        ;; drive=1 → scale=8 (heavy triangle-fold distortion).
        driven  (wave-fold (mul in (add 1.0 (mul (param :drive) 7.0))))

        ;; Filter A — processes the driven signal
        filt-a  (svf driven
                     (param :freq-a)
                     (param :res-a)
                     (param :mode-a))

        ;; Cross-FM: scale filter A's output, add to filter B's cutoff.
        ;; Clipped to [0,1] so B's cutoff stays in the normalised range.
        cutoff-b (clip (add (param :freq-b)
                            (mul filt-a (param :fm-amt)))
                       0.0 1.0)

        ;; Filter B input: series (routing=0) uses filter A output;
        ;; parallel (routing=1) uses the driven input directly.
        b-input (crossfade filt-a driven (param :routing))

        ;; Filter B
        filt-b  (svf b-input
                     cutoff-b
                     (param :res-b)
                     (param :mode-b))]

    ;; Output VCA — master level control
    (output (vca filt-b (param :level)))))
