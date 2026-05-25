; SPDX-License-Identifier: EPL-2.0
(ns examples.blades
  "Blades-inspired dual multimode filter.

  Topology: two SVF sections (A and B) with voltage-controlled routing
  between series and parallel configurations, plus cross-FM from filter A
  into filter B's cutoff.

  Series  (routing=0): input → filter A → filter B → output
  Parallel (routing=1): input → filter A ─┐
                        input → filter B ─┴─ mix → output

  Cross-FM: filter A's output is scaled by :fm-amt and added to filter B's
  cutoff, giving classic frequency-shifting / vowel-formant interactions.

  All cutoff params are normalised [0,1] → [0, Nyquist] by the :svf emit.
  Mode [0,1]: 0=LP, 0.5=BP, 1=HP (continuously variable)."
  (:require [alembic.patch :refer [defpatch!]]))

(defpatch! blades
  {:params {:freq-a   {:range [0.0 1.0] :default 0.5  :unit :hz}
            :res-a    {:range [0.0 1.0] :default 0.0}
            :mode-a   {:range [0.0 1.0] :default 0.0}
            :freq-b   {:range [0.0 1.0] :default 0.5  :unit :hz}
            :res-b    {:range [0.0 1.0] :default 0.0}
            :mode-b   {:range [0.0 1.0] :default 0.0}
            :routing  {:range [0.0 1.0] :default 0.0}
            :fm-amt   {:range [0.0 1.0] :default 0.0}}}
  (let [in      (audio-in)

        ;; Filter A — processes the raw input
        filt-a  (svf in
                     (param :freq-a)
                     (param :res-a)
                     (param :mode-a))

        ;; Cross-FM: scale filter A's output, add to filter B's cutoff
        cutoff-b (add (param :freq-b)
                      (mul filt-a (param :fm-amt)))

        ;; Filter B input: crossfade between raw input (parallel) and
        ;; filter A output (series). routing=0 → series, routing=1 → parallel.
        b-input (crossfade filt-a in (param :routing))

        ;; Filter B
        filt-b  (svf b-input
                     cutoff-b
                     (param :res-b)
                     (param :mode-b))]

    (output filt-b)))
