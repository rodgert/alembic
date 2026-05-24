; SPDX-License-Identifier: EPL-2.0
(ns alembic.ops)

(def inlet-names
  "Maps op keyword → ordered vector of inlet names.
  Order must match the positional argument order in the authoring syntax."
  {:phasor   [:freq]
   :sine-bi  [:input]
   :sine-uni [:input]
   :tri      [:input]
   :rect     [:input :width]
   :mul      [:a :b]
   :add      [:a :b]
   :sub      [:a :b]
   :div      [:a :b]
   :history  [:input]
   :delay    [:input :time]
   :sah      [:input :trigger]
   :delta    [:input]
   :wrap     [:input :lo :hi]
   :fold     [:input :lo :hi]
   :clip     [:input :lo :hi]
   :smooth   [:input]
   :ar-env   [:gate :attack :release]
   ;; Level 1 filters
   ;; :ladder — ZDF 4-pole Moog ladder (ve.moogLadder, Zavalishin model)
   ;;   cutoff    normalised frequency [0, 1]  (0.5 ≈ Nyquist)
   ;;   resonance normalised Q         [0, 1]  (1 → self-oscillation / sine)
   :ladder   [:input :cutoff :resonance]})

(def node-rate
  "Maps op keyword → rate keyword (:sample | :block | :beat)."
  {:phasor   :sample
   :sine-bi  :sample
   :sine-uni :sample
   :tri      :sample
   :rect     :sample
   :mul      :sample
   :add      :sample
   :sub      :sample
   :div      :sample
   :history  :sample
   :delay    :sample
   :sah      :sample
   :delta    :sample
   :wrap     :sample
   :fold     :sample
   :clip     :sample
   :smooth   :sample
   :ar-env   :sample
   :ladder   :sample
   :param    :block
   :const    :sample})
