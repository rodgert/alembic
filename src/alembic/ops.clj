; SPDX-License-Identifier: EPL-2.0
(ns alembic.ops)

(def inlet-names
  "Maps op keyword → ordered vector of inlet names.
  Order must match the positional argument order in the authoring syntax."
  {:phasor      [:freq]
   :sine-bi     [:input]
   :sine-uni    [:input]
   :tri         [:input]
   :rect        [:input :width]
   :mul         [:a :b]
   :add         [:a :b]
   :sub         [:a :b]
   :div         [:a :b]
   :history     [:input]
   :delay       [:input :time]
   :sah         [:input :trigger]
   :delta       [:input]
   :wrap        [:input :lo :hi]
   :fold        [:input :lo :hi]
   :clip        [:input :lo :hi]
   :smooth      [:input]
   :ar-env      [:gate :attack :release]
   ;; Level 1 filters
   ;; :ladder — ZDF 4-pole Moog ladder (ve.moogLadder, Zavalishin model)
   ;;   cutoff    normalised frequency [0, 1]  (0.5 ≈ Nyquist)
   ;;   resonance normalised Q         [0, 1]  (1 → self-oscillation / sine)
   :ladder      [:input :cutoff :resonance]
   ;; :svf — ZDF state variable filter (fi.svf_morph, Oleg Nesterov / Vital model)
   ;;   cutoff    normalised frequency [0, 1]  (maps linearly to [0, Nyquist])
   ;;   resonance normalised Q         [0, 1]  (maps to Q [0.5, 10])
   ;;   mode      normalised blend     [0, 1]  (0=LP  0.5=BP  1=HP, continuous morph)
   :svf         [:input :cutoff :resonance :mode]
   ;; :one-pole — ZDF first-order LP (si.smooth, single cutoff parameter)
   ;;   cutoff  normalised [0, 1]  (0 = DC-only / max smooth; 1 = pass-through)
   :one-pole    [:in :cutoff]
   ;; :dc-block — 1-pole highpass that removes DC offset (fi.dcblocker)
   :dc-block    [:in]
   ;; :allpass — allpass delay section for reverb diffusion / phasing (de.apf)
   ;;   time   delay time in seconds
   ;;   coeff  feedback/feedforward coefficient [−1, 1]; |coeff| < 1 → flat amplitude
   :allpass     [:in :time :coeff]
   ;; :vca — voltage-controlled amplifier; linear multiply
   :vca         [:in :level]
   ;; :slew — asymmetric first-order lag (rise/fall in seconds)
   ;;   rise  time constant for signal increasing
   ;;   fall  time constant for signal decreasing
   :slew        [:in :rise :fall]
   ;; :sample-hold — sample and hold; rising edge of :trigger captures :in
   :sample-hold [:in :trigger]
   ;; :comparator — threshold detector; output 1.0 when :in > :threshold, else 0.0
   :comparator  [:in :threshold]
   ;; :noise — white noise generator (no.noise); no signal inputs
   :noise       []
   ;; :pink-noise — 1/f pink noise generator (no.pink_noise); no signal inputs
   :pink-noise  []
   ;; :crossfade — linear crossfade; :pos [0,1] blends a → b
   :crossfade   [:a :b :pos]
   ;; :ring-mod — ring modulator; :dc [0,1] shifts ring-mod → AM
   :ring-mod    [:carrier :modulator :dc]
   ;; :bitcrusher — bit-depth quantisation; :bits [1,24] effective bit depth
   :bitcrusher  [:in :bits]
   ;; :soft-clip — symmetric soft saturation via Padé tanh approximation x/(1+|x|)
   :soft-clip   [:in]
   ;; :hard-clip — cubic polynomial soft-then-hard overdrive, clips at ±1
   :hard-clip   [:in]
   ;; :wave-fold — triangle wavefolder; folds input > 1 or < −1 back into [−1, 1]
   :wave-fold   [:in]
   ;; ---- ops that accept a compile-time options map before signal args ----
   ;; :vco — anti-aliased oscillator
   ;;   opts: {:shape :saw|:sine|:square|:triangle|:pulse  :pw 0.5  :sync false}
   :vco         [:freq]
   ;; :counter — clocked integer counter; carry output requires multi-output extension
   ;;   opts: {:max 16 :dir :up|:down :wrap true|false}
   :counter     [:clock :reset]
   ;; :table — read-only lookup table with interpolation
   ;;   opts: {:data [floats] :size N :mode :wrap|:clamp|:fold}
   :table       [:index]
   ;; :select — N-to-1 signal multiplexer via ba.selectn
   ;;   opts: {:n 2}  (n >= 2; default 2)
   ;;   inlets: :in-0 through :in-(n-1), then :index
   :select      (fn [{:keys [n] :or {n 2}}]
                  (when (< n 2)
                    (throw (ex-info ":select :n must be >= 2" {:n n})))
                  (into (mapv #(keyword (str "in-" %)) (range n)) [:index]))})

(def node-rate
  "Maps op keyword → rate keyword (:sample | :block | :beat)."
  {:phasor      :sample
   :sine-bi     :sample
   :sine-uni    :sample
   :tri         :sample
   :rect        :sample
   :mul         :sample
   :add         :sample
   :sub         :sample
   :div         :sample
   :history     :sample
   :delay       :sample
   :sah         :sample
   :delta       :sample
   :wrap        :sample
   :fold        :sample
   :clip        :sample
   :smooth      :sample
   :ar-env      :sample
   :ladder      :sample
   :svf         :sample
   :one-pole    :sample
   :dc-block    :sample
   :allpass     :sample
   :vca         :sample
   :slew        :sample
   :sample-hold :sample
   :comparator  :sample
   :noise       :sample
   :pink-noise  :sample
   :crossfade   :sample
   :ring-mod    :sample
   :bitcrusher  :sample
   :soft-clip   :sample
   :hard-clip   :sample
   :wave-fold   :sample
   :vco         :sample
   :counter     :sample
   :table       :sample
   :select      :sample
   :param       :block
   :const       :sample})

(def port-node-specs
  "Secondary port nodes created automatically for multi-output ops.
  Maps op-kw → {port-name {:op port-op-kw :inlets [inlet-kws]}}.
  :source always refers to the primary node id; other inlet names are
  resolved from the primary node's own inputs map by the same key."
  {:counter    {:carry    {:op :counter-carry  :inlets [:source :clock]}}
   :comparator {:inv-gate {:op :comparator-inv :inlets [:source]}}})
