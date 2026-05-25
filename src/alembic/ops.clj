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
   ;; :delay — parameterisable delay line
   ;;   opts: {:max-time 1.0       compile-time buffer allocation in seconds
   ;;          :interp   :linear   :none (integer) | :linear | :cubic
   ;;          :smooth   true      glitch-free time changes via de.sdelay crossfade
   ;;          :time-cv  false     when true, adds :time-cv audio-rate inlet instead of :time}
   :delay       (fn [{:keys [time-cv] :or {time-cv false}}]
                  (if time-cv
                    [:in :time-cv]
                    [:in :time]))
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
   ;; :abs — absolute value; full-wave rectify; rate follows input
   :abs         [:in]
   ;; :sqrt — square root; rate follows input; undefined for negative values
   ;;   Use for: RMS envelope (sqrt(slew(mul x x))), equal-power crossfade,
   ;;            octave-to-Hz (mul 440.0 (pow 2.0 (sub cv 69.0/12.0))).
   :sqrt        [:in]
   ;; :min — signal minimum of two inputs; AND gate on boolean signals
   :min         [:a :b]
   ;; :max — signal maximum of two inputs; OR gate on boolean signals
   ;;   half-wave rectify: (max x 0.0) passes positive half, zeros negative
   :max         [:a :b]
   ;; :track-hold — track-and-hold; follows :in while :gate > 0.5, holds on falling edge
   ;;   Complements :sample-hold (edge-triggered) with a level-triggered variant.
   :track-hold  [:in :gate]
   ;; :naive-svf — Chamberlin SVF; LP+HP simultaneous; cutoff capped at Nyquist/6
   ;;   cutoff    normalised [0, 1]; stability limit enforced at emit time
   ;;   resonance normalised [0, 1]
   ;;   Returns multi-output port-map: {:out lp-id :hp hp-id}
   :naive-svf   [:in :cutoff :resonance]
   ;; :crossover — 4th-order Linkwitz-Riley; LP+HP sum = flat amplitude response
   ;;   cutoff    normalised [0, 1]
   ;;   Returns multi-output port-map: {:out lp-id :hp hp-id}
   :crossover   [:in :cutoff]
   ;; :hysteresis — Schmitt trigger with deadband; suppresses CV jitter without lag
   ;;   threshold  detection level; output goes high when :in crosses above
   ;;   width      dead-band below threshold; output clears only when in < threshold-width
   :hysteresis  [:in :threshold :width]
   ;; :damping — 3-tap FIR LP; brightness-parameterised; used in Rings KS loop
   ;;   coeff brightness coefficient [0, 1]; 1 = pass-through; 0 = fully damped
   :damping     [:in :coeff]
   ;; :segment — morphable slope waveshaper (Level 1 phasor-based LFO primitive)
   ;;   phase [0,1)  from os.phasor; output is [0,1] unipolar
   ;;   shape [0,1]  rise/fall ratio: 0=falling-saw, 0.5=symmetric triangle, 1=rising-saw
   ;;   curve [0,1]  curvature: 0=concave/exp-in (x^4), 0.5=linear (x^1), 1=convex/exp-out (x^0.25)
   ;;   Bipolar: (sub (mul seg 2.0) 1.0) → [-1, 1]
   :segment     [:phase :shape :curve]
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
   ;; :buffer — read-write circular buffer (rwtable)
   ;;   opts: {:size 48000}  compile-time allocation in samples
   ;;   write-pos and read-pos are float signals; int() is applied at emit time.
   ;;   write-gate and write-mode are deferred (caller manages via write-pos).
   :buffer      [:in :write-pos :read-pos]
   ;; :select — N-to-1 signal multiplexer via ba.selectn
   ;;   opts: {:n 2}  (n >= 2; default 2)
   ;;   inlets: :in-0 through :in-(n-1), then :index
   :select      (fn [{:keys [n] :or {n 2}}]
                  (when (< n 2)
                    (throw (ex-info ":select :n must be >= 2" {:n n})))
                  (into (mapv #(keyword (str "in-" %)) (range n)) [:index]))
   ;; ---- audio input ----
   ;; :audio-in — process-level audio input signal; maps to Faust's _ wire.
   ;;   No inlets. Each :audio-in node in a patch is a distinct process input.
   ;;   The CLAP plugin host delivers audio to these in channel order (first
   ;;   :audio-in node = channel 0, second = channel 1, etc.).
   :audio-in    []
   ;; ---- beat-domain ops ----
   ;; :beat-phase — host fractional beat position [0,1); rate :beat
   ;;   No inlets — populated by faust_modulator via reserved hslider("beat").
   ;;   Semantically coarser than :block; use to drive beat-sync modulators.
   ;;   Use (beat-trigger (beat-phase)) to obtain a sample-rate trigger at each boundary.
   :beat-phase  []
   ;; :beat-bpm — host tempo in BPM; rate :block (host updates at block rate)
   ;;   No inlets — populated by faust_modulator via reserved hslider("bpm").
   ;;   Convert to beat period: (div 60.0 (beat-bpm))
   ;;   Convert to Hz:          (div (beat-bpm) 60.0)
   :beat-bpm    []
   ;; :beat-trigger — 1-sample pulse on phase wrap (beat boundary); rate :sample
   ;;   phase — [0,1) phase signal, typically from :beat-phase
   ;;   Detects the discrete backward jump (phase' - phase) > 0.5 that occurs when
   ;;   the sawtooth wraps from ~1 → 0. Use as a clock input to :counter or :sah.
   :beat-trigger [:phase]})

(def node-rate
  "Maps op keyword → rate keyword (:sample | :block | :beat | :polymorphic).
  :polymorphic ops derive their rate from max(input rates) at walk time.
  Only stateless pure-function ops are polymorphic; any op that uses a
  1-sample delay ('operator), IIR state, or sample-indexed memory stays :sample."
  {;; ---- signal generators — always sample-rate ----
   :phasor      :sample
   :sine-bi     :sample
   :sine-uni    :sample
   :tri         :sample
   :rect        :sample
   :vco         :sample
   :noise       :sample
   :pink-noise  :sample
   :audio-in    :sample
   ;; ---- ops with 1-sample state or IIR feedback — always sample-rate ----
   :history     :sample   ; uses Faust ' operator
   :delta       :sample   ; uses '
   :smooth      :sample   ; si.smooth — IIR pole
   :slew        :sample   ; ~ _ feedback
   :hysteresis  :sample   ; ~ _ feedback
   :damping     :sample   ; 3-tap FIR uses '
   :beat-trigger :sample  ; (phase' - phase) uses '
   :sah         :sample   ; ba.sAndH — sample-indexed
   :sample-hold :sample   ; ba.sAndH
   :track-hold  :sample   ; select2(gate, _, in) ~ _ — 1-sample feedback
   :ar-env      :sample   ; envelope state machine
   :delay       :sample   ; delay line — sample-indexed
   :allpass     :sample   ; de.apf — delay-based
   :buffer      :sample   ; rwtable — sample-indexed
   :counter     :sample   ; edge detection via '
   ;; ---- utility ops — stateless, rate-polymorphic ----
   ;; :abs — absolute value; full-wave rectify when applied to audio
   :abs         :polymorphic
   ;; :sqrt — square root; undefined for negative inputs (no guarding applied)
   :sqrt        :polymorphic
   ;; :min — signal minimum; acts as AND gate on boolean (gate) signals
   :min         :polymorphic
   ;; :max — signal maximum; acts as OR gate on boolean signals;
   ;;   half-wave rectify: (max x 0.0)
   :max         :polymorphic
   ;; ---- filters — always sample-rate ----
   :ladder      :sample
   :svf         :sample
   :naive-svf   :sample
   :crossover   :sample
   :one-pole    :sample
   :dc-block    :sample
   ;; ---- stateless pure-function ops — rate follows max(input rates) ----
   :mul         :polymorphic
   :add         :polymorphic
   :sub         :polymorphic
   :div         :polymorphic
   :clip        :polymorphic
   :wrap        :polymorphic
   :fold        :polymorphic
   :vca         :polymorphic
   :crossfade   :polymorphic
   :ring-mod    :polymorphic
   :soft-clip   :polymorphic
   :hard-clip   :polymorphic
   :wave-fold   :polymorphic
   :bitcrusher  :polymorphic
   :comparator  :polymorphic
   :segment     :polymorphic
   :table       :polymorphic
   :select      :polymorphic
   ;; ---- beat-domain ----
   :beat-phase  :beat
   :beat-bpm    :block
   ;; ---- parameter and constant nodes ----
   ;; :param — hslider, updated at block rate by the plugin host.
   ;; :const — numeric literal; does not vary per-sample, so :block is correct.
   ;;   Faust auto-promotes consts to sample-rate when combined with sample-rate
   ;;   signals; using :block here ensures pure-param patches get dominant-rate :block
   ;;   and route to the nomos-rt modulator compile path rather than CLAP.
   :param       :block
   :const       :block})

(def port-node-specs
  "Secondary port nodes created automatically for multi-output ops.
  Maps op-kw → {port-name {:op port-op-kw :inlets [inlet-kws]}}.
  :source always refers to the primary node id; other inlet names are
  resolved from the primary node's own inputs map by the same key."
  {:counter    {:carry    {:op :counter-carry   :inlets [:source :clock]}}
   :comparator {:inv-gate {:op :comparator-inv  :inlets [:source]}}
   :naive-svf  {:hp       {:op :naive-svf-hp    :inlets [:in :cutoff :resonance]}}
   :crossover  {:hp       {:op :crossover-hp    :inlets [:in :cutoff]}}})
