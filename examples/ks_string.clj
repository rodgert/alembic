; SPDX-License-Identifier: EPL-2.0
(ns examples.ks-string
  "Karplus-Strong plucked string resonator — one KS mode of MI Rings.

  This is a single-voice KS string, not a full Rings: no modal resonator
  bank, no polyphony, no FM voice, no sympathetic-strings mode, no strum
  input, no internal pitch quantisation.

  Signal flow:
    gate → ar-env(1ms/10ms) ─┐
    noise ─────────────────── vca → exciter ─┐
                                             │
                ┌────────────────────────────┘
                ↓
           (exciter + feedback)
                ↓
           de.delay(SR/freq − 1)    ← pitch-setting delay line
                ↓
           averaging FIR damp       ← 0.5·decay·(y + bright·y')
                │
                ├─→ output
                └─→ feedback  (Faust ~ _ combinator)

  Damping filter: first-order averaging FIR.
    bright=0 → no HF damping (pass-through); full-bandwidth, slowest decay.
    bright=1 → maximum HF loss per loop cycle; darkest tone, fastest decay.

  decay [0,1]: per-loop amplitude multiplier on top of the FIR damping.
    decay=1 → no extra loss (pure KS: only bright controls timbre/sustain).
    decay<1 → exponential taper each cycle; useful for staccato plucks.

  Implementation note:
    The ~ _ feedback combinator is inexpressible in the current alembic
    graph model, which supports only single-node feedback via :history.
    The KS loop body is therefore expressed via (faust ...).  The exciter
    chain (noise, ar-env, vca) and the damping coefficients are built from
    alembic ops so that param routing and rate inference work normally.

  Damping coefficients are pre-computed as alembic nodes to avoid inline
  arithmetic ambiguity inside the Faust string interpolation:
    scale = decay × 0.5           (direct branch coefficient)
    damp  = bright × decay × 0.5  (1-sample-delayed branch coefficient)
  The FIR output is scale·y + damp·y' = 0.5·decay·(y + bright·y').

  Compile target: sample-rate CLAP plugin (no audio-in; dominant-rate :sample
  from ar-env, noise, and the delay inside the faust escape)."
  (:require [alembic.patch :refer [defpatch!]]))

(defpatch! ks-string
  {:params {:freq   {:range [20.0 4000.0] :default 440.0 :unit :hz}
            :bright {:range [0.0 1.0]     :default 0.5}
            :decay  {:range [0.0 1.0]     :default 0.9}
            :gate   {:range [0.0 1.0]     :default 0.0}}}
  (let [;; Short noise burst shaped by a fast AR envelope.
        ;; 1 ms attack / 10 ms release gives a natural plucked impulse.
        env     (ar-env (param :gate) 0.001 0.01)
        exciter (vca (noise) env)

        ;; Pre-compute FIR damping coefficients as alembic graph nodes.
        ;; Using named nodes rather than inline arithmetic avoids ambiguity
        ;; in the faust string where %scale and %damp are substituted as
        ;; Faust signal identifiers.
        scale   (mul (param :decay) 0.5)
        damp-c  (mul (mul (param :bright) (param :decay)) 0.5)

        ;; KS feedback loop via Faust ~ _ combinator:
        ;;   (exciter + _)            — sum exciter and feedback
        ;;   de.delay(N, d)           — delay d = int(SR/freq)−1 samples
        ;;   <: *(%scale), …          — fan-out into two branches
        ;;       *(%scale)            — direct:  scale·y
        ;;       (*(%damp) : @(1))    — delayed: damp·y' (1-sample lag)
        ;;   :> +                     — fan-in:  sum → scale·y + damp·y'
        ;;   ~ _                      — feed output back to (exciter + _)
        ;; max(1, …) guards against zero-length delay at high frequencies.
        string  (faust
                 "(((_ + %exc) : de.delay(131072, max(1, int(ma.SR / max(1.0, %freq)) - 1)) <: *(%scale), (*(%damp) : @(1)) :> +) ~ _)"
                 {:exc   exciter
                  :freq  (param :freq)
                  :scale scale
                  :damp  damp-c})]
    (output string)))
