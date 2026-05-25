; SPDX-License-Identifier: EPL-2.0
(ns examples.ears
  "Ears-inspired external input / envelope follower.

  Signal flow:
    audio-in → gain → peak envelope follower → CV out
                                             → gate out (threshold comparator)
                                             → transient out (onset detection)

  Peak envelope follower: abs(gained) → slew(attack, release)
    Rectify the signal to get instantaneous amplitude, then apply an
    asymmetric first-order lag: fast attack captures transients, slow release
    holds the envelope between them.

    With attack ≈ 1–10 ms and release ≈ 50–500 ms this behaves identically
    to a standard analog peak detector (diode + RC network).

  Gate: comparator(env, threshold)
    Fires while the envelope exceeds the threshold.  The optional hysteresis
    width prevents gate chatter on signals that hover near the threshold.

  Transient: comparator(delta(env), transient-thresh)
    The delta of the envelope is large and positive when a new loud event
    starts (onset detection).  Useful as a 1-sample trigger into a counter,
    sample-hold, or sequencer clock.

  No new ops required — this example exercises the composition of:
    :abs (new in Kinks), :slew, :vca, :comparator, :hysteresis, :delta.

  Note: RMS following would need (sqrt (slew (mul x x) ...)) — the
  required :sqrt op is a near-term gap (also needed for equal-power
  crossfade and octave-to-Hz CV scaling).

  Compile target: sample-rate CLAP plugin (audio-in present → :sample)."
  (:require [alembic.patch :refer [defpatch!]]))

(defpatch! ears
  {:params {:gain             {:range [0.0 4.0]  :default 1.0}
            :attack           {:range [0.001 0.5] :default 0.01}
            :release          {:range [0.01 2.0]  :default 0.1}
            :threshold        {:range [0.0 1.0]   :default 0.3}
            :hyst-width       {:range [0.0 0.2]   :default 0.05}
            :transient-thresh {:range [0.0 0.2]   :default 0.02}}}
  (let [in     (audio-in)

        ;; Input gain stage — VCA before the envelope detector
        gained (vca in (param :gain))

        ;; Peak envelope follower:
        ;;   abs(gained) → instantaneous rectified amplitude
        ;;   slew → asymmetric RC: fast attack, slow release
        env    (slew (abs gained)
                     (param :attack)
                     (param :release))

        ;; Gate: fires while envelope exceeds threshold.
        ;; Hysteresis deadband prevents chatter on signals near the threshold.
        gate   (hysteresis env
                           (param :threshold)
                           (param :hyst-width))

        ;; Transient / onset trigger:
        ;; delta(env) is large and positive at the start of a loud event.
        ;; comparator fires a 1-sample trigger on each new onset.
        onset-cmp (comparator (delta env) (param :transient-thresh))
        onset     (:out onset-cmp)]

    ;; Three sequential outputs: envelope CV, gate, onset trigger
    (output env)
    (output gate)
    (output onset)))
