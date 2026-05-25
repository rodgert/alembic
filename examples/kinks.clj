; SPDX-License-Identifier: EPL-2.0
(ns examples.kinks
  "Kinks-inspired utility processor.

  MI Kinks is a dual utility section: a sample/track-and-hold pair (top),
  and a rectifier/boolean-logic section (bottom).

  Signal flow (per section):

    S&H / T&H section (top):
      in-a, gate → sample-hold   — captures in-a on rising edge of gate
      in-a, gate → track-hold    — follows in-a while gate > 0.5, holds on fall

    Rectifier / logic section (bottom):
      in-a        → abs in-a     — full-wave rectify: folds negative half up
      in-a        → max in-a 0   — half-wave rectify: zeros negative half
      in-a, in-b  → max in-a in-b — OR: output follows the larger (louder) signal
      in-a, in-b  → min in-a in-b — AND: output follows the smaller signal
      in-a, in-b  → abs(a - b)   — XOR-like: passes signal when inputs differ

  Rate note: abs / min / max are rate-polymorphic.  Use params for all
  inputs → block-rate modulator (kinks-cv, dominant-rate :block).
  Use audio-in → sample-rate CLAP plugin (kinks-audio, dominant-rate :sample).
  track-hold and sample-hold are always sample-rate (1-sample feedback / edge
  detection)."
  (:require [alembic.patch :refer [defpatch!]]))

;; ---------------------------------------------------------------------------
;; Audio-rate Kinks — CLAP plugin target
;;
;; Two audio inputs, one gate param.
;; All rectifier/logic outputs are :sample (audio-in drives them).
;; S&H and T&H are always :sample.
;; ---------------------------------------------------------------------------

(defpatch! kinks-audio
  {:params {:gate {:range [0.0 1.0] :default 0.0}}}
  (let [a    (audio-in)
        b    (audio-in)
        gate (param :gate)

        ;; S&H / T&H
        sh   (sample-hold a gate)          ; capture on rising edge
        th   (track-hold  a gate)          ; follow while gate high, hold on fall

        ;; Rectifier section
        full-rect  (abs a)                 ; |a|: fold negative half up
        half-rect  (max a 0.0)             ; a+: zero negative half

        ;; Boolean logic section
        sig-or  (max a b)                  ; OR:  follows the larger
        sig-and (min a b)                  ; AND: follows the smaller
        xor-ish (abs (sub a b))]           ; XOR-like: signal when inputs differ

    (output sh)
    (output th)
    (output full-rect)
    (output half-rect)
    (output sig-or)
    (output sig-and)
    (output xor-ish)))

;; ---------------------------------------------------------------------------
;; Block-rate Kinks — nomos-rt modulator target
;;
;; All signal inputs are params (:block rate).  abs / min / max are
;; rate-polymorphic and resolve to :block → dominant-rate :block.
;; Note: sample-hold and track-hold require :sample; omit from CV version.
;;   Use (sample-hold (param :in) (param :gate)) if S&H at block rate is needed
;;   — it works but promotes dominant-rate to :sample.
;; ---------------------------------------------------------------------------

(defpatch! kinks-cv
  {:params {:in-a {:range [-5.0 5.0] :default 0.0}
            :in-b {:range [-5.0 5.0] :default 0.0}}}
  (let [a (param :in-a)
        b (param :in-b)]
    (output (abs a))           ; full-wave rectify
    (output (max a 0.0))       ; half-wave rectify
    (output (max a b))         ; OR
    (output (min a b))         ; AND
    (output (abs (sub a b))))) ; XOR-like
