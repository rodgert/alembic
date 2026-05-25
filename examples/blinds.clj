; SPDX-License-Identifier: EPL-2.0
(ns examples.blinds
  "Blinds-inspired quad polarising VCA.

  MI Blinds is a four-channel polarising mixer: each channel scales its input
  by a bipolar gain [-1, 1].  At gain=0 the signal is silenced; at gain=+1 it
  passes unchanged; at gain=-1 it is phase-inverted.  When nothing is patched
  into an even channel's input the odd-channel output sums into it (daisy-chain
  mixing); that topology detail is omitted here.

  Rate duality — two patch definitions from the same op vocabulary:

    blinds-audio  — sample-rate: audio-in for each channel input.
                    dominant-rate = :sample → compiles to CLAP plugin.

    blinds-cv     — block-rate: params supply both signal and gain.
                    dominant-rate = :block → compiles to nomos-rt modulator.

  The difference is solely whether the signal inlet is audio-in (:sample) or
  a param (:block).  Rate-polymorphic ops (mul/vca) inherit the maximum rate
  of their inputs, so the patch graph and compile route are determined by the
  signal source — no explicit rate annotation required."
  (:require [alembic.patch :refer [defpatch!]]))

;; ---------------------------------------------------------------------------
;; Audio-rate Blinds — CLAP plugin target
;;
;; Four channels of audio VCA.  Each channel: audio-in × gain-param → output.
;; Gain param is bipolar [-1, 1]: negative → phase inversion.
;; vca(audio-in[:sample], param[:block]) = :sample → dominant-rate = :sample.
;; ---------------------------------------------------------------------------

(defpatch! blinds-audio
  {:params {:gain-1 {:range [-1.0 1.0] :default 1.0}
            :gain-2 {:range [-1.0 1.0] :default 1.0}
            :gain-3 {:range [-1.0 1.0] :default 1.0}
            :gain-4 {:range [-1.0 1.0] :default 1.0}}}
  (let [in-1 (audio-in)
        in-2 (audio-in)
        in-3 (audio-in)
        in-4 (audio-in)]
    (output (vca in-1 (param :gain-1)))
    (output (vca in-2 (param :gain-2)))
    (output (vca in-3 (param :gain-3)))
    (output (vca in-4 (param :gain-4)))))

;; ---------------------------------------------------------------------------
;; Block-rate Blinds — nomos-rt faust_modulator target
;;
;; Four channels of CV scaling.  Both signal and gain are params (:block rate).
;; mul(param[:block], param[:block]) = :block → dominant-rate = :block.
;; ---------------------------------------------------------------------------

(defpatch! blinds-cv
  {:params {:in-1   {:range [-1.0 1.0] :default 0.0}
            :gain-1 {:range [-1.0 1.0] :default 1.0}
            :in-2   {:range [-1.0 1.0] :default 0.0}
            :gain-2 {:range [-1.0 1.0] :default 1.0}
            :in-3   {:range [-1.0 1.0] :default 0.0}
            :gain-3 {:range [-1.0 1.0] :default 1.0}
            :in-4   {:range [-1.0 1.0] :default 0.0}
            :gain-4 {:range [-1.0 1.0] :default 1.0}}}
  (let [out-1 (mul (param :in-1) (param :gain-1))
        out-2 (mul (param :in-2) (param :gain-2))
        out-3 (mul (param :in-3) (param :gain-3))
        out-4 (mul (param :in-4) (param :gain-4))]
    (output out-1)
    (output out-2)
    (output out-3)
    (output out-4)))
