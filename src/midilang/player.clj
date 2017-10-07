(ns midilang.player
  (:require [midi :refer [midi-out]]
            [overtone.at-at :refer :all])
  (:import
   (javax.sound.midi MidiMessage ShortMessage)))


;; --------------------------------------------------------------------------------
;; midi and at-at basics
(defn- midi-cc
  ([sink cc val] (midi-cc sink cc val 0))
  ([sink channel cc val]
   (let [msg (ShortMessage.)]
     (.setMessage msg ShortMessage/CONTROL_CHANGE channel cc val)
     (.send (:receiver sink) msg -1))))

(defn- midi-note-on-2
  "Send a midi on msg to the sink."
  ([sink note-num vel] (midi-note-on-2 sink 0 note-num vel))
  ([sink channel note-num vel]
   (let [on-msg  (ShortMessage.)]
     (.setMessage on-msg ShortMessage/NOTE_ON channel note-num vel)
     (.send (:receiver sink) on-msg -1))))

(def pool (mk-pool))

;; default port is basically "any"
(def default-output nil)

(defn set-default-output! [which]
  ;; FIXME #'set! won't let me change root bindings. #'bindings and #'push-root-bindings are overpowered considering there's only one thread here. why on earth does #'def work here?
  (def default-output which))

;; --------------------------------------------------------------------------------
;; multimethods
(defmulti trigger :type)

(defmethod trigger :note [evt]
  ;; on
  (after (:time evt)
         #((midi-note-on-2 (or (:midi-output evt) default-output)
                           (or (:midi-channel evt) 0)
                           (:note-number evt)
                           (:note-velocity evt)))
         pool)
  ;; off
  (after ((if (:portamento evt) + -)
          (+ (:time evt) (:note-duration evt)) 
          5 ; by default, shave 5ms off note to prevent slurring.
                                        ; add 5ms to provoke it for a portamento.
          )
         #((midi-note-on-2 (or (:midi-output evt) default-output)
                           (or (:midi-channel evt) 0)
                           (:note-number evt)
                           0))
         pool))

(defmethod trigger :control-change [evt]
  (after (:time evt)
         #((midi-cc (or (:midi-output evt) default-output)
                    (or (:midi-channel evt) 0)
                    (:cc-number evt)
                    (:cc-value evt)))
         pool))

(defmethod trigger :break [evt]
  nil)

;; --------------------------------------------------------------------------------
;; player
(def =play-loop= (atom false))

(defn play [fn duration]
  (let [events (sort-by :time (fn 0 duration))]
    (doall (for [evt events]
             (trigger evt))))
  (after duration
         #(when @=play-loop=
            (play fn duration))
         pool)
  nil)

(defn play-looping [fn duration]
  (reset! =play-loop= true)
  (play fn duration))

(defn stop-looping []
  (reset! =play-loop= false))

(defn stop-everything []
  (stop-looping)
  (stop-and-reset-pool! pool :strategy :kill)
  nil)
