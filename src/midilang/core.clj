(ns midilang.core
  (:gen-class)
  (:require [clojure.test :refer :all]
            [midi :refer :all]
            [overtone.at-at :refer :all]
            [midilang.composition :refer :all]
            [midilang.gear :refer :all])
  (:import
   (javax.sound.midi MidiMessage ShortMessage)))

(defn -main
  [& args]
  (println "Hello, World!"))

;; --------------------------------------------------------------------------------
;; basic

(defn midi-cc
  ([sink cc val] (midi-cc sink cc val 0))
  ([sink channel cc val]
   (let [msg (ShortMessage.)]
     (.setMessage msg ShortMessage/CONTROL_CHANGE channel cc val)
     (.send (:receiver sink) msg -1))))

(defn midi-note-on-2
  "Send a midi on msg to the sink."
  ([sink note-num vel] (midi-note-on-2 sink 0 note-num vel))
  ([sink channel note-num vel]
   (let [on-msg  (ShortMessage.)]
     (.setMessage on-msg ShortMessage/NOTE_ON channel note-num vel)
     (.send (:receiver sink) on-msg -1))))

(def pool (mk-pool))

(def output (midi-out "Boutique"))

;; --------------------------------------------------------------------------------
;; player
(defmulti trigger :type)

(defmethod trigger :note [evt]
  ;; on
  (after (:time evt)
         #((midi-note-on-2 output 9
                           (:note-number evt)
                           (:note-velocity evt)))
         pool)
  ;; off
  (after (+ (:time evt) (:note-duration evt))
         #((midi-note-on-2 output 9
                           (:note-number evt)
                           0))
         pool))

(defmethod trigger :control-change [evt]
  (after (:time evt)
         #((midi-cc output 9
                    (:cc-number evt)
                    (:cc-value evt)))
         pool))

(defmethod trigger :break [evt]
  nil)

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

;; --------------------------------------------------------------------------------
;; examples

(def bpm 136)
(def beat (/ (* 60 1000) bpm))
(def bar (* 4 beat))

(def four-floor
  ;;(apply append (map note-event (repeat 4 36)))
  (append (repeat 4 kick)))

(def tztztz
  (overlay four-floor
           ;;(apply append (map note-event (repeat 16 42)))
           (append (repeat 16 chat))))

(def btzack
  (overlay tztztz
           (append (flatten (repeat 2 [nix snare])))))

(def weirdo
  (overlay four-floor
           (append (repeat 3 snare))
           (append (repeat 7 chat))))

(def note-dur-test
  (append (repeat 4 (scale snare 0.001))))

(def weirdo-2
  (overlay four-floor
           (scale-r (append (repeat 3 snare))
                    1/2)
           (append (repeat 2 (scale (append (repeat 3 chat))
                                    1/3)))))

(def volume-test
  (append (volume four-floor 20)
          (volume four-floor 40)
          (volume four-floor 80)
          (volume four-floor 120)))

(def volume-spaz
  (volume (append (repeat 8 four-floor))
          [40 80 20 120 60 20 90 70]))

(def abrubtor
  (volume (append (repeat 2 (scale (append kick kick) 1/2))
                  kick kick)
          ;;[127 10 127 10 127 127]
          [127 10]
          ))

(def tom-tuner
  (overlay (append (lt-tune 0)
                   (lt-tune 40)
                   (lt-tune 80)
                   (lt-tune 127))
           (volume (append (repeat 4 ltom))
                   127)))

(def tom-tricks
  (overlay ;;(append (repeat 2 four-floor))
   (append (take 16 (cycle [ltom mtom htom mtom])))
           (append (map tom-decay
                        (map (partial + 15)
                             (reverse (range 0 127 16)) ; 8 values
                             )))
           (append (map tom-tune
                        (range 0 127 16) ; 8 values
                        ))))

(def htom-lvl (partial cc-event 54))

(defn stroy [num]
  (overlay (append (repeat 8 htom))
           (append (take num (cycle [(ht-tune 127)
                                     (ht-tune 0)])))
           (ht-decay 127)
           ;;four-floor
           ))

(def tom-stroyer (append (stroy 8)
                         (stroy 16)
                         (stroy 32)
                         (stroy 64)
                         (stroy 128)
                         (stroy 256)))

(def errorize
  (let [rrr (fn [count] (append (repeat count kick)))]
    (overlay (append (repeat 12 snare))
             (append (rrr 64)
                     (rrr 128)
                     (rrr 256)))))

(def staggered
  (overlay four-floor
           (stagger (append snare snare)
                    1/8)
           (stagger (append chat chat chat chat)
                    1/32)))

(def grungy
  (overlay (append (apply append (append (repeat 12 htom))
                          (repeat 3 nix))
                   (apply append (append (repeat 48 htom))
                          (repeat 3 nix)))
           (append (repeat 4 snare))
           four-floor))

(comment
  (play four-floor bar)
  (play tztztz bar)
  (play btzack bar)
  (play weirdo bar)
  (play-looping weirdo bar)
  (play-looping note-dur-test bar)
  (play-looping weirdo-2 bar)
  (play volume-test bar)
  (play volume-spaz (* 2 bar))
  (play abrubtor bar)
  (play tom-tuner bar)
  (play-looping tom-tricks (* 2 bar))
  (play tom-stroyer (* 6 bar))
  (play-looping errorize (* 3 bar))
  (play staggered bar)
  (play-looping (slice grungy 1/2 1/2) bar)
  ;;
  (stop-looping)
  (stop-everything)
  )

;; --------------------------------------------------------------------------------
;; live
(defn live-example [t dur]
  ((overlay (append (apply append (append (repeat 12 htom))
                           (repeat 3 nix))
                    (apply append (append (repeat 48 htom))
                           (repeat 3 nix)))
            (append (repeat 4 snare))
            four-floor)
   t dur))

(comment
  (play-looping #'live-example bar)
  (stop-everything)
  )

