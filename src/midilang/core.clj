(ns midilang.core
  (:gen-class)
  (:require [clojure.test :refer :all]
            [midi :refer :all]
            [overtone.at-at :refer :all])
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
  (stop-loop)
  (stop-and-reset-pool! pool :strategy :kill)
  nil)

;; --------------------------------------------------------------------------------
;; composition

;; elementary
(defn note-event [note]
  (fn [t dur]
    [{:time t
      :type :note
      :note-duration dur
      :note-number note
      :note-velocity 127}]))

(defn cc-event [num val]
  (fn [t dur]
    [{:time t
      :type :control-change
      :cc-number num
      :cc-value val}]))

(defn break []
  "currently unused / synonymous with #'nix"
  (fn [t dur]
    [{:time t
      :type :break}]))

(defn nix []
  (fn [t dur]
    []))


;; pre-modifier
(defn scale [f s]
  "scale event to take S * DURATION time, aligned/starting at T."
  (fn [t dur]
    (f t (* dur s))))

(defn scale-r [f s]
  "scale event to take S * DURATION time, aligned/ending at DURATION."
  (fn [t dur]
    (f (+ t (- dur (* dur s)))
       (* dur s))))

#_(defn interleave [] ;; TODO
    )

;; post-modifier
#_(defn reverse [] nil ;; TODO
    )

(defn volume [f vol]
  (let [vols (if (coll? vol)
               (cycle vol)
               (cycle [vol]))]
    (fn [t dur]
      (map #(assoc %1 :note-velocity %2)
           (f t dur)
           vols))))

;; combinatory
(defn- collect [some rest]
  (concat (if (coll? some)
            some
            [some])
          rest))

(defn overlay [f & more]
  "overlay a series of events to occur at the same time."
  (let [fns (collect f more)]
    (fn [t dur]
      (flatten
       (map #(% t dur)
            fns)))))

(defn append [f & more]
  "append a series of events to occur one after another."
  (let [fns (collect f more)]
    (fn [t dur]
      (let [dur-fraction (/ dur (count fns))]
        (flatten
         (for [i (range (count fns))]
           ((nth fns i)
            (+ t (* i dur-fraction))
            dur-fraction)))))))

;; --------------------------------------------------------------------------------
;; examples
(def kick (note-event 35))
(def snare (note-event 38))
(def chat (note-event 42))
(def ltom (note-event 41))
(def mtom (note-event 45))
(def htom (note-event 48))

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
           (append (flatten (repeat 2 [(nix) snare])))))

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

(def ltom-tune (partial cc-event 46))
(def mtom-tune (partial cc-event 49))
(def htom-tune (partial cc-event 52))
(defn tom-tune [val]
  (overlay (ltom-tune val)
           (mtom-tune val)
           (htom-tune val)))

(def tom-tuner
  (overlay (append (ltom-tune 0)
                   (ltom-tune 40)
                   (ltom-tune 80)
                   (ltom-tune 127))
           (volume (append (repeat 4 ltom))
                   127)))

(def ltom-decay (partial cc-event 47))
(def mtom-decay (partial cc-event 50))
(def htom-decay (partial cc-event 53))
(defn tom-decay [val]
  (overlay (ltom-decay val)
           (mtom-decay val)
           (htom-decay val)))

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
           (append (take num (cycle [(htom-tune 127)
                                     (htom-tune 0)])))
           (htom-decay 127)
           ;;four-floor
           ))

(def tom-stroyer (append (stroy 8)
                         (stroy 16)
                         (stroy 32)
                         (stroy 64)
                         (stroy 128)
                         (stroy 256)))

(defn live-example [t dur]
  ((overlay (append snare snare snare snare)
            (append mtom mtom ltom ltom htom)
            (append (take 128 (cycle [(mtom-tune 127)
                                      (mtom-tune 0)])))
            tztztz)
   t dur))

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
  (play-looping #'live-example bar)
  ;;
  (stop-looping)
  (stop-everything)
  )

(comment
  (play (append (flatten (repeat 4 [(note-event 35) (note-event 36)]))) bar))
