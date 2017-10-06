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
  ([sink cc val channel]
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
(defn single-note [note]
  (fn [t dur]
    [{:time t
      :type :note
      :note-duration dur
      :note-number note
      :note-velocity 127}]))

(defn nix []
  (fn [t dur]
    []))

(defn break []
  (fn [t dur]
    [{:time t
      :type :break}]))

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

;; post-modifier
#_(defn reverse [] nil ;; TODO
  )

;; combinatory
(defn overlay [f & more]
  "overlay a series of events to occur at the same time."
  (let [fns (concat (if (seq? f) f [f]) more)]
    (fn [t dur]
      (flatten
       (map #(% t dur)
            fns)))))

(defn append [f & more]
  "append a series of events to occur one after another."
  (let [fns (concat (if (seq? f) f [f]) more)]
    (fn [t dur]
      (let [dur-fraction (/ dur (count fns))]
        (flatten
         (for [i (range (count fns))]
           ((nth fns i)
            (+ t (* i dur-fraction))
            dur-fraction)))))))

;; --------------------------------------------------------------------------------
;; examples
(def kick (single-note 36))
(def snare (single-note 38))
(def chat (single-note 42))

(def four-floor
  ;;(apply append (map single-note (repeat 4 36)))
  (append (repeat 4 kick)))

(def tztztz
  (overlay four-floor
           ;;(apply append (map single-note (repeat 16 42)))
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

(comment
  (play four-floor 2000)
  (play tztztz 2000)
  (play btzack 2000)
  (play weirdo 2000)
  (play-looping weirdo 2000)
  (play-looping note-dur-test 2000)
  (play-looping weirdo-2 2000)
  ;;
  (stop-looping)
  (stop-everything)
  )
