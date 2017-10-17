(ns midilang.song01
  (:require [midi :refer [midi-out]]
            [midilang.player :as p :refer [set-default-output!
                                           play
                                           play-looping
                                           stop-looping
                                           stop-everything]]
            [midilang.composition :as c]
            [midilang.gear.tr09 :as tr]
            [midilang.gear.tb03 :as tb]
            [midilang.gear.common :refer [all-notes-off]]))

(def tr-09-midi-output (midi-out "2"))
(def tb-03-midi-output (midi-out "3"))
(set-default-output! (midi-out "Boutique"))

(def tr (partial c/with-device tr-09-midi-output 9))
(def tb (partial c/with-device tb-03-midi-output 0))

(def bpm 136)
(def beat (/ (* 60 1000) bpm))
(def bar (* 4 beat))

(def floor09 (tr (c/overlay (c/append tr/bd tr/bd tr/bd tr/bd)
                             (tr/bd-tune 0)
                             (tr/bd-attack 0)
                             (tr/bd-comp 127)
                             (tr/bd-decay 127)
                             (tr/bd-level 127))))

(def snares
  (tr (c/overlay (c/stagger 1/4 (c/append (repeat 2 tr/sd)))
                 (tr/sd-tune 0)
                 (tr/sd-snappy 60)
                 (tr/sd-tone 40))))


(def snares-hectic
  (tr (c/overlay (c/stagger 1/8 (c/append (repeat 4 tr/sd)))
                 (tr/sd-tune 0)
                 (tr/sd-snappy 60)
                 (tr/sd-tone 40))))

(def tb-base-settings (tb (c/overlay (tb/cutoff 20)
                           (tb/reso 0)
                           (tb/env 127)
                           (tb/decay 0)
                           (tb/accent 80)
                           (tb/overdrive 0)
                           (tb/dly-t 20)
                           (tb/dly-fb 0))))

(def p303-1
  (tb (c/overlay
       (c/volume [80 80 80 127]
                 (c/append (repeat 8 (c/scale 1/6 (c/note-event 60)))))
       )))

(def p303-1-rising
  (tb (c/overlay (c/append (repeat 4 p303-1))
                 (c/append (map tb/cutoff (range 20 120 4))))))

(def p303-2
  (tb (c/overlay
       p303-1
       (tb/cutoff 120)
       (c/append (map tb/dly-t [8 18]))
       (c/append (map tb/dly-fb [60]))
       )))

(def intro          (c/overlay p303-1
                               tb-base-settings))
(def slow-beginning (c/overlay p303-1
                               floor09))
(def opening-filter (c/overlay p303-1-rising
                               (c/append (repeat 4 floor09))))
(def open           (c/overlay p303-2
                               floor09
                               snares))

(defn live [t dur]
  (
   t dur))

#_(defsong
  :bar-duration bar
  :parts [4 (repeat 4 p0)       ; number = x * bar for the next parts
          (repeat 4 p1)         ; coll = c/append these parts into one
          p2                    ; fn
          ])

(declare full-stop)
(comment
  (play-looping #'live bar)
  (full-stop)
  )

(defn full-stop []
  (stop-everything)
  (play (c/overlay (tb-03 all-notes-off)
                   (tr-09 all-notes-off))
        100))

