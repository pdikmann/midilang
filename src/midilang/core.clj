(ns midilang.core
  (:gen-class)
  (:require [clojure.test :refer :all]
            [midi :refer [midi-out]]
            ;;[overtone.at-at :refer :all]
            [midilang.player :as p :refer [set-default-output!
                                           play
                                           play-looping
                                           stop-looping
                                           stop-everything]]
            [midilang.composition :as c]
            [midilang.gear.tr09 :refer :all]
            [midilang.gear.tb03 :refer :all]
            [midilang.gear.common :refer [all-notes-off]]))

(defn -main
  [& args]
  (println "Hello, World!"))

;; defining a fixed output for both TR-09 and TB-03 is tricky, because
;; overtone.midi picks them by name, and the name is influenced by
;; their order of appearance on the USB ports. Long story short,
;; switch on the TR-09 first!
(def tr-09-midi-output (midi-out "2"))
(def tb-03-midi-output (midi-out "3"))
(set-default-output! (midi-out "Boutique")) ;; aka "any, please"

(def tr-09 (partial c/with-device tr-09-midi-output 9))
(def tb-03 (partial c/with-device tb-03-midi-output 0))

;; --------------------------------------------------------------------------------
;; examples

(def bpm 136)
(def beat (/ (* 60 1000) bpm))
(def bar (* 4 beat))

(def pitch-on (partial c/transfer-pitches (c/notes [20 20 23 22 40 48 32 38])))

(defn live [t dur]
  ((c/overlay (tb-03
               ;; (c/transfer-pitches (c/notes [20 32 24 34 60 48])
               ;;                     (c/rythm [2 1 1 1 1 2 1 1 1 2 1 1 1]))
               (pitch-on ;; (c/rythm [2 1 1 2 1 2 1 3 1 1 1])
                (c/rythm [4 1 2 1 4 1 2 1])
                )
               )
              (tr-09 (c/transfer-pitches (c/append bd bd rs sd ch ch cp rs cc rc)
                                         (c/rythm [1 1 2 1 2 1 2 1 2 3]))))
   t dur))

(comment
  (play-looping #'live bar)
  (do (stop-everything)
      (play (c/overlay (tb-03 all-notes-off)
                       (tr-09 all-notes-off))
            100))
  )

(def four-floor
  (tr-09
   (c/append (repeat 4 kick))))

(def tztztz
  (tr-09
   (c/overlay four-floor
              (c/append (repeat 16 chat)))))

(def btzack
  (tr-09
   (c/overlay tztztz
              (c/append (flatten (repeat 2 [c/nix snare]))))))

(def weirdo
  (tr-09
   (c/overlay four-floor
              (c/append (repeat 3 snare))
              (c/append (repeat 7 chat)))))

(def note-dur-test
  (tr-09
   (c/append (repeat 4 (c/scale 0.001 snare)))))

(def weirdo-2
  (tr-09
   (c/overlay four-floor
              (c/scale-r 1/2 (c/append (repeat 3 snare)))
              (c/append (repeat 2 (c/scale 1/3 (c/append (repeat 3 chat))))))))

(def volume-test
  (tr-09
   (c/append (c/volume 20 four-floor)
             (c/volume 40 four-floor)
             (c/volume 80 four-floor)
             (c/volume 120 four-floor))))

(def volume-spaz
  (tr-09
   (c/volume [40 80 20 120 60 20 90 70]
             (c/append (repeat 8 four-floor)))))

(def abrubtor
  (tr-09
   (c/volume [127 10]
             ;;[127 10 127 10 127 127]
             (c/append (repeat 2 (c/scale (c/append kick kick) 1/2))
                       kick kick))))

(def tom-tuner
  (tr-09
   (c/overlay (c/append (lt-tune 0)
                        (lt-tune 40)
                        (lt-tune 80)
                        (lt-tune 127))
              (c/volume 127 (c/append (repeat 4 ltom))))))

(def tom-tricks
  (tr-09
   (c/overlay
    (c/append (take 16 (cycle [ltom mtom htom mtom])))
    (c/append (map tom-decay
                   (map (partial + 15)
                        (reverse (range 0 127 16)) ; 8 values
                        )))
    (c/append (map tom-tune
                   (range 0 127 16)     ; 8 values
                   )))))

(def htom-lvl
  (partial c/cc-event 54))

(defn stroy [num]
  (tr-09
   (c/overlay (c/append (repeat 8 htom))
              (c/append (take num (cycle [(ht-tune 127)
                                          (ht-tune 0)])))
              (ht-decay 127)
              ;;four-floor
              )))

(def tom-stroyer
  (tr-09
   (c/append (stroy 8)
             (stroy 16)
             (stroy 32)
             (stroy 64)
             (stroy 128)
             (stroy 256))))

(def errorize
  (let [rrr (fn [count] (c/append (repeat count kick)))]
    (tr-09
     (c/overlay (c/append (repeat 12 snare))
                (c/append (rrr 64)
                          (rrr 128)
                          (rrr 256))))))

(def staggered
  (tr-09 (c/overlay four-floor
                    (c/stagger 1/8 (c/append snare snare))
                    (c/stagger 1/32 (c/append chat chat chat chat)))))

(def grungy
  (tr-09 (c/overlay (c/append (c/append (c/append (repeat 12 htom))
                                        (repeat 3 c/nix))
                              (c/append (c/append (repeat 48 htom))
                                        (repeat 3 c/nix)))
                    (c/append (repeat 4 snare))
                    four-floor)))

(comment
  (play-looping
   (tb-03
    (c/overlay (c/volume [40 80 120 40 120]
                         (c/append (c/frepeat 6 [(c/note-event 60)
                                                 (c/note-event 62)
                                                 (c/note-event 30)
                                                 (c/note-event 48)])))
               (c/append (c/ctake 8 (map overdrive [0 80 0 40])))
               (c/append (c/ctake 6 (map delay-feedback [0 20])))
               (c/append (map delay-time [0 20]))
               (c/append (env-mod 0)
                         (env-mod 127))))
   (* 3/2 bar))
  (stop-everything)
  )

(comment ;; note off events!
  (play-looping
   (tb-03 (c/overlay (c/note-event 60)
                     (c/stagger 1/2 all-notes-off)))
   bar)
  (stop-looping)
  )

(defn live-duo [t dur]
  ((c/overlay (tb-03
               (c/overlay (c/volume 64
                                    (c/append (repeat 4 (c/append (c/append (c/frepeat 4 (c/scale 0.1 (c/note-event 40))))
                                                                  (c/append (c/frepeat 4 (c/note-event 40)))))))
                          (let [m 80
                                middle (tuning m)]
                            (c/append middle middle middle
                                      (c/append middle
                                                (c/append (map tuning (range m 60 -4))))))
                          ))
              (tr-09
               (c/overlay (c/volume 60 (c/append (repeat 16 bd)))
                          (bd-decay 127)
                          (c/volume 60 (c/append (repeat 32 sd)))
                          (c/append (map sd-tone (c/ctake 32 [20 80])))
                          (sd-tune 127)
                          (sd-snappy 60)
                          (c/append (repeat 8 (c/append (repeat 6 c/nix)
                                                        (repeat 2 ch))))
                          (ch-level 80))))
   t dur))

(comment
  (play-looping #'live-duo (* 4 bar))
  (play-looping (tb-03 (c/overlay (c/volume 64 (c/append (c/frepeat 32 (c/scale 0.1 (c/note-event 40)))))
                                  (tuning 80)))
                (* 4 bar))
  (stop-looping)
  )

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
  (play-looping (slice 1/2 1/2 grungy) bar)
  ;;
  (stop-looping)
  (stop-everything)
  )

;; --------------------------------------------------------------------------------
;; live
(defn live-example [t dur]
  ((c/overlay (tr-09
               (c/overlay (c/append (apply c/append (c/append (repeat 12 htom))
                                           (repeat 3 c/nix))
                                    (apply c/append (c/append (repeat 48 htom))
                                           (repeat 3 c/nix)))
                          (c/append (repeat 4 snare))
                          four-floor))
              (tb-03
               (c/notes [60 70 80 40])))
   t dur))

(comment
  (play-looping #'live-example bar)
  (stop-everything)
  )

