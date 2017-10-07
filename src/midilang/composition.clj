(ns midilang.composition)

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

(defn no-event []
  (fn [t dur]
    []))

(def nix (no-event))

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

(defn stagger [f s]
  "delay event by S * DURATION time."
  (fn [t dur]
    (f (+ t (* dur s))
       dur)))

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

(defn slice [f fractional-t fractional-dur]
  "select only a particular part of the events, starting at (* FRACTIONAL-T T) running for (* FRACTIONAL-DUR DUR)."
  (fn [t dur]
    ;; scale t and dur so that fractional-t and fractional-dur would come out at 0 and dur
    (let [scaled-dur (* dur (/ 1 fractional-dur))
          scaled-t (- (* fractional-t scaled-dur))
          evts (f scaled-t scaled-dur)]
      (filter #(and (<= 0 (:time %))
                    (> dur (:time %)))
              evts))))

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
