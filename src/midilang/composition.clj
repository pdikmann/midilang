(ns midilang.composition)

;; helpers
(defn ctake [n coll]
  (take n (cycle coll)))
(defn frepeat
  ([n x] (flatten (repeat n x)))
  ([x] (flatten (repeat x))))

(defn- step-reduce
  ([lst] (step-reduce lst 0 []))
  ([lst init accum]
   (if (empty? lst)
     accum
     (let [sum (+ init (first lst))]
       (recur (rest lst) sum (conj accum init))))))

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
  "currently unused / synonymous with #'no-event."
  (fn [t dur]
    [{:time t
      :type :break}]))

(defn no-event []
  (fn [t dur]
    []))

(def nix (no-event))

;; complex
(defn rythm [durs]
  (let [total (reduce + 0 durs)
        frac (partial * (/ 1 total))
        durs-1 (map frac durs)
        ts-1 (map frac (step-reduce durs))]
    (fn [t dur]
      (let [ts-actual (map (comp (partial + t) (partial * dur)) ts-1)
            durs-actual (map (partial * dur) durs-1)]
        (flatten
         (map (fn [start duration]
                ((note-event 60) start duration))
              ts-actual
              durs-actual))))))

(defn rythm2 [durs]
  "allows to write [1 2 [3] 4 [5]] and expect a rythm where index 2
  and 4 are BREAKs (instead of NOTEs). limitations: only 1 break per
  vector."
  (let [{:keys [durs breaks]}
        (loop [durs durs
               i 0
               pass-thru []
               break-indices []]
          (let [head (first durs)]
            (cond
              (nil? head) {:durs pass-thru
                           :breaks break-indices}
              (number? head) (recur (rest durs) (inc i) (conj pass-thru head)
                                    break-indices)
              (vector? head) (recur (rest durs) (inc i) (conj pass-thru (first head))
                                    (conj break-indices i)))))]
    (rythm durs)))

(defn notes [nums]
  (let [total (count nums)
        frac (partial * (/ 1 total))]
    (fn [t dur]
      (let [note-times (map (comp (partial + t) (partial frac dur))
                            (range total))
            note-duration (frac dur)]
        (flatten
         (map (fn [note time]
                ((note-event note)
                 time
                 note-duration))
              nums
              note-times))))))

;; pre-modifier
(defn mute [& rest]
  (fn [t dur]
    []))

(defn scale [s f]
  "scale event to take S * DURATION time, aligned/starting at T."
  (fn [t dur]
    (f t (* dur s))))

(defn scale-r [s f]
  "scale event to take S * DURATION time, aligned/ending at DURATION."
  (fn [t dur]
    (f (+ t (- dur (* dur s)))
       (* dur s))))

(defn stagger [s f]
  "delay event by S * DURATION time."
  (fn [t dur]
    (f (+ t (* dur s))
       dur)))

#_(defn interleave [] ;; TODO
    )

;; post-modifier
(defn volume [vol f]
  (let [vols (if (coll? vol)
               (cycle vol)
               (cycle [vol]))]
    (fn [t dur]
      (map #(assoc %1 :note-velocity %2)
           (f t dur)
           vols))))

(defn slice [fractional-t fractional-dur f]
  "select only a particular part of the events, starting at (* FRACTIONAL-T T) running for (* FRACTIONAL-DUR DUR)."
  (fn [t dur]
    ;; scale t and dur so that fractional-t and fractional-dur would come out at 0 and dur
    (let [scaled-dur (* dur (/ 1 fractional-dur))
          scaled-t (- (* fractional-t scaled-dur))
          evts (f scaled-t scaled-dur)]
      (filter #(and (<= 0 (:time %))
                    (> dur (:time %)))
              evts))))

(defn with-channel [which f]
  (fn [t dur]
    (let [evts (f t dur)]
      (map #(assoc % :midi-channel which)
           evts))))

(defn with-output [which f]
  (fn [t dur]
    (let [evts (f t dur)]
      (map #(assoc % :midi-output which)
           evts))))

(defn with-device [out chan f]
  (with-output out (with-channel chan f)))

#_(defn reverse [] nil ;; TODO
    )

;; combinatory
#_(defn- collect [some rest]
    (concat (if (coll? some)
              some
              [some])
            rest))

(defn overlay [& rest]
  "overlay a series of events to occur at the same time."
  (let [fns (flatten rest)]
    (fn [t dur]
      (flatten
       (map #(% t dur)
            fns)))))

(defn append [& rest]
  "append a series of events to occur one after another. takes care
  that the result runs from T for DUR."
  (let [fns (flatten rest)]
    (fn [t dur]
      (let [dur-fraction (/ dur (count fns))]
        (flatten
         (for [i (range (count fns))]
           ((nth fns i)
            (+ t (* i dur-fraction))
            dur-fraction)))))))

(defn glue! [& rest]
  "glues a series of events (like append), but starts them at
  T+((n-1)DUR): the first at T, the second at T+DUR, the third at
  T+2*DUR etc. the result no longer runs from T to DUR!"
  (let [fns (flatten rest)
        c (count fns)]
    (fn [t dur]
      (flatten
       (cons {:duration-notice (* c dur)}) ;; FIXME :duration-notice isn't interpreted by anyone yet.
       (for [i (range c)]
         ((nth fns i)
          (+ t (* i dur))
          dur))))))

;; distill/infuse/percolate/decoct
(comment
  (defn extract-notes [f] [::note])
  (defn extract-groove [f] [[::time ::duration]])
  (defn apply-notes [notes f] (fn [t dur] nil))
  (defn apply-groove [groove f] (fn [t dur] nil)))

(defn- extract-pitches [f]
  (let [evts (f 0 1)]
    (for [e evts
          :when (= (:type e) :note)]
      (:note-number e))))

(defn transfer-pitches [from to]
  (fn [t dur]
    (let [evts (to t dur)
          pitches (cycle (extract-pitches from))]
      (loop [es evts
             ps pitches
             accum []]
        (let [e (first es)
              p (first ps)]
          (cond (empty? e)
                accum
                (= (:type e) :note)
                (recur (rest es)
                       (rest ps)
                       (conj accum (assoc e :note-number p)))
                :else
                (recur (rest es)
                       ps
                       (conj accum e))))))))
