(ns midilang.gear.common
  (:require [midilang.composition
             :refer [note-event
                     cc-event]]))

(defmacro name-note-events [pairs]
  `(do
     ~@(map (fn [[name note-num]]
              `(def ~name (note-event ~note-num)))
            (partition 2 pairs))))

(defmacro name-cc-events [pairs]
  `(do
     ~@(map (fn [[name cc-num]]
              `(def ~name (partial cc-event ~cc-num)))
            (partition 2 pairs))))

(def all-notes-off (cc-event 0x7B 0))
