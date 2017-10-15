(ns midilang.note-math
  (:require [clojure.string :as s]))

;;                       a  b c d e f g                        
(let [note-value-lookup [9 11 0 2 4 5 7]]
  (defn char-as-note 
    "Turns characters from 'a' to 'g' to indices into
  NOTE-VALUE-LOOKUP, returns corresponding value. Modulos out-of-bound
  indices."
    [c]
    (note-value-lookup (mod (- (byte c) 97) 7))))

(let [note-name-lookup {:cb -1 :cf -1
                        :c 0
                        :c# 1 :cs 1
                        :df 1 :db 1
                        :d 2
                        :d# 3 :ds 3
                        :ef 3 :eb 3
                        :e 4
                        :fb 4 :ff 4
                        :e# 5 :es 5
                        :f 5
                        :f# 6 :fs 6
                        :gf 6 :gb 6
                        :g 7
                        :g# 8 :gs 8
                        :af 8 :ab 8
                        :a 9
                        :a# 10 :as 10
                        :bf 10 :bb 10
                        :b 11
                        :b# 12 :bs 12}]
  (defn key-as-note
    [key]
    "Turns note-name keys (like :c# or :bb) into midi note
    values (from :c as 0 to :b as 11)."
    (let [note (note-name-lookup key)]
      (if (nil? note)
        (throw (Exception. (str "Key is not the name of a note (like :c# or :bb) - " key)))
        note))))

(let [scales {:maj [0 2 4 5 7 9 11]
              :min [0 2 3 5 7 8 10]}]
  (defn index-into-scale
    "Given a scale (e.g. :maj or :min), return halftone offset from
  root for note INDEX-BASE1. The index is 1-based (starts at 1, not at
  0). Modulos out-of-bound indices."
    [scale index-base1]
    (let [the-scale (scales scale)
          scale-len (count the-scale)]
      (the-scale (mod (dec index-base1) scale-len)))))
