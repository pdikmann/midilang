(ns midilang.gear
  (:require [midilang.composition :refer :all]))

;; TR-09 Note Events

(def kick (note-event 35))
(def snare (note-event 38))
(def chat (note-event 42))

(def ltom (note-event 41))
(def mtom (note-event 45))
(def htom (note-event 48))

;; TR-09 Control Events

(def ltom-tune (partial cc-event 46))
(def mtom-tune (partial cc-event 49))
(def htom-tune (partial cc-event 52))
(defn tom-tune [val]
  (overlay (ltom-tune val)
           (mtom-tune val)
           (htom-tune val)))

(def ltom-decay (partial cc-event 47))
(def mtom-decay (partial cc-event 50))
(def htom-decay (partial cc-event 53))
(defn tom-decay [val]
  (overlay (ltom-decay val)
           (mtom-decay val)
           (htom-decay val)))
