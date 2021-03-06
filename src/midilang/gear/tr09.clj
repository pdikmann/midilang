(ns midilang.gear.tr09
  "define control events for Roland TR-09 device."
  (:require [midilang.gear.common :refer [name-note-events
                                          name-cc-events]]
            [midilang.composition :refer [overlay]]))

;; TR-09 Note Events
(name-note-events
 [kick  35 bd 35
  snare 38 sd 38
  ltom  41 lt 41
  mtom  45 mt 45
  htom  48 ht 48
  rim   37 rs 37
  clap  39 cp 39 hc 39
  chat  42 ch 42
  ohat  46 oh 46
  crash 49 cr 49 cc 49
  ride  51 rd 51 rc 51])

;; TR-09 Control Events
(name-cc-events
 [shuffle      9
  bd-tune      20 bd-attack 21 bd-comp   22 bd-decay 23 bd-level 24
  sd-tune      25 sd-snappy 26 sd-comp   27 sd-tone  28 sd-level 29
  lt-tune      46 lt-decay  47 lt-level  48
  mt-tune      49 mt-decay  50 mt-level  51
  ht-tune      52 ht-decay  53 ht-level  54
  rs-tune      55 rs-decay  56 rs-level  57
  hc-tune      58 hc-decay  59 hc-level  60
  ch-tune      61 ch-decay  62 ch-level  63
  total-accent 71 accent    71
  oh-tune      80 oh-decay  81 oh-level  82
  cc-tune      83 cc-decay  84 cc-level  85
  rc-tune      86 rc-decay  87 rc-level  88])

(defn tom-tune [val]
  (overlay (lt-tune val)
           (mt-tune val)
           (ht-tune val)))

(defn tom-decay [val]
  (overlay (lt-decay val)
           (mt-decay val)
           (ht-decay val)))
