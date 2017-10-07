(ns midilang.gear.tb03
  "define control events for Roland TB-03 device."
  (:require [midilang.gear.common :refer [name-cc-events]]))

;; TB-03 Control Events
(name-cc-events
 [env-mod        12 env       12
  accent-level   16 tb-accent 16
  overdrive      17
  delay-time     18
  delay-feedback 19
  resonance      71 reso      71 res 71
  cutoff-freq    74 cutoff    74 cut 74
  decay          75
  slide-status   102 slide    102
  tuning         104 tb-tune  104])
