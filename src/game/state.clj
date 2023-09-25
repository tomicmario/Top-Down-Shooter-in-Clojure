(ns game.state
  (:require [game.entity.field :as f]))

; STATE VARIABLES
(def inputs (atom #{}))
(def mouse (atom {:x 0 :y 0}))
(def entity-state (atom (f/default-field)))

(defn get-state  
  []
  (let [inputs @inputs
        mouse @mouse
        entities @entity-state]
    (-> entities
        (assoc :inputs inputs)
        (assoc :mouse mouse))))

; INPUTS UPDATE
(defn add-input 
  [x]
  (swap! inputs conj x))

(defn remove-input 
  [x]
  (swap! inputs disj x))

(defn update-mouse
  "Requires the maximum size of display to have interpretable positions, 
     that doesn't depend on a fixed size"
  [x y max-x max-y]
  (let [new-x (/ x max-x)
        new-y (/ y max-y)]
    (swap! mouse assoc :x new-x)
    (swap! mouse assoc :y new-y)))
; END INPUTS UPDATE

(defn reset 
  []
  (reset! entity-state (f/default-field)))

(defn save-state 
  [state] 
    (reset! entity-state 
            (assoc state :timestamp (+ (:timestamp state) (:speed state)))))

(defn update-state 
  [state]
  (if (contains? (:inputs state) :reset)
    (reset)
    (save-state state)))