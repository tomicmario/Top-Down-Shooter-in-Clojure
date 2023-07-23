(ns game.state
  (:require [game.entity.entities :as e]
            [game.state :as state]))

(def bounds {:min-x 0.0 :min-y 0.0 :max-x 500.0 :max-y 500.0})

(def display-range {:x 250 :y 250})

(defn default-player []
  (let [x (/ (:max-x bounds) 2)
        y (/ (:max-y bounds) 2)]
  (e/default-player x y)))

(defn default-state []
  {:player (default-player) :p-proj [] :e-proj []
   :enemies [] :timestamp 0 :bounds bounds :score 0 :speed 1
   :display-range display-range})

; STATE VARIABLES
(def inputs (atom #{}))
(def mouse (atom {:x 0 :y 0}))
(def entity-state (atom (default-state)))

(defn get-state [] 
  (let [inputs @inputs
        mouse @mouse
        entities @entity-state]
    (merge entities {:inputs inputs :mouse mouse})))

; INPUTS UPDATE
(defn add-input [x]
  (swap! inputs conj x))

(defn remove-input [x]
  (swap! inputs disj x))

(defn update-mouse [x y max-x max-y] 
  ; Requires the maximum size of display to have interpretable positions, 
  ; that doesn't depend on a fixed size
  (let [new-x (/ x max-x)
        new-y (/ y max-y)]
  (swap! mouse assoc :x new-x)
  (swap! mouse assoc :y new-y)))
; END INPUTS UPDATE

; GLOBAL STATE UPDATE
(defn default-entity-state []
  (reset! entity-state (default-state)))

(defn reset []
  (default-entity-state))

(defn clean-state [state]
  (-> state
      (assoc :timestamp (+ (:timestamp state) (:speed state)))
      (dissoc :inputs)
      (dissoc :mouse)
      (assoc :bounds (:bounds state))))

(defn save-state [state] 
    (reset! entity-state (clean-state state)))

(defn update-state [state]
  (if (contains? (:inputs state) :reset)
    (reset)
    (save-state state)))