(ns game.state
  (:require [game.entity.field :as f]))

(def supported-player 1)
(def default-field (f/default-field))
(def default-mouse {:x 0 :y 0})
(def default-inputs #{})

(defn get-new-state
  [] 
  default-field) 

(defn get-new-mouse
  []
  (vec (repeat supported-player default-mouse)))

(defn get-new-inputs 
  [] 
  (vec (repeat supported-player default-inputs)))

(defn assign-to-player 
  [player mouse-coor inputs]
   (-> player
      (assoc :inputs inputs)
      (assoc :mouse mouse-coor)))

(defn combined-state
  [field inputs mouse]
   (assoc field :player (mapv assign-to-player (:player field) mouse inputs)))