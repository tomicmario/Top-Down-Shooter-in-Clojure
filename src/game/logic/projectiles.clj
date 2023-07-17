(ns game.logic.projectiles
   (:require [game.entities :as e]
             [game.state :as state]))

(defn extract-from-data [type data] ; util function for pairs, filters nils
  (filterv identity (mapv type data)))

(defn closer-than-distance? [a b d]
  ; Simplified version not using sqrt
  (let [dist (* d d)
        distY (* (- (:y a) (:y b)) (- (:y a) (:y b)))
        distX (* (- (:x a) (:x b)) (- (:x a) (:x b)))]
    (> dist (+ distX distY))))

(defn in-bounds? [entity state]
  (let [bounds (:bounds state)]
    (and (>= (:x entity) (:min-x bounds)) (>= (:y entity) (:min-y bounds))
         (<= (:x entity) (:max-x bounds)) (<= (:y entity) (:max-y bounds)))))

(defn proj-valid? [entity state]
  (let [ttl (:max-ttl entity)]
    (if (nil? ttl) true (< (:timestamp state) ttl))))

(defn clean-projectiles [state]
  (let [is-valid? (fn [p] (and (proj-valid? p state) (in-bounds? p state)))]
    (-> state
        (assoc :p-proj (filterv is-valid? (:p-proj state)))
        (assoc :e-proj (filterv is-valid? (:e-proj state))))))

(defn colliding? [a b]
  (let [max-dist (+ (/ (:width a) 2) (/ (:width b) 2))]
    (closer-than-distance? a b max-dist)))

(defn get-collision-data [entity projectiles]
  (let [collide-cond (fn [e] (colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles (if (empty? colliding) [] colliding)}))

(defn remove-collided [projectiles collided] ; requires the map of get-collision data on collided
  (let [colliding-proj (into #{} collided)
        has-not-collided? (fn [p] (not (contains? colliding-proj p)))]
    (filterv has-not-collided? projectiles)))

(defn treat-collision-player [state]
  (let [collided-proj (get-collision-data (:player state) (:e-proj state))
        new-proj (remove-collided (:e-proj state) (:projectiles collided-proj))]
        (assoc state :e-proj new-proj)))

(defn treat-collision-enemies [state]
  (let [collision-data (mapv (fn [e] (get-collision-data e (:p-proj state))) (:enemies state))
        collided-proj  (flatten (extract-from-data :projectiles collision-data))
        new-proj (remove-collided (:p-proj state) collided-proj)]
        (assoc state :p-proj new-proj))) 

(defn move-proj [state]
  (let [e-proj (mapv e/move (:e-proj state))
        p-proj (mapv e/move (:p-proj state))]
    (-> state
        (assoc :e-proj e-proj)
        (assoc :p-proj p-proj))))

(defn return-projectiles [state] 
  {:p-proj (:p-proj state) 
   :e-proj (:e-proj state)})

(defn next-tick [state]
  (-> state
      (clean-projectiles)
      (treat-collision-enemies)
      (treat-collision-player)
      (move-proj)
      (return-projectiles)))

(next-tick (state/get-state))