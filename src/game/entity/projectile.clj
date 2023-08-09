(ns game.entity.projectile 
  (:require [game.entity.common :as common]))

(defn create 
  [x y damage radius speed vector & [max-ttl]]
  (let [proj-data (merge vector {:max-ttl max-ttl})]
    (common/entity x y damage radius radius speed :projectile proj-data)))

(defn move 
  [entity speed]
  (let [vec {:vec-x (:vec-x entity) :vec-y (:vec-y entity)}
        spd (if (nil? speed) 1 speed)
        pos (common/new-position entity vec spd)]
    (common/apply-position entity pos)))