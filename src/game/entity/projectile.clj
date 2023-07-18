(ns game.entity.projectile 
  (:require [game.entity.common :as common]))

(defn create [x y damage radius speed vector & [max-ttl]]
  (let [proj-data (merge vector {:max-ttl max-ttl})]
    (common/entity x y damage radius radius speed :projectile proj-data)))

(defn move [entity]
  (let [vec {:vec-x (:vec-x entity) :vec-y (:vec-y entity)}
        pos (common/new-position entity vec)]
    (common/apply-position entity pos)))