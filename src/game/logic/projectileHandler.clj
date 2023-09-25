(ns game.logic.projectileHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]))

(defn proj-valid?
  [entity state]
  (and (common/in-bounds? entity state)
       (or (neg? (:max-ttl entity))
           (< (:timestamp state) (:max-ttl entity)))))

(defn colliding-entities?
  [a b]
  (let [max-dist (+ (/ (:width a) 2) (/ (:width b) 2))]
    (common/closer-than-distance? a b max-dist)))

(defn non-colliding-projectile? 
  [entity targets]
  (not-any? #(colliding-entities? entity %) targets))

(defn xf-proj
  [targets
   {:keys [speed] :as state}]
  (comp
   (filter #(non-colliding-projectile? % targets))
   (map #(e/move % speed))
   (filter #(proj-valid? % state))))

(defn treat-projectiles
  [projectiles target state]
  (persistent!
   (transduce (xf-proj target state) conj!
              (transient []) projectiles)))

(defn next-tick 
  [{:keys [player enemies e-proj p-proj] :as state}]
  (-> (transient {})
      (assoc! :p-proj (treat-projectiles p-proj enemies state))
      (assoc! :e-proj (treat-projectiles e-proj [player] state))
      (persistent!)))