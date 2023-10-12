(ns game.logic.enemyHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]))

(def exclusion-radius 150)

(def max-enemy 2)

(defn get-collide-damage
  [collisions]
  (reduce + (mapv :health collisions)))

(defn apply-damage
  "requires the map of get-collision data"
  [d]
  (e/damage-entity (:entity d) (get-collide-damage (:projectiles d))))

(defn get-collision-data 
  [entity projectiles]
  (let [collide-cond (fn [e] (common/colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles colliding}))

(defn shoot 
  [state entity]
  (let [timestamp (:timestamp state)
        target (common/get-target entity state)]
  (if (== timestamp (:last-shot entity)) (e/create-projectile entity target) :none)))

(defn xf-shot
  [state]
  (comp
   (map #(shoot state %))
   (filter coll?)))

(defn enemies-shoot
  [{:keys [enemies] :as t-state}
   state]
  (-> t-state
      (assoc! :e-proj (persistent!
                       (transduce (xf-shot state) conj! (transient []) enemies)))))

(defn rand-coordinates ; uses loop, to fix
  "returns a coordinate not present withing a given amount of units from the player"
  [{:keys [bounds player]}]
  (loop []
   (let [rand-x (rand (:max-x bounds))
        rand-y (rand (:max-y bounds))
        close-data (filterv #(common/closer-than-distance? {:x rand-x :y rand-y} % exclusion-radius) player)]
    (if (first close-data) (recur)
        {:x rand-x :y rand-y}))))

(defn add-enemy 
  [state]
    (let [rand-cor (rand-coordinates state)]
      ((e/random-enemy) (:x rand-cor) (:y rand-cor))))

(defn add-enemies
  [{:keys [enemies] :as t-state}
   state]
   (loop [new-enemies (transient enemies)]
    (if (<= (count new-enemies) max-enemy)
      (recur (conj! new-enemies (add-enemy state)))
      (assoc! t-state :enemies (persistent! new-enemies)))))

(defn assign-target
  [entity state]
  (assoc entity :target (common/get-target entity state)))

(defn xf-enemies
  [{:keys [timestamp p-proj speed bounds] :as state}]
    (comp
     (map #(assign-target % state))                         ;pre-calculate target
     (map #(get-collision-data % p-proj))                   ;collect collided projectiles
     (map apply-damage)                                     ;apply damage if any
     (filter e/is-alive?)                                   ;remove dead
     (map #(e/move % (e/gen-vector % (:target %)) speed))   ;move
     (map #(e/correct-position % bounds))                   ;correct out of bound
     (map #(e/update-angle % (:target %)))                  ;angle for display
     (map #(if (common/can-shoot? % state)                  ;update timestamp as a way to signify they can shoot
             (e/update-timestamp % timestamp) %))))

(defn update-current-enemies
  [t-state state]
  (assoc! t-state :enemies (persistent! 
                            (transduce (xf-enemies state) conj! 
                                       (transient []) (:enemies state)))))

(defn update-score
  [t-state state]
  (let [killed (- (count (:enemies state)) (count (:enemies t-state)))]
    (assoc! t-state :score (+ (:score state) killed))))

(defn next-tick
  [state]
  (-> (transient {})
      (update-current-enemies state)
      (update-score state)
      (enemies-shoot state)
      (add-enemies state)
      (persistent!)))