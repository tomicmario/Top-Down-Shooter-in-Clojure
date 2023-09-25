(ns game.logic.enemyHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]
            [game.logic.enemyHandler :as enemies]))

(def exclusion-radius 150)

(def max-enemy 2)

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

(defn spawn-coordinates 
  [bounds player exclusion]
  (let [diameter (* exclusion 2)
        x (rand (- (:max-x bounds) diameter))
        y (rand (- (:max-y bounds) diameter))
        new-x (if (> (- (:x player) exclusion) x) x
                  (+ x diameter))
        new-y (if (> (- (:y player) exclusion) y) y
                  (+ y diameter))]
    {:x new-x :y new-y}))

(defn rand-coordinates 
  "returns a coordinate not present withing a given amount of units from the player"
  [{:keys [bounds player]}]
  (let [rand-x (rand (:max-x bounds))
        rand-y (rand (:max-y bounds))
        close-enough? (common/closer-than-distance? {:x rand-x :y rand-y} player exclusion-radius)]
    (if close-enough? (spawn-coordinates bounds player exclusion-radius)
        {:x rand-x :y rand-y})))

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
     (map #(assign-target % state))
     (map #(get-collision-data % p-proj)) 
     (map common/apply-damage)
     (filter e/is-alive?)
     (map #(e/move % (e/gen-vector % (:target %)) speed))
     (map #(e/correct-position % bounds))
     (map #(e/update-angle % (:target %)))
     (map #(if (common/can-shoot? % state) (e/update-timestamp % timestamp) %))))

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