(ns game.logic.enemyHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]
            [game.logic.enemyHandler :as enemies]
            [game.state :as state]
            [game.logic.partitionner :as part]))

(def exclusion-radius 150)

(def max-enemy 4000)

(defn get-collision-data 
  [entity projectiles]
  (let [collide-cond (fn [e] (common/colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles colliding}))

(defn treat-collision-enemies-partitionned
  [{:keys [p-proj-p enemies] :as state}]
  (let [near (fn [e] (part/adjacent-entities e p-proj-p))
        collision-data (mapv (fn [e] (get-collision-data e (near e))) enemies) 
        updated-enemies (mapv common/apply-damage collision-data)]
    (-> state
        (assoc :enemies updated-enemies))))

(defn treat-collision-enemies
  [{:keys [p-proj enemies] :as state}]
  (let [collision-data (mapv (fn [e] (get-collision-data e p-proj)) enemies)
        updated-enemies (mapv common/apply-damage collision-data)]
    (-> state
        (assoc :enemies updated-enemies))))

(defn get-shoot-data 
  [entity state]
  (let [timestamp (:timestamp state)
        target (common/get-target entity state)]
    (if (common/can-shoot? entity state)
      (let [updated-entity (e/update-timestamp entity timestamp)]
        {:entity updated-entity :projectiles (e/create-projectile updated-entity target)})
      {:entity entity :projectiles []})))

(defn enemies-shoot 
  [{:keys [enemies] :as state}]
  (let [proj-data (mapv (fn [e] (get-shoot-data e state)) enemies)
        shot-projectiles (common/extract-from-data :projectiles proj-data)
        updated-enemies (common/extract-from-data :entity proj-data)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :new-proj (vec (flatten shot-projectiles))))))

(defn correct-positions 
  [{:keys [bounds enemies] :as state}]
  (let [corrected-entity (fn [e] (e/correct-position e bounds))]
    (assoc state :enemies (mapv corrected-entity enemies))))

(defn move-enemies 
  [{:keys [speed enemies] :as state}]
  (let [get-entity-vec (fn [e] (e/gen-vector e (common/get-target e state)))
        move-enemy (fn [e] (e/move e (get-entity-vec e) speed))
        enemies (map move-enemy enemies)]
    (assoc state :enemies enemies)))

(defn clean-enemies 
  [{:keys [enemies score] :as state}]
  (let [updated-enemies (filterv e/is-alive? enemies)
        killed (- (count enemies) (count updated-enemies))
        new-score (+ score killed)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :score new-score))))

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
    (let [en-fn (e/random-enemy) ; enemy create function
          rand-cor (rand-coordinates state)
          new-enemy (en-fn (:x rand-cor) (:y rand-cor))]
      new-enemy))

(defn add-enemies
  [{:keys [enemies] :as state}]
  (loop [new-enemies enemies]
    (if (<=(count new-enemies) max-enemy)
      (recur (conj new-enemies (add-enemy state)))
      (assoc state :enemies new-enemies))))

(add-enemies (state/get-state))

(defn return-enemy-data 
  [{:keys [enemies new-proj score]}]
  {:enemies enemies
   :e-proj new-proj
   :score score})

(defn update-angle-enemies 
  [{:keys [enemies] :as state}]
  (let [updated-enemy (fn [e] (e/update-angle e (common/get-target e state)))]
    (assoc state :enemies (mapv updated-enemy enemies))))

(defn next-tick 
  [state]
  (-> state
      (assoc :enemies (flatten (:enemies state)))
      ;(treat-collision-enemies)
      (treat-collision-enemies-partitionned)
      (clean-enemies)
      (move-enemies)
      (correct-positions)
      (enemies-shoot)
      (add-enemies)
      (update-angle-enemies)
      (return-enemy-data)))