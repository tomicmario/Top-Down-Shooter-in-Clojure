(ns game.entity.player
  (:require [game.entity.common :as common])
  (:require [game.entity.projectile :as proj]))

(def pellets 6)

(defn move 
  [entity vector speed]
  (common/default-move entity vector speed))

(defn default-player 
  [x y]
  (common/entity x y 100 20 20 1 :player {:last-shot -100 :firerate 30}))

(defn new-projectile
  [x y target]
  (let [proj (proj/create x y 10 5 3 nil)
        vec (common/gen-vector proj target)]
    (merge proj vec)))

(defn gen-target 
  [{:keys [x y]} ;target
   offset]
  {:target+ {:x (+ offset x) :y (+ offset y)}
   :target- {:x (- x offset) :y (- y offset)}})

(defn create-projectile
  [entity mousePosition]
  (loop [projectiles []]
    (let [count-proj (count projectiles)]
      (if (<= count-proj pellets)
        (let [offset (* 10 (/ count-proj 2))
              targets (gen-target mousePosition offset)
              proj+ (new-projectile (:x entity) (:y entity) (:target+ targets))
              proj- (new-projectile (:x entity) (:y entity) (:target- targets))]
          (recur (conj projectiles proj+ proj-)))
        projectiles))))
    