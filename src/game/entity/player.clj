(ns game.entity.player
  (:require [game.entity.common :as common]
            [game.entity.projectile :as proj]))

(def pellets 6)

(def pellet-angle (/ Math/PI 30))

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

(defn rotate-point [x y x-center y-center theta]
  (let [cos (Math/cos theta)
        sin (Math/sin theta)
        dx (- x x-center)
        dy (- y y-center)
        x-new (+ x-center (* dx cos) (* dy (- sin)))
        y-new (+ y-center (* dx sin) (* dy cos))]
    {:x x-new :y y-new}))

(defn gen-target 
  [mouse player offset]
  (let [x1 (:x mouse) x2 (:x player)
        y1 (:y mouse) y2 (:y player)]
  {:target+ (rotate-point x1 y1 x2 y2 offset)
   :target- (rotate-point x1 y1 x2 y2 (- offset))}))

(defn create-projectile
  [entity mousePosition]
  (loop [projectiles []]
    (let [count-proj (count projectiles)]
      (if (<= count-proj pellets)
        (let [offset (* pellet-angle (/ count-proj 2)) 
              targets (gen-target mousePosition entity offset)
              proj+ (new-projectile (:x entity) (:y entity) (:target+ targets))
              proj- (new-projectile (:x entity) (:y entity) (:target- targets))]
          (recur (conj projectiles proj+ proj-)))
        projectiles))))

