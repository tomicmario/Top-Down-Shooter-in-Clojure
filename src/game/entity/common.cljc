(ns game.entity.common)

(defn apply-position 
  [entity pos]
  (-> entity
      (assoc :x (:x pos))
      (assoc :y (:y pos))))

(defn entity 
  [x y health width height speed type & [more]]
  (merge {:x x :y y :health health :speed speed
          :width width :height height :type type :angle 0 :max-health health} more))

(defn new-position
  [{:keys [x y]} ;entity
   {:keys [vec-x vec-y]} ;vector
   speed]
  {:x (+ x (* speed vec-x))
   :y (+ y (* speed vec-y))})

(defn calculate-angle
  [a b]
  (Math/atan2 (- (:x a) (:x b)) (- (:y a) (:y b))))

(defn gen-vector 
  [entity target]
  (let [speed (:speed entity)
        angle (- (/ Math/PI 4) (calculate-angle target entity))
        vec-x (- (* (Math/cos angle) speed) (* (Math/sin angle) speed))
        vec-y (+ (* (Math/sin angle) speed) (* (Math/cos angle) speed))]
    {:vec-x vec-x :vec-y vec-y}))

(defn default-move 
  [entity vector speed]
  (let [spd (or speed 1)
        pos (new-position entity vector spd)]
    (if (not= (:type (:target entity)) (:type entity)) 
      (apply-position entity pos)
      entity)))
