(ns game.entity.common)

(defn apply-position [entity pos]
  (-> entity
      (assoc :x (:x pos))
      (assoc :y (:y pos))))

(defn entity [x y health width height speed type & [more]]
  (merge {:x x :y y :health health :speed speed
          :width width :height height :type type :angle 0 :max-health health} more))

(defn new-position [entity vector speed]
  {:x (+ (:x entity) (* speed (:vec-x vector)))
   :y (+ (:y entity) (* speed (:vec-y vector)))})

(defn calculate-angle [a b]
  (Math/atan2 (- (:x a) (:x b)) (- (:y a) (:y b))))

(defn gen-vector [entity target]
  (let [speed (:speed entity)
        angle (- (/ Math/PI 4) (calculate-angle target entity))
        vec-x (- (* (Math/cos angle) speed) (* (Math/sin angle) speed))
        vec-y (+ (* (Math/sin angle) speed) (* (Math/cos angle) speed))]
    {:vec-x vec-x :vec-y vec-y}))

(defn default-move [entity vector speed]
  (let [spd (if (nil? speed) 1 speed)
        pos (new-position entity vector spd)]
    (apply-position entity pos)))
