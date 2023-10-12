(ns game.logic.controller
  (:require [game.logic.projectileHandler :as proj]
            [game.logic.playerHandler :as player]
            [game.logic.enemyHandler :as enemies]
            [game.entity.field :as f]))

(def range-diff-frame 3)

(defn apply-and-keep-in-bounds [num min max op diff]
  (Math/min (Math/max ^Double (op num diff) ^Double min) ^Double max))

(defn transform-render-range
  [{:keys [speed render-range]}]
  (let [op (if (= speed 1) + -)
        x (apply-and-keep-in-bounds (:x render-range) f/min-range f/max-range op range-diff-frame)
        y (apply-and-keep-in-bounds (:y render-range) f/min-range f/max-range op range-diff-frame)]
    {:x x :y y}))

(defn generate-range
  [center range bounds]
  (let [full-range (* range 2)
        attempt-max (Math/min ^Double (+ center range) ^Double (:max bounds))
        attempt-min (Math/max ^Double (- center range) ^Double (:min bounds))
        min (Math/max ^Double (- attempt-max full-range) ^Double (:min bounds))
        max (Math/min ^Double (+ attempt-min full-range) ^Double (:max bounds))]
    {:min min :max max}))

(defn generate-render-bounds-player
  [player range
   {:keys [bounds]}]
  (let [x-range (generate-range (:x player) (:x range) {:min (:min-x bounds) :max (:max-x bounds)})
        y-range (generate-range (:y player) (:y range) {:min (:min-y bounds) :max (:max-y bounds)})
        render-bounds {:min-x (:min x-range) :max-x (:max x-range)
                       :min-y (:min y-range) :max-y (:max y-range)}]
    (-> player
        (assoc :render-bounds render-bounds)
        (assoc :render-range range))))

(defn generate-render-bounds
  [{:keys [player] :as state}]
  (let [range (transform-render-range state)]
    (-> state
        (assoc :render-range range) 
        (assoc :player (mapv #(generate-render-bounds-player % range state) player)))))

(defn unify-data
  [state proj-data player-data enemy-data]
  (let [p-proj (concat (:p-proj proj-data) (:p-proj player-data))
        e-proj (concat (:e-proj proj-data) (:e-proj enemy-data))]
    (-> (transient state)
        (assoc! :player (:player player-data))
        (assoc! :enemies (:enemies enemy-data))
        (assoc! :score (:score enemy-data))
        (assoc! :p-proj p-proj)
        (assoc! :e-proj e-proj)
        (persistent!))))

(defn translate-mouse-position-player
  [{:keys [render-bounds mouse] :as player}]
  (let [{:keys [min-x min-y max-x max-y]} render-bounds
        x (* (:x mouse) (- max-x min-x))
        y (* (:y mouse) (- max-y min-y))]
    (assoc player :mouse {:x (+ x min-x) :y (+ y min-y)})))

(defn translate-mouse-position
  [state]
  (assoc state :player (mapv translate-mouse-position-player (:player state))))

(defn generate-next-tick
  [state]
  (let [proj-data (proj/next-tick state)
        player-data (player/next-tick state)
        enemy-data (enemies/next-tick state)]
    (-> state
        (unify-data proj-data player-data enemy-data))))

(defn alter-speed 
  [state]
   (let [slow (filter #(contains? (:inputs %) :slow) (:player state))]
     (if (first slow)
       (assoc state :speed 0.2)
       (assoc state :speed 1))))

(defn reset-if-necessary
  [state]
 (let [reset (filter #(contains? (:inputs %) :reset) (:player state))] 
  (if (first reset) (f/default-field) state)))

(defn next-tick
  [state]
  (-> state
      (generate-render-bounds)
      (translate-mouse-position)
      (generate-next-tick)
      (assoc :timestamp (+ (:timestamp state) (:speed state)))
      (alter-speed)
      (reset-if-necessary)))