(ns game.logic.controller
  (:require [game.logic.projectileHandler :as proj]
            [game.logic.playerHandler :as player]
            [game.logic.enemyHandler :as enemies]
            [game.state :as state]))

(def min-range 120)
(def max-range 150)
(def range-diff-frame 3)

(defn apply-and-keep-in-bounds [num min max op diff]
  (Math/min (Math/max ^Double (op num diff) ^Double min) ^Double max))

(defn transform-render-range [state]
  (let [op (if (contains? (:inputs state) :slow) - +)
        x-range (:x (:render-range state))
        y-range (:y (:render-range state))
        x (apply-and-keep-in-bounds x-range min-range max-range op range-diff-frame)
        y (apply-and-keep-in-bounds y-range min-range max-range op range-diff-frame)]
    {:x x :y y}))

(defn generate-range [center range bounds]
  (let [full-range (* range 2)
        attempt-max (Math/min ^Double (+ center range) ^Double (:max bounds))
        attempt-min (Math/max ^Double (- center range) ^Double (:min bounds))
        min (Math/max ^Double (- attempt-max full-range) ^Double (:min bounds))
        max (Math/min ^Double (+ attempt-min full-range) ^Double (:max bounds))]
    {:min min :max max}))

(defn generate-render-bounds [state]
  (let [center (:player state)
        bounds (:bounds state)
        range (transform-render-range state)
        x-range (generate-range (:x center) (:x range) {:min (:min-x bounds) :max (:max-x bounds)})
        y-range (generate-range (:y center) (:y range) {:min (:min-y bounds) :max (:max-y bounds)})
        render-bounds {:min-x (:min x-range) :max-x (:max x-range)
                       :min-y (:min y-range) :max-y (:max y-range)}]
    (-> state 
        (assoc :render-bounds render-bounds)
        (assoc :render-range range))))

(defn unify-data [state proj-data player-data enemy-data]
  (let [p-proj (concat (:p-proj proj-data) (:p-proj player-data))
        e-proj (concat (:e-proj proj-data) (:e-proj enemy-data))]
    (-> state
        (assoc :player (:player player-data))
        (assoc :enemies (:enemies enemy-data))
        (assoc :score (:score enemy-data))
        (assoc :p-proj p-proj)
        (assoc :e-proj e-proj))))

(defn translate-mouse-position [state]
  (let [bounds (:render-bounds state)
        x (* (:x (:mouse state)) (- (:max-x bounds) (:min-x bounds)))
        y (* (:y (:mouse state))(- (:max-y bounds) (:min-y bounds)))]
    (assoc state :mouse {:x (+ x (:min-x bounds)) :y (+ y (:min-y bounds))})))

(defn generate-next-tick[state]
  (let [proj-data (future (proj/next-tick state))
        player-data (future (player/next-tick state))
        enemy-data (future (enemies/next-tick state))
        new-speed (if (contains? (:inputs state) :slow) 0.2 1)]
    (-> state
        (unify-data (deref proj-data) (deref player-data) (deref enemy-data))
        (assoc :speed new-speed))))

(defn init-scene[min-x min-y max-x max-y]
  (let [new-bounds {:min-x min-x :min-y min-y
                    :max-x max-x :max-y max-y}]
    (-> (state/get-state)
        (assoc :bounds new-bounds)
        (state/update-state))))

; ENTIRE FRAME LOGIC
(defn next-tick []
  (-> (state/get-state)
      (generate-render-bounds)
      (translate-mouse-position)
      (generate-next-tick)
      (state/update-state)))

(next-tick)