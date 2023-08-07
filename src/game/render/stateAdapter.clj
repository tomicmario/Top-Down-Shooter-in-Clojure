(ns game.render.stateAdapter)

(defn within? [p min max]
  (and (>= p min) (<= p max)))

(defn generate-ratio [disp-bounds bounds]
  (let [range-x (- (:max-x disp-bounds) (:min-x disp-bounds))
        max-range-x (- (:max-x bounds) (:min-x bounds))
        range-y (- (:max-y disp-bounds) (:min-y disp-bounds))
        max-range-y (- (:max-y bounds) (:min-y bounds))
        x-ratio (/ range-x max-range-x)
        y-ratio (/ range-y max-range-y)]
    {:x x-ratio :y y-ratio}))

(defn generate-display-bounds [state]
  (let [render-bound (:render-bounds state)
        bounds (:bounds state)
        ratios (generate-ratio render-bound bounds)]
    (-> state
        (assoc :ratio ratios))))

(defn renderable? [e bounds]
  (let [max-x (+ (:x e) (/ (:width e) 2))
        min-x (- (:x e) (/ (:width e) 2))
        max-y (+ (:y e) (/ (:height e) 2))
        min-y (- (:y e) (/ (:height e) 2))]
    (and (or (within? max-x (:min-x bounds) (:max-x bounds))
             (within? min-x (:min-x bounds) (:max-x bounds)))
         (or (within? max-y (:min-y bounds) (:max-y bounds))
             (within? min-y (:min-y bounds) (:max-y bounds))))))

(defn filter-renderable [state]
  (let [display-range (:render-bounds state)
        renderable (fn [e] (renderable? e display-range))]
    (-> state
        (assoc :p-proj (filterv renderable (:p-proj state)))
        (assoc :e-proj (filterv renderable (:e-proj state)))
        (assoc :enemies (filterv renderable (:enemies state))))))

(defn adapt-ratio [entity ratio bounds disp]
  (-> entity
      (assoc :x (* (/ (- (:x entity) (:min-x bounds)) (- (:max-x bounds) (:min-x bounds))) (:x disp)))
      (assoc :y (* (/ (- (:y entity) (:min-y bounds)) (- (:max-y bounds) (:min-y bounds))) (:y disp)))
      (assoc :width (/ (:width entity) (:x ratio) 0.5))
      (assoc :height (/ (:height entity) (:y ratio) 0.5))
      (assoc :angle (- (/ Math/PI 4) (:angle entity)))))

(defn adapt-entities [state]
  (let [fn (fn [e] (adapt-ratio e (:ratio state) (:render-bounds state) (:display-max state)))]
    (-> state
        (assoc :player (fn (:player state)))
        (assoc :p-proj (mapv fn (:p-proj state)))
        (assoc :e-proj (mapv fn (:e-proj state)))
        (assoc :enemies (mapv fn (:enemies state))))))

(defn transform-state [state x y]
  (-> state
      (assoc :display-max {:x x :y y})
      (generate-display-bounds)
      (filter-renderable)
      (adapt-entities)))