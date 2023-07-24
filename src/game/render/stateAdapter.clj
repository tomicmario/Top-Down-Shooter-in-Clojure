(ns game.render.stateAdapter)

(defn within? [p min max]
  (and (>= p min) (<= p max)))

(defn generate-range [center range bounds]
  (let [full-range (* range 2)
        attempt-max (Math/min ^Double (+ center range) ^Double (:max bounds))
        attempt-min (Math/max ^Double (- center range) ^Double (:min bounds))
        min (Math/max ^Double (- attempt-max full-range) ^Double (:min bounds))
        max (Math/min ^Double (+ attempt-min full-range) ^Double (:max bounds))]
    {:min min :max max}))

(defn generate-render-bounds [center range bounds]
  (let [x-range (generate-range (:x center) (:x range) {:min (:min-x bounds) :max (:max-x bounds)})
        y-range (generate-range (:y center) (:y range) {:min (:min-y bounds) :max (:max-y bounds)})]
    {:min-x (:min x-range) :max-x (:max x-range)
     :min-y (:min y-range) :max-y (:max y-range)}))

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
        player (:player state)
        bounds (:bounds state)
        disp-bounds (generate-render-bounds player render-bound bounds)
        ratios (generate-ratio disp-bounds bounds)]
    (-> state
        (assoc :render-bounds disp-bounds)
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
      (assoc :width (/ (:width entity) (:x ratio)))
      (assoc :height (/ (:height entity) (:y ratio)))
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