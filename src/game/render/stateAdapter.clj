(ns game.render.stateAdapter)

(defn generate-ratio 
  [{:keys [render-bounds bounds] :as state}]
  (let [range-x (- (:max-x render-bounds) (:min-x render-bounds))
        max-range-x (- (:max-x bounds) (:min-x bounds))
        range-y (- (:max-y render-bounds) (:min-y render-bounds))
        max-range-y (- (:max-y bounds) (:min-y bounds))
        x-ratio (/ range-x max-range-x)
        y-ratio (/ range-y max-range-y)]
    (assoc state :ratio {:x x-ratio :y y-ratio})))

(defn renderable? 
  [{:keys [x y width height]} ;entity
   {:keys [min-x min-y max-x max-y ]}] ;bounds 
  (let [entity-max-x (+ x (/ width 2))
        entity-min-x (- x (/ width 2))
        entity-max-y (+ y (/ height 2))
        entity-min-y (- y (/ height 2))]
    (and (or (<= min-x entity-max-x max-x )
             (<= min-x entity-min-x max-x ))
         (or (<= min-y entity-max-y max-y)
             (<= min-y entity-min-y max-y)))))

(defn filter-renderable 
  [{:keys [render-bounds p-proj e-proj enemies] :as state}]
  (let [renderable (fn [e] (renderable? e render-bounds))]
    (-> state
        (assoc :p-proj (filterv renderable p-proj))
        (assoc :e-proj (filterv renderable e-proj))
        (assoc :enemies (filterv renderable enemies)))))

(defn adapt-ratio
  [{:keys [x y width height angle] :as entity} ; entity
   {:keys [min-x min-y max-x max-y]} ;bounds
   disp]
  (let [range-x (- max-x min-x)
        range-y (- max-y min-y)]
    (-> entity
        (assoc :x (* (/ (- x min-x) range-x) (:x disp)))
        (assoc :y (* (/ (- y min-y) range-y) (:y disp)))
        (assoc :width (* (/ width range-x) (:x disp)))
        (assoc :height (* (/ height range-y) (:y disp)))
        (assoc :angle (- (/ Math/PI 4) angle)))))

(defn adapt-entities 
  [{:keys [render-bounds display-max player e-proj p-proj enemies] :as state}]
  (let [fn (fn [e] (adapt-ratio e render-bounds display-max))]
    (-> state
        (assoc :player (fn player))
        (assoc :p-proj (mapv fn p-proj))
        (assoc :e-proj (mapv fn e-proj))
        (assoc :enemies (mapv fn enemies )))))

(defn transform-state 
  [state x y]
  (-> state
      (assoc :display-max {:x x :y y})
      (generate-ratio)
      (filter-renderable)
      (adapt-entities)))