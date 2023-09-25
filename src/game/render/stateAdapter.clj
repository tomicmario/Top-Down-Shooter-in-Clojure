(ns game.render.stateAdapter)

(defn generate-ratio
  [render-bounds bounds]
  (let [range-x (- (:max-x render-bounds) (:min-x render-bounds))
        max-range-x (- (:max-x bounds) (:min-x bounds))
        range-y (- (:max-y render-bounds) (:min-y render-bounds))
        max-range-y (- (:max-y bounds) (:min-y bounds))
        x-ratio (/ range-x max-range-x)
        y-ratio (/ range-y max-range-y)]
    {:x x-ratio :y y-ratio}))

(defn renderable?
  [{:keys [x y width height]} ;entity
   {:keys [min-x min-y max-x max-y]}] ;bounds 
  (let [entity-max-x (+ x (/ width 2))
        entity-min-x (- x (/ width 2))
        entity-max-y (+ y (/ height 2))
        entity-min-y (- y (/ height 2))]
    (and (or (<= min-x entity-max-x max-x)
             (<= min-x entity-min-x max-x))
         (or (<= min-y entity-max-y max-y)
             (<= min-y entity-min-y max-y)))))

(defn adapt-ratio
  [{:keys [x y width height angle] :as entity} ; entity
   {:keys [min-x min-y max-x max-y]} ;bounds
   disp]
  (let [range-x (- max-x min-x)
        range-y (- max-y min-y)]
    (-> (transient entity)
        (assoc! :x (* (/ (- x min-x) range-x) (:x disp)))
        (assoc! :y (* (/ (- y min-y) range-y) (:y disp)))
        (assoc! :width (* (/ width range-x) (:x disp)))
        (assoc! :height (* (/ height range-y) (:y disp)))
        (assoc! :angle (- (/ Math/PI 4) angle))
        (persistent!))))

(defn xf-adapter
  [{:keys [render-bounds display-max]}]
  (comp
   (filter #(renderable? % render-bounds))
   (map #(adapt-ratio % render-bounds display-max))))

(defn adapt-coll
  [coll state]
  (persistent!
   (transduce (xf-adapter state) conj! (transient []) coll)))

(defn transform-state
  [{:keys [p-proj e-proj enemies player render-bounds bounds] :as state}
   x y]
  (let [render-helper {:display-max {:x x :y y}
                       :bounds bounds :render-bounds render-bounds
                       :ratio (generate-ratio render-bounds bounds)}]
    (-> (transient state)
        (assoc! :display-max (:display-max render-helper))
        (assoc! :ratio (:ratio render-helper))
        (assoc! :p-proj (adapt-coll p-proj render-helper))
        (assoc! :e-proj (adapt-coll e-proj render-helper))
        (assoc! :enemies (adapt-coll enemies render-helper))
        (assoc! :player (adapt-ratio player render-bounds (:display-max render-helper)))
        (persistent!))))