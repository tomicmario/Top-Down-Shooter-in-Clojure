(ns game.logic.partitionner)

(def cell-size 100)

(defn upper 
  [number]
  (int (Math/ceil number)))

(defn lower
  [number]
  (int (Math/floor number)))

(defn gen-indexes [coll-x coll-y max-x]
  (vec (for [x coll-x
             y coll-y]
         (+ x (* max-x y)))))

(defn adjacent-indexes [x y max-x max-y]
  (let [possible-x [(dec x) x (inc x)]
        possible-y [(dec y) y (inc y)]
        fn-x (fn [x] (< -1 x max-x))
        fn-y (fn [y] (< -1 y max-y))]
    (gen-indexes (filterv fn-x possible-x)
                 (filterv fn-y possible-y)
                 max-x)))

(defn empty-partition-coll
  [amount]
  (loop [i 0 c (transient [])]
    (if (>= i amount)
      (persistent! c)
      (recur (inc i) (conj! c [])))))

(defn adjacent-entities
  [{:keys [x y]}
   {:keys [cell-size coll cells-x cells-y]}]
  (let [px (lower (/ x cell-size))
        py (lower (/ y cell-size))
        indexes (adjacent-indexes px py cells-x cells-y)]
    ;(println indexes)
    (vec (flatten
          (mapv #(nth coll %) indexes)))))

(defn new-partition [cell-size coll cells-x cells-y ]
  {:cell-size cell-size :coll coll :cells-x cells-x :cells-y cells-y})

(defn partitionned-coll
  [{:keys [max-x max-y min-x min-y]}]
  (let [cells-x (upper (/ (- max-x min-x) cell-size))
        cells-y (upper (/ (- max-y min-y) cell-size))
        amount (* cells-x cells-y)
        coll (empty-partition-coll amount)]
    (new-partition cell-size coll cells-x cells-y)))

(defn to-coordinate [x cell-size max]
  (let [converted (int (/ x cell-size))]
    (Math/max (Math/min max converted) 0)))

(defn- partition-coll
  [partitionned flat-coll cells-x cells-y]
  (loop [new-coll  partitionned
         ;new-coll (mapv #(transient %) partitionned)
         element flat-coll]
    (if (empty? element)
      ;(new-partition cell-size (mapv #(persistent! %) new-coll) cells-x cells-y)
      (new-partition cell-size  new-coll cells-x cells-y)
      (let [e (first element)
            x-converted (to-coordinate (:x e) cell-size cells-x)
            y-converted (to-coordinate (:y e) cell-size cells-y)
            pos (+ (* y-converted cells-y) x-converted)
            updated-tile (conj (subvec new-coll pos (inc pos)) e)
                    ;_ (println pos (count new-coll) (:x e) (:y e))
            updated (assoc new-coll pos updated-tile)]
        (recur updated (rest element))))))

(defn store-to-partition
  [{:keys [cell-size cells-x cells-y]}
   flat-coll]
  (let [amount (* cells-x cells-y)
        partitionned (empty-partition-coll amount)]
    (if (seq flat-coll)
      (partition-coll partitionned flat-coll cells-x cells-y)
      (new-partition cell-size partitionned cells-x cells-y))))