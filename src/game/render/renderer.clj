(ns game.render.renderer
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double AffineTransform])
  (:import [java.awt Color Graphics2D Graphics Font])
  (:import [javax.imageio ImageIO])
  (:require [game.state :as state]
            [clojure.java.io :as io]
            [game.render.stateAdapter :as state-adapter]))

(def font (Font. "TimesRoman" Font/BOLD 20))

; ENTITY IMAGES
(defn get-image-from-file 
  [path]
  (ImageIO/read (io/file path)))

(def bg (get-image-from-file "resources/Space001.png"))

(def player-image (get-image-from-file "resources/player_ship.PNG"))

(def projectile-image (get-image-from-file "resources/energy_ball.png"))

(def kamikaze-image (get-image-from-file "resources/kamikaze.png"))

(def shooter-image (get-image-from-file "resources/shooter.png"))
; END ENTITY IMAGES

; BASIC DRAWING FUNCTIONS
(defn draw-label 
  [image x y text color]
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setPaint color)
      (.setFont ^Font font)
      (.drawString ^String text ^Integer x ^Integer y)))
  image)

(defn draw-image 
  [image x y w h path]
    (.drawImage ^Graphics image path w h x y nil))

(defn new-image 
  [x y]
  (let [image (BufferedImage. x y BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor Color/WHITE)
      (.fill (Rectangle2D$Double. 0 0 x y)))
    (draw-image ^Graphics graphics x y 0 0 bg)
    image))

(defn draw-shape 
  [image color shape]
  (let [c (if (nil? color) Color/RED color)
        graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor c)
      (.fill shape)))
  image)

(defn draw-rect 
  [image x y w h & [color]]
  (let [shape (Rectangle2D$Double. x y w h)]
    (draw-shape image color shape))
  image)

(defn draw-image-rotation 
  [image 
   {:keys [x y width height angle]} ;entity 
   disp]
  (let [angle (- angle Math/PI (/ Math/PI 4))
        new-x (- x (/ width 2))
        new-y (- y (/ height 2))
        graphics (.createGraphics image)
        old-angle (.getTransform ^Graphics2D graphics)]
    (.translate ^Graphics graphics x y)
    (.rotate ^Graphics2D graphics angle)
    (.translate ^Graphics graphics (- x) (- y))
    (draw-image ^Graphics graphics width height new-x new-y disp)
    (.setTransform ^Graphics2D graphics ^AffineTransform old-angle)
    image))

(defn draw-image-ent 
  [image entity disp]
  (let [x (- (:x entity) (/ (:width entity) 2))
        y (- (:y entity) (/ (:height entity) 2))
        graphics (.createGraphics image)]
    (draw-image ^Graphics graphics (:width entity) (:height entity) x y disp) 
    image))

(defmulti draw 
  (fn [image entity] [(:type entity)]))

(defmethod draw [:projectile]
  [image projectile]
  (let [adapted-proj (-> projectile
                         (assoc :width (+ 10 (:width projectile)))
                         (assoc :height (+ 10 (:height projectile))))]
    (draw-image-ent image adapted-proj projectile-image)))

(defmethod draw [:kamikaze] 
  [image enemy]
  (draw-image-rotation image enemy kamikaze-image))

(defmethod draw [:shooter] 
  [image enemy]
  (draw-image-rotation image enemy shooter-image))

(defmethod draw [:player] 
  [image player]
  (draw-image-rotation image player player-image))

(defn get-health-ratio 
  [{:keys [health max-health]}]
  (/ health max-health))

; RENDER STEPS
(defn draw-collection 
  [image coll]
  (let [reducer (fn [e] (draw image e))]
    (when-not (or (nil? coll) (empty? coll))
      (run! reducer coll)))
  image)

(defn display-game-over 
  [image state]
  (let [bounds (:display-max state)
        middle-x (/ (:x bounds) 2)
        middle-y (/ (:y bounds) 2)]
    (if (> (:health (:player state)) 0)
      image
      (draw-label image middle-x middle-y "Game Over" Color/BLACK))))

(defn draw-healthbar 
  [image
   {:keys [x y width height] :as entity}] 
  (let [x  (- x (/ width 2))
        y (+ (- y height) 5)
        ratio (get-health-ratio entity)
        disp-width (* ratio width)
        c (if (< ratio 0.3) Color/RED Color/GREEN)]
    (draw-rect image x y disp-width 5 c)))

(defn draw-interface 
  [image
   {:keys [player enemies score] :as state}]
    (run! (fn [e] (draw-healthbar image e)) enemies)
    (-> image
        (draw-healthbar player)
        (draw-label 10 20 (str "Score : " score) Color/BLACK)
        (display-game-over state)))

(defn sub-image 
  [image 
   {:keys [ratio bounds render-bounds display-max]}] ;state
  (let [disp-x (:x display-max)
        disp-y (:y display-max)
        w (* (:x ratio) disp-x)
        h (* (:y ratio) disp-y)
        x (* (/ (:min-x render-bounds) (:max-x bounds)) disp-x)
        y (* (/ (:min-y render-bounds) (:max-y bounds)) disp-y)
        sub (.getSubimage image x y w h)
        new (new-image disp-x disp-y)]
    (.drawImage ^Graphics (.createGraphics new) sub 0 0 disp-x disp-y nil)
    new))

; Render the state, takes requires to know what the maximum resolution of the display is with x and y
(defn render 
  [x y]
  (let [raw-state (state/get-state)
        display-state (state-adapter/transform-state raw-state x y)]
    (-> (new-image x y)
        (sub-image display-state)
        (draw (:player display-state))
        (draw-collection (:e-proj display-state))
        (draw-collection (:p-proj display-state))
        (draw-collection (:enemies display-state))
        (draw-interface display-state))))

(render 1000 1000)