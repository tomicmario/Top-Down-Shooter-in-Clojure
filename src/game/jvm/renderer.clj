(ns game.jvm.renderer
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double AffineTransform]
           [java.awt Color Graphics2D Graphics Font]
           [javax.imageio ImageIO])
  (:require [clojure.java.io :as io]
            [game.state.adapter :as state-adapter]))

(def font (Font. "TimesRoman" Font/BOLD 20))
(def state-to-display (atom nil))

(defn update-field 
  [state]
  (reset! state-to-display state))

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
  [graphics x y text color]
  (doto ^Graphics2D graphics
    (.setPaint color)
    (.setFont ^Font font)
    (.drawString ^String text ^Integer x ^Integer y))
  graphics)

(defn draw-image
  [graphics x y w h path]
  (.drawImage ^Graphics graphics path w h x y nil))

(defn draw-shape
  [graphics color shape]
  (let [c (if (nil? color) Color/RED color)]
    (doto ^Graphics2D graphics
      (.setColor c)
      (.fill shape)))
  graphics)

(defn draw-rect
  [graphics x y w h & [color]]
  (let [shape (Rectangle2D$Double. x y w h)]
    (draw-shape graphics color shape))
  graphics)

(defn draw-circle
  [graphics x y w h & [color]]
  (let [shape (Ellipse2D$Double. x y w h)]
    (draw-shape graphics color shape))
  graphics)

(defn draw-image-rotation
  [graphics
   {:keys [x y width height angle]} ;entity 
   disp]
  (let [angle (- angle Math/PI (/ Math/PI 4))
        new-x (- x (/ width 2))
        new-y (- y (/ height 2))
        old-angle (.getTransform ^Graphics2D graphics)]
    (.translate ^Graphics graphics x y)
    (.rotate ^Graphics2D graphics angle)
    (.translate ^Graphics graphics (- x) (- y))
    (draw-image ^Graphics graphics width height new-x new-y disp)
    (.setTransform ^Graphics2D graphics ^AffineTransform old-angle)
    graphics))

(defn draw-image-ent
  [graphics entity disp]
  (let [x (- (:x entity) (/ (:width entity) 2))
        y (- (:y entity) (/ (:height entity) 2))]
    (draw-image ^Graphics graphics (:width entity) (:height entity) x y disp)
    graphics))

(defmulti draw
  (fn [_ entity] [(:type entity)]))

(defmethod draw [:projectile]
  [graphics projectile]
  (let [adapted-proj (-> projectile
                         (assoc :width (+ 10 (:width projectile)))
                         (assoc :height (+ 10 (:height projectile))))]
    (draw-image-ent graphics adapted-proj projectile-image)))

(defmethod draw [:kamikaze]
  [graphics enemy]
  (draw-image-rotation graphics enemy kamikaze-image))

(defmethod draw [:shooter]
  [graphics enemy]
  (draw-image-rotation graphics enemy shooter-image))

(defmethod draw [:player]
  [graphics player]
  (draw-image-rotation graphics player player-image))

(defn get-health-ratio
  [{:keys [health max-health]}]
  (/ health max-health))

; RENDER STEPS
(defn draw-collection
  [graphics coll]
  (when (seq coll)
    (run! #(draw graphics %) coll))
  graphics)

(defn display-game-over
  [graphics state]
  (let [bounds (:display-max state)
        middle-x (/ (:x bounds) 2)
        middle-y (/ (:y bounds) 2)
        alive (filterv #(> (:health %) 0) (:player state))]
    (if (not-empty alive)
      graphics
      (draw-label graphics middle-x middle-y "Game Over" Color/BLACK))))

(defn draw-healthbar
  [graphics
   {:keys [x y width height] :as entity}]
  (let [x  (- x (/ width 2))
        y (+ (- y height) 5)
        ratio (get-health-ratio entity)
        disp-width (* ratio width)
        c (if (< ratio 0.3) Color/RED Color/GREEN)]
    (draw-rect graphics x y disp-width 5 c)))

(defn display-cursor 
  [graphics 
   {:keys [x y]}
   {:keys [display-max]}]
  (draw-circle graphics (- (* x (:x display-max)) 10) (- (* y (:y display-max)) 10) 20 20)
  graphics)

(defn draw-interface
  [graphics
   {:keys [mouse]}
   {:keys [player enemies score] :as state}]
  (run! #(draw-healthbar graphics %) enemies)
  (run! #(draw-healthbar graphics %) player)
  (-> graphics
      (draw-label 10 20 (str "Score : " score) Color/BLACK) 
      (display-game-over state)
      (display-cursor mouse state)))

(defn draw-background
  [graphics
   {:keys [render-bounds]}
   {:keys [display-max]}] ;state
  (let [disp-x (:x display-max)
        disp-y (:y display-max)
        x (:min-x render-bounds)
        y (:min-y render-bounds)
        w (- (:max-x render-bounds) (:min-x render-bounds))
        h (- (:max-y render-bounds) (:min-y render-bounds))
        sub (.getSubimage bg x y w h)]
    (.drawImage ^Graphics graphics sub 0 0 disp-x disp-y nil)
    graphics))

; Render the state, takes requires to know what the maximum resolution of the display is with x and y
(defn render
  [panelGraphics max-x max-y]
  (let [state @state-to-display]
    (if (nil? state)
      (draw-label panelGraphics 0 0 "Game is not running yet" Color/BLACK)
      (let [first-player (get (:player state) (:tracked state))
            display-state (state-adapter/transform-state first-player state max-x max-y)]
        (-> panelGraphics
            (draw-background first-player display-state)
            (draw-collection (:player display-state))
            (draw-collection (:e-proj display-state))
            (draw-collection (:p-proj display-state))
            (draw-collection (:enemies display-state))
            (draw-interface first-player display-state))))))
