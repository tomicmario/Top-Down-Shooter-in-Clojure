(ns game.jvm.swing
  (:import [java.awt Color]
           [javax.swing JFrame JPanel]
           [java.awt Dimension]
           [java.awt.event KeyAdapter KeyEvent MouseEvent]
           [javax.swing.event MouseInputAdapter])
  (:require [game.jvm.renderer :as r]))

(def mouse-position-percent (atom {:x 0 :y 0}))
(def inputs (atom #{}))

(def frame (JFrame.))
(def panel
  (proxy [JPanel] []
    (paint [g]
      (r/render g (.getWidth panel) (.getHeight panel)))))

(defn width
  []
  (.getWidth panel))

(defn height
  []
  (.getHeight panel))

(def min-dimension (Dimension. 480 360))
(def dimension (Dimension. 1000 500))
(def exit-action JFrame/EXIT_ON_CLOSE)

(defn direction [event]
  (let  [keycode (.getKeyCode event)]
    (cond (= keycode KeyEvent/VK_W) :up
          (= keycode KeyEvent/VK_S) :down
          (= keycode KeyEvent/VK_A) :left
          (= keycode KeyEvent/VK_D) :right
          (= keycode KeyEvent/VK_R) :reset
          (= keycode KeyEvent/VK_CONTROL) :slow
          :else :undefined)))

(defn update-mouse
  "Requires the maximum size of display to have interpretable positions, 
     that doesn't depend on a fixed size"
  [x y max-x max-y]
  (let [new-x (/ x max-x)
        new-y (/ y max-y)]
    (reset! mouse-position-percent {:x new-x :y new-y})))

(def mouse-listener
  (proxy [MouseInputAdapter] []
    (mouseMoved [#^MouseEvent m]
      (update-mouse (.getX m) (.getY m) (width) (height)))
    (mouseDragged [#^MouseEvent m]
      (update-mouse (.getX m) (.getY m) (width) (height)))))

(defn add-input
  [x]
  (swap! inputs conj x))

(defn remove-input
  [x]
  (swap! inputs disj x))

(def click-listener
  (proxy [MouseInputAdapter] []
    (mousePressed [#^MouseEvent m]
      (add-input :click))
    (mouseReleased [#^MouseEvent m]
      (remove-input :click))))

(def key-listener
  (proxy [KeyAdapter] []
    (keyPressed [#^KeyEvent e] (add-input (direction e)))
    (keyReleased [#^KeyEvent e] (remove-input (direction e)))))

(defn init 
  "init of ui components"
  [title]

  (doto frame
    (.setTitle title)
    (.setVisible true)
    (.setLocation 0 0)
    (.setMinimumSize min-dimension)
    (.setDefaultCloseOperation exit-action)
    (.setContentPane panel)
    (.addKeyListener key-listener))

  (doto panel
    (.setSize dimension)
    (.setDoubleBuffered true)
    (.setBackground Color/WHITE)
    (.setPreferredSize dimension)
    (.addMouseMotionListener mouse-listener)
    (.addMouseListener click-listener))
  (.pack frame))

(defn display 
  "Displays a render of the current state"
  [state]
  (r/update-field state)
  (.repaint panel))