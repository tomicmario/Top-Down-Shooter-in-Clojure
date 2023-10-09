(ns game.render.swing
  (:import [java.awt Color]
           [javax.swing JFrame JPanel]
           [java.awt Dimension]
           [java.awt.event KeyAdapter KeyEvent MouseEvent]
           [javax.swing.event MouseInputAdapter])
  (:require [game.render.renderer :as r]
            [game.state :as im]))


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
          (= keycode KeyEvent/VK_CONTROL) :slow)))

(def mouse-listener
  (proxy [MouseInputAdapter] []
    (mouseMoved [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m) (width) (height)))
    (mouseDragged [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m) (width) (height)))))

(def click-listener
  (proxy [MouseInputAdapter] []
    (mousePressed [#^MouseEvent m]
      (im/add-input :click))
    (mouseReleased [#^MouseEvent m]
      (im/remove-input :click))))

(def key-listener
  (proxy [KeyAdapter] []
    (keyPressed [#^KeyEvent e] (im/add-input (direction e)))
    (keyReleased [#^KeyEvent e] (im/remove-input (direction e)))))

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
  [] 
  (.repaint panel))