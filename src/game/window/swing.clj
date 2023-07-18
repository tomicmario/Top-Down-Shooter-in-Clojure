(ns game.window.swing
  (:import [java.awt Color Graphics2D])
  (:import [java.awt.image BufferedImage])
  (:require [game.window.inputs :as im]
            [game.window.jframe :as j]
            [game.render.renderer :as r]))

(defn init [title] ; init of ui components
  (doto j/frame
    (.setTitle title)
    (.setVisible true)
    (.setLocation 0 0)
    (.setMinimumSize j/min-dimension)
    (.setDefaultCloseOperation j/exit-action)
    (.setContentPane j/panel)
    (.addKeyListener im/key-listener))

  (doto j/panel
    (.setSize j/dimension)
    (.setDoubleBuffered true)
    (.setBackground Color/WHITE)
    (.setPreferredSize j/dimension)
    (.addMouseMotionListener im/mouse-listener)
    (.addMouseListener im/click-listener))
  (.pack j/frame))

(defn display [] ; Simply displays a render of the current state
  (let [panelGraphics (.getGraphics j/panel)
        image (r/render (j/width) (j/height))]
    (doto ^Graphics2D panelGraphics
      (.drawImage ^BufferedImage image 0 0 nil))))

