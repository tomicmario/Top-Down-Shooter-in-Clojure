(ns game.window.jframe
  (:import [javax.swing JFrame JPanel])
  (:import [java.awt Dimension]))

(def frame (JFrame.))
(def panel (JPanel.))
(def min-dimension (Dimension. 480 360))
(def dimension (Dimension. 1000 500))
(def exit-action JFrame/EXIT_ON_CLOSE)

(defn width 
  []
  (.getWidth panel))

(defn height 
  []
  (.getHeight panel))