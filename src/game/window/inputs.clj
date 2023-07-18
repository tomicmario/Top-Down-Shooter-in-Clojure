(ns game.window.inputs
  (:import [java.awt.event KeyAdapter KeyEvent MouseEvent])
  (:import [javax.swing.event MouseInputAdapter])
  (:require [game.state :as im]
            [game.window.jframe :as j]))

(defn set-direction [fn event] ; A bit of a hack, because switch case didn't want to work
  (let  [keycode (.getKeyCode event)]
    (when (= keycode KeyEvent/VK_W) (fn :up))
    (when (= keycode KeyEvent/VK_S) (fn :down))
    (when (= keycode KeyEvent/VK_A) (fn :left))
    (when (= keycode KeyEvent/VK_D) (fn :right))
    (when (= keycode KeyEvent/VK_R) (fn :reset))))

(def mouse-listener
  (proxy [MouseInputAdapter] []
    (mouseMoved [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m) (j/width) (j/height)))
    (mouseDragged [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m) (j/width) (j/height)))))

(def click-listener 
  (proxy [MouseInputAdapter] []
    (mousePressed [#^MouseEvent m]
      (im/add-input :click))
    (mouseReleased [#^MouseEvent m]
      (im/remove-input :click))))

(def key-listener
  (proxy [KeyAdapter] []
    ;(keyTyped [#^KeyEvent e] (handlePress e))
    (keyPressed [#^KeyEvent e] (set-direction im/add-input e))
    (keyReleased [#^KeyEvent e] (set-direction im/remove-input e))))