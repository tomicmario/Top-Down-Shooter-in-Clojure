(ns game.window.inputs
  (:import [java.awt.event KeyAdapter KeyEvent MouseEvent])
  (:import [javax.swing.event MouseInputAdapter])
  (:require [game.state :as im]
            [game.window.jframe :as j]))

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
    (keyPressed [#^KeyEvent e] (im/add-input (direction e)))
    (keyReleased [#^KeyEvent e] (im/remove-input (direction e)))))