(ns game.core
  (:gen-class)
  (:require [game.window.swing :as display]
            [game.logic.controller :as controller]))

(def logic-frame-time-ms 10)
(def display-frame-time-ms 8)
(def max-coordinate 500)

(defn schedule-task [task interval-ms]
  (let [last-execution-time (atom (System/currentTimeMillis))]
    (while true
      ; the purpose is that the thread sleeps when the frame is about to get simulated too fast
      (let [current-time (System/currentTimeMillis)
            time-diff (- current-time @last-execution-time)]
        ;(when (> time-diff interval-ms) (println (str" Slow frame by " (- time-diff interval-ms) "ms")))
        (when (>= time-diff interval-ms)
          (task)
          (reset! last-execution-time current-time))
        (when (< time-diff interval-ms)
          (Thread/sleep 1))))))

(defn simulate-game []
  (controller/init-scene 0 0 max-coordinate max-coordinate)
  (schedule-task controller/next-tick logic-frame-time-ms))

(defn render-game []
  (display/init "Game") 
  (schedule-task display/display display-frame-time-ms))

(defn start-threads []
  (let [move-thread (future (simulate-game))
        display-thread (future (render-game))]
    (deref move-thread)
    (deref display-thread)))

;; launch the game
(defn -main []
  (start-threads))