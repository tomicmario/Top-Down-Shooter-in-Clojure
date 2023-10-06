(ns game.core
  (:gen-class)
  (:require [game.render.swing :as display]
            [clj-async-profiler.core :as prof]
            [game.logic.controller :as controller]))

(def profiling false)
(def logic-frame-time-ms 10)
(def display-frame-time-ms 8)

(defn schedule-task
  [task interval-ms]
  (loop [last (System/currentTimeMillis)]
    ; the purpose is that the thread sleeps when the frame is about to get simulated too fast
    (let [current-time (System/currentTimeMillis)
          time-diff (- current-time last)
          last-render-time (if (>= time-diff interval-ms) current-time last)]
      ;(when (> time-diff interval-ms) (println (str " Slow frame by " (- time-diff interval-ms) "ms")))
      (if (>= time-diff interval-ms) (task)
          (Thread/sleep 1))
      (recur last-render-time))))

(defn simulate-game
  []
  (schedule-task controller/next-tick logic-frame-time-ms))

(defn render-game
  [] 
  (schedule-task display/display display-frame-time-ms))


(defn start-threads
  []
  (let [move-thread (future (simulate-game))
        display-thread (future (render-game))]
    (deref move-thread)
    (deref display-thread)))

(defn fn-to-profile
  []
  (dotimes [_ 200]
    ((controller/next-tick)
     (display/display))))

(defn -main
  "launch the game"
  []
  (display/init "Game")
  (if profiling
    (prof/profile (fn-to-profile))
    (start-threads)))