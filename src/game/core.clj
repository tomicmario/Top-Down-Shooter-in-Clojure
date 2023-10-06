(ns game.core
  (:gen-class)
  (:require [game.window.swing :as display]
            [game.logic.controller :as controller]
            [clj-async-profiler.core :as prof]))

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
  (display/init "Game") 
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
  ;(display/init "Game")
  ;(prof/profile (fn-to-profile))
  (start-threads)
  )


