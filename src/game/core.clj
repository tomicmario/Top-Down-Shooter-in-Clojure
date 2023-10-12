(ns game.core 
  (:gen-class)
  (:require [game.jvm.swing :as display]
            [clj-async-profiler.core :as prof]
            [game.logic.controller :as controller]
            [game.state.helper :as state-helper]))

(def profiling false)
(def logic-frame-time-ms 10)
(def display-frame-time-ms 10)
(def player-id 0)
(def field (atom ()))
(def inputs (atom ()))
(def mouse (atom ()))

(defn current-state 
  []
  (let [d-field @field d-input @inputs d-mouse @mouse]
    (state-helper/combined-state d-field d-input d-mouse)))

(defn current-state-display
  []
  (-> (current-state)
      (assoc :tracked player-id)))

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

(defn next-tick
  []
  (let [swing-inputs (deref display/inputs)
        swing-mouse (deref display/mouse-position-percent)]
    (swap! inputs assoc player-id swing-inputs)
    (swap! mouse assoc player-id swing-mouse)
    (->> (current-state)
         (controller/next-tick)
         (reset! field))))

(defn schedule-logic
  []
  (schedule-task next-tick logic-frame-time-ms))

(defn render 
  []
  (display/display (current-state-display)))

(defn schedule-render
  [] 
  (schedule-task render display-frame-time-ms))

(defn start-threads
  []
  (let [move-thread (future (schedule-logic))
        display-thread (future (schedule-render))]
    (deref move-thread)
    (deref display-thread)))

(defn fn-to-profile
  []
  (dotimes [_ 200]
    ((next-tick)
     (render))))

(defn init-components []
  (reset! field  (state-helper/get-new-field))
  (reset! inputs (state-helper/get-new-inputs))
  (reset! mouse  (state-helper/get-new-mouse))
  (display/init "Game"))

(defn -main
  "launch the game"
  []
  (init-components)
  (if profiling
    (prof/profile (fn-to-profile))
    (start-threads)))
