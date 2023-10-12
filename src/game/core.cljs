(ns game.core
  (:require [game.logic.controller :as controller]
            [game.state.helper :as state-helper]))

(def logic-frame-time-ms 100)
(enable-console-print!)

(def field (atom ()))
(def inputs (atom ()))
(def mouse (atom ()))

(defn init-components []
  (reset! field  (state-helper/get-new-field))
  (reset! inputs (state-helper/get-new-inputs))
  (reset! mouse  (state-helper/get-new-mouse)))

(defn current-state
  []
  (let [d-field @field d-input @inputs d-mouse @mouse]
    (state-helper/combined-state d-field d-input d-mouse)))

(defn printresult []
  (->> (current-state)
       (controller/next-tick)
       (reset! field))
  (println (current-state)))

((init-components)
 (js/setInterval printresult logic-frame-time-ms))