(ns mix-machine.console
  (:require [mix-machine.machine :as m]
            [mix-machine.debug :refer [debug]]))

(defn sign->str
  [data]
  (get {:plus "+" :minus "-"} (:sign data)))

(defn bytes->str
  [data]
  (->> (:bytes data)
       (map #(format "%2d" %))
       (interpose "|")
       (apply str)))

(defn data->str
  [data]
  (str (sign->str data) "|" (bytes->str data)))

(defn print-registers
  [registers]
  (println (format "-- %-7s %20s" "rA" (data->str (:A registers))))
  (println (format "-- %-7s %20s" "rX" (data->str (:X registers))))
  (dorun
   (map #(println (format "-- %-7s %20s" (str "rI" (inc %1)) (data->str %2)))
        (range)
        (drop 1 (:I registers))))
  (println (format "-- %-7s %20s" "rJ" (data->str (:J registers)))))

(defn print-machine
  [machine]
  (println "\nMachine")
  (print-registers (:registers machine))
  (println (format "-- %-9s %18b" "Overflow" (:overflow machine)))
  (println (format "-- %-9s %18s" "Condition" (name (:condition-indicator machine))))
  ;; Return the machine, so we can chain this in a thread macro
  machine)

(defn print-memory
  [machine & ranges]
  (println "\nMemory")
  (let [fmt "%04d: %25s"
        addresses (mapcat (fn [[a b]] (range a b)) ranges)]
    (dorun
     (map #(println (format fmt % (data->str (m/get-memory machine %))))
          addresses))
    machine))

(defn print-data-numbered
  [data]
  (dorun
   (map-indexed (fn [i d] (println (format "%04d: %25s" i (data->str d)))) data)))

;; TODO - Pretty this up
(defn print-device
  [machine d]
  (let [device (m/get-device machine d)]
    (debug device) ;; TODO - debug is not appropriate here... this is dev oriented output
    machine))
