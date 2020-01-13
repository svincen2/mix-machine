(ns mix-machine.console
  ;; (:require [mix-machine.machine :as m])
  )

(defn word-sign->str
  [word]
  (get {:plus "+" :minus "-"} (:sign word)))

(defn word-bytes->str
  [word]
  (apply str (interpose "|" (:bytes word))))

(defn word->str
  [word]
  (str (word-sign->str word) "|" (word-bytes->str word)))

;; (defn instruction->str
;;   [inst]
;;   (let [s (:sign inst)
;;         [a1 a2 i f c] (:bytes inst)
;;         addr (m/word->num (m/new-data s [a1 a2]))]
;;     (apply str (interpose "|" [addr i f c]))))

(defn print-registers
  [registers]
  (println "\t- A: " (word->str (:A registers)))
  (println "\t- X: " (word->str (:X registers)))
  (doall (map #(println (str "\t- I" (inc %1) ":") (word->str %2))
              (range)
              (drop 1 (:I registers))))
  (println "\t- J: " (word->str (:J registers))))

(defn print-machine
  [mix-machine]
  (println "MIX Machine")
  (println "- registers:")
  (print-registers (:registers mix-machine))
  (println "- overflow:" (:overflow mix-machine))
  (println "- condition indicator:" (name (:condition-indicator mix-machine)))
  ;; Return the machine, so we can chain this in a thread macro
  mix-machine)

(defn print-memory
  ([mix-machine address]
   (println (format "M[%s]:" address) (word->str (get-in mix-machine [:memory address])))
   mix-machine)
  ([mix-machine addr1 addr2]
   (dorun (map (partial print-memory mix-machine) (range addr1 (inc addr2))))))

;; (defn print-program
;;   [program]
;;   (doall (map #(println (instruction->str %)) program)))
