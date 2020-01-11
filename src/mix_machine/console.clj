(ns mix-machine.console)

(defn word-sign->str
  [word]
  (get {:plus "+" :minus "-"} (:sign word)))

(defn word-bytes->str
  [word]
  (apply str (interpose "|" (:bytes word))))

(defn word->str
  [word]
  (str (word-sign->str word) "|" (word-bytes->str word)))

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
  [mix-machine address]
  (println (format "M[%s]:" address) (word->str (get-in mix-machine [:memory address])))
  mix-machine)
