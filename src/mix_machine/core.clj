(ns mix-machine.core
  (:require [mix-machine.console :as console]
            [mix-machine.machine :as m]
            [mix-machine.data :as d]
            [mix-machine.operations :as ops]))

;; MIX (1009) is a hypothetical machine created by Knuth,
;; defined in The Art of Computer Programming, Vol 1, Sec. 1.3

(defn -main
  [& args]
  (let [mix-machine (atom m/new-machine)
        program [
                 (d/new-data :minus [3 10 0 2 48]) ;; ENTA -1
                 (d/new-data :plus [1 36 0 5 24]) ;; STA 100
                 (d/new-data :plus [1 36 0 2 48]) ;; ENTA 100
                 (d/new-data :plus [1 37 0 5 24]) ;; STA 101
                 (d/new-data :plus [3 8 0 2 49])  ;; ENT1 200
                 (d/new-data :plus [1 36 0 2 7])  ;; MOVE 100(2)
                 (d/new-data :plus [0 0 0 2 5]) ;; HLT
                 ]]
    (-> @mix-machine
        (ops/load-program program 0)
        ops/execute-program
        (console/print-memory [100 102] [200 202])
        (console/print-machine)
        )

    ;; Don't return the entire machine (memory is too much to print...)
    nil))
