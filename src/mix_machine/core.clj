(ns mix-machine.core
  (:require [mix-machine.console :as console]
            [mix-machine.machine :as m]
            [mix-machine.operations :as ops]))

;; MIX (1009) is a hypothetical machine created by Knuth,
;; defined in The Art of Computer Programming, Vol 1, Sec. 1.3




(defn -main
  [& args]
  (let [mix-machine (atom m/new-machine)]
    (console/print-machine @mix-machine)
    ;; TESTING
    (-> @mix-machine
        (m/set-memory 2002 (m/new-data :minus [1 2 3 4 5]))
        (console/print-memory 2002)
        (m/set-memory 1934 (m/new-data :minus [6 7 8 9 10]))
        (console/print-memory 1934)
        (m/set-register [:I 1] (m/new-data :plus [0 2]))
        console/print-machine
        (ops/execute-program [
                              (m/new-data :plus [31 16 1 5 15])
                              (m/new-data :plus [31 16 1 2 10])
                              (m/new-data :plus [31 16 2 5 16])
                              ])
        )
    ;; Don't return the entire machine (memory is too much to print...)
    nil

    ;; Read a program, and execute it
    ;; * Should execute step-by-step, printing the machine after each instruction
    ;; * Should wait for input before continuing to next instruction
    ;;   (just for testing purposes for now)
    ))
