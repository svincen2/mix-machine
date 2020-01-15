(ns mix-machine.core
  (:require [mix-machine.console :as console]
            [mix-machine.machine :as m]
            [mix-machine.data :as d]
            [mix-machine.operations :as ops]))

;; MIX (1009) is a hypothetical machine created by Knuth,
;; defined in The Art of Computer Programming, Vol 1, Sec. 1.3




(defn -main
  [& args]
  (let [mix-machine (atom m/new-machine)]
    (console/print-machine @mix-machine)
    ;; TESTING
    (-> @mix-machine
        (m/set-memory 2002 (d/new-data :minus [1 2 3 4 5]))
        (console/print-memory 2002)
        (m/set-memory 1934 (d/new-data :minus [6 7 8 9 10]))
        (console/print-memory 1934)
        (m/set-register [:I 1] (d/new-data :plus [0 2]))
        (m/set-register :J (d/new-data :plus [2 3]))
        console/print-machine
        (ops/execute-program [
                              (d/new-data :plus [31 16 1 5 15])
                              (d/new-data :plus [31 16 1 2 10])
                              (d/new-data :plus [31 16 2 29 16])
                              (d/new-data :plus [31 17 0 2 26])
                              (d/new-data :plus [31 19 0 2 32])
                              (d/new-data :plus [31 18 0 10 33])
                              ])
        (console/print-memory 2001)
        (console/print-memory 2003)
        (console/print-memory 2002)
        )
    ;; Don't return the entire machine (memory is too much to print...)
    nil

    ;; Read a program, and execute it
    ;; * Should execute step-by-step, printing the machine after each instruction
    ;; * Should wait for input before continuing to next instruction
    ;;   (just for testing purposes for now)
    ))
