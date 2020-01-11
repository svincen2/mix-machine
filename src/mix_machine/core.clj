(ns mix-machine.core
  (:require [mix-machine.console :as console]
            [mix-machine.machine :as m]
            [mix-machine.operations :as ops]))

;; MIX (1009) is a hypothetical machine created by Knuth,
;; defined in The Art of Computer Programming, Vol 1, Sec. 1.3




(defn -main
  [& args]
  (let [mix-machine (atom m/new-mix-machine)]
    (console/print-machine @mix-machine)
    ;; TESTING
    (-> @mix-machine
        (assoc-in [:memory 2002] (m/new-word :minus [1 2 3 4 5]))
        (console/print-memory 2002)
        (assoc-in [:registers :I 1] (m/new-word :plus [0 0 0 0 2]))
        console/print-machine
        (ops/execute-instruction (m/new-word :plus [31 16 1 11 8]))
        console/print-machine
        )
    ;; Don't return the entire machine (memory is too much to print...)
    nil

    ;; Read a program, and execute it
    ;; * Should execute step-by-step, printing the machine after each instruction
    ;; * Should wait for input before continuing to next instruction
    ;;   (just for testing purposes for now)
    ))
