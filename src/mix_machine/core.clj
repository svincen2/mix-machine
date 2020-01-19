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
    ;; (console/print-machine @mix-machine)
    ;; TEST 1 (load / store)
    ;; (-> @mix-machine
    ;;     (m/set-memory 2002 (d/new-data :minus [1 2 3 4 5]))
    ;;     (console/print-memory 2002)
    ;;     (m/set-memory 1934 (d/new-data :minus [6 7 8 9 10]))
    ;;     (console/print-memory 1934)
    ;;     (m/set-register [:I 1] (d/new-data :plus [0 2]))
    ;;     (m/set-register :J (d/new-data :plus [2 3]))
    ;;     console/print-machine
    ;;     (ops/execute-program [
    ;;                           (d/new-data :plus [31 16 1 5 15])
    ;;                           (d/new-data :plus [31 16 1 2 10])
    ;;                           (d/new-data :plus [31 16 2 29 16])
    ;;                           (d/new-data :plus [31 17 0 2 26])
    ;;                           (d/new-data :plus [31 19 0 2 32])
    ;;                           (d/new-data :plus [31 18 0 10 33])
    ;;                           ])
    ;;     (console/print-memory 2001)
    ;;     (console/print-memory 2003)
    ;;     (console/print-memory 2002)
    ;;     )
    ;; TEST 2 (enter, increase, store small registers)
    ;; (-> @mix-machine
    ;;     (m/set-memory 2000 (d/new-data :plus [1 2 3 4 5]))
    ;;     (ops/execute-program [
    ;;                           (d/new-data :plus [0 1 0 2 49]) ;; ENT1 1 // store 1 in rI1
    ;;                           (d/new-data :plus [0 9 0 0 49]) ;; INC1 9 // add 9 to rI1
    ;;                           (d/new-data :plus [31 16 0 20 25]) ;; ST1 2000(2:4) // store rI1 in 2000(0:5)
    ;;                           ])
    ;;     (console/print-machine)
    ;;     (console/print-memory 2000)
    ;;     )
    ;; TEST 3 (enter +/-0, multiple increase)
    ;; (-> @mix-machine
    ;;     (ops/execute-program [
    ;;                           (d/new-data :minus [0 1 0 2 55]) ;; ENTX -1
    ;;                           (d/new-data :minus [0 1 0 0 55]) ;; INCX -1
    ;;                           ])
    ;;     (console/print-machine))
    ;; TEST 4 comparison
    (-> @mix-machine
        (ops/load-program [
                           (d/new-data :minus [0 1 0 2 48]) ;; ENTA -1
                           (d/new-data :plus [1 36 0 5 24]) ;; STA 2000
                           ;; (d/new-data :plus [0 4 0 0 39]) ;; JMP 4
                           (d/new-data :plus [0 4 0 1 39]) ;; JSJ 4
                           (d/new-data :plus [1 36 0 5 56]) ;; CMPA 2000
                           (d/new-data :plus [1 36 0 5 63]) ;; CMPA 2000
                           (d/new-data :plus [0 0 0 2 5]) ;; HLT
                           ]
                          0)
        ops/execute-program
        (console/print-machine)
        )

    ;; Don't return the entire machine (memory is too much to print...)
    nil

    ;; Read a program, and execute it
    ;; * Should execute step-by-step, printing the machine after each instruction
    ;; * Should wait for input before continuing to next instruction
    ;;   (just for testing purposes for now)
    ))
