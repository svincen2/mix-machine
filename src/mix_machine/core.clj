(ns mix-machine.core
  (:require [mix-machine.console :as console]
            [mix-machine.data :as d]
            [mix-machine.debug :as debug] ;; I wonder why debug doesn't want to work???
            [mix-machine.device :as dev]
            [mix-machine.machine :as m]
            [mix-machine.operations :as ops]))

;; NOTE This is just testing stuff...
;; TODO We should do some testing

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
                 (d/new-data :plus [1 36 0 1 37]) ;; OUT 100(1)
                 (d/new-data :plus [0 0 0 0 5]) ;; NUM 0
                 (d/new-data :plus [0 0 0 1 5]) ;; CHAR 0
                 (d/new-data :plus [4 44 0 5 24]) ;; STA 300
                 (d/new-data :plus [4 45 0 5 31]) ;; STX 301
                 (d/new-data :plus [4 44 0 20 37]) ;; OUT 300(20)
                 (d/new-data :plus [0 0 0 2 5]) ;; HLT
                 ]]
    (-> @mix-machine
        (m/add-device [:tape 1] (dev/new-tape 5))
        (m/add-device [:paper-tape] (dev/new-paper-tape 1))
        (ops/load-program program 0)
        (ops/execute-program)
        (console/print-memory [100 102] [200 202] [300 302])
        (console/print-machine)
        (console/print-device [:tape 1])
        (console/print-device [:paper-tape])
        )
    ;; Don't return the entire machine (memory is too much to print...)
    nil))
