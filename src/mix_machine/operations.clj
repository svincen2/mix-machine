(ns mix-machine.operations
  (:require [mix-machine.console :as console]
            [mix-machine.field-spec :as fs]))

;; Just to toggle some output on
(def DEBUG true)

(declare lda)

;; Operations
(def operations-by-key
  {:LDA {:key :LDA :code 8 :field-spec :field :fn lda}})

(def operations-by-code
  (into {} (map (fn [[_ v]] {(:code v) v}) operations-by-key)))

;; Instructions:
;; Each instruction is a word:
;; Sign | Address 1 | Address 2 | Index | Field Spec | Code
;; Sign, the sign part of a word
;; Address - first 2 bytes of the word
;; Index - 0-6
;; Field Spec - as defined above
;; Code - operation code
;; For each instruction, if I = 0, SAA is used as is as M
;; Otherwise I should be 1-6, and the contents of the index register are added to SAA,
;; which is the value of M.

(defn decode-instruction
  [machine inst]
  (let [sign (:sign inst)
        [a1 a2 idx fs code] (:bytes inst)
        address (word->num sign [0 0 0 a1 a2])
        idx-content (word->num (get-in machine [:registers :I idx]))
        M (+ address idx-content)]
    (when DEBUG
      (println "-- decode-instruction --")
      (println "address parts" sign a1 a2)
      (println "index" idx)
      (println "field spec" fs)
      (println "code" code)
      (println "address" address)
      (println "idx-content" idx-content)
      (println "M" M)
      (println "------------------------"))
    {:M M
     :F {:machine fs :field (fs/decode-field-spec fs)}
     :op (operations-by-code code)}))

(defn execute-instruction
  [machine inst]
  ;; (println "EI: ")
  ;; (console/print-memory machine 2000)
  (let [{:keys [M F op]} (decode-instruction machine inst)
        op-fn (:fn op)]
    (when DEBUG
      (println "-- execute-instruction --")
      (println "M" M)
      (println "F" F)
      (println "op" op)
      (println "op-fn" op-fn)
      (println "-------------------------"))
    (op-fn M F machine)))


;; Actual operations
;;
;; Each (hopefully) can be of the form [M F machine]
;; Where M is the address computed by the SAA + content(I) method,
;; F is the field spec (both machine and (L:R) format)
;; and machine is the current state of the machine
;; Each should return the new state of the machine.


(defn lda
  "Load Accumulator.
  Loads the field of CONTENTS(M) in the Accumulator.
  Field is right shifted.  If the sign is omitted, + is assumed."
  [M F machine]
  ;; (print "lda: ")
  ;; (console/print-memory machine 2000)
  (let [content-M (get-in machine [:memory M])
        new-word (fs/apply-field-spec content-M (:field F))]
    (when DEBUG
      (println "-- LDA --")
      (println "content-M" content-M)
      (println "new-word" new-word)
      (println "---------"))
    (assoc-in machine [:registers :A] new-word)))
