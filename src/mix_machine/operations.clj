(ns mix-machine.operations
  (:require [mix-machine.field-spec :as fs]
            [mix-machine.machine :as m]
            [mix-machine.data :as d]
            [mix-machine.console :as console]))

(def DEBUG true)

;; Actual operations
;;
;; Each (hopefully) can be of the form [M F machine]
;; Where M is the address computed by the SAA + content(I) method,
;; F is the field spec (both machine and (L:R) format)
;; and machine is the current state of the machine
;; Each should return the new state of the machine.


;; Load operations -------------------------------------------------------------------------------

(defn- ld*
  "Basic load operation.
  reg is either a keyword or collection.
  If reg is a keyword, it is either :A or :X.
  If reg is a collection, it is some collection [:I i],
  where i in range [1, 6].
  M is the indexed address.
  F is the F-modification."
  [reg M F machine]
  (let [field (fs/decode-field-spec F)
        content-M (m/get-memory machine M)
        new-word (fs/load-field-spec content-M field)]
    (when DEBUG
      (println "-- LD* --")
      (println "field" field)
      (println "content-M" content-M)
      (println "new-word" new-word)
      (println "---------"))
    (m/set-register machine reg new-word)))

(def lda (partial ld* :A))
(def ldx (partial ld* :X))
(def ld1 (partial ld* [:I 1]))
(def ld2 (partial ld* [:I 2]))
(def ld3 (partial ld* [:I 3]))
(def ld4 (partial ld* [:I 4]))
(def ld5 (partial ld* [:I 5]))
(def ld6 (partial ld* [:I 6]))

(defn- ldn*
  "Basic load negative operation.
  Same as ld*, but negates new-word before storing in a register."
  [reg M F machine]
  (let [field (fs/decode-field-spec F)
        content-M (m/get-memory machine M)
        new-word (d/negate-data (fs/load-field-spec content-M field))]
    (when DEBUG
      (println "-- LDN* --")
      (println "field" field)
      (println "content-M" content-M)
      (println "new-word" new-word)
      (println "---------"))
    (m/set-register machine reg new-word)))

(def ldan (partial ldn* :A))
(def ldxn (partial ldn* :X))
(def ld1n (partial ldn* [:I 1]))
(def ld2n (partial ldn* [:I 2]))
(def ld3n (partial ldn* [:I 3]))
(def ld4n (partial ldn* [:I 4]))
(def ld5n (partial ldn* [:I 5]))
(def ld6n (partial ldn* [:I 6]))


;; Store operations ------------------------------------------------------------------------------

(defn- st*
  [reg M F machine]
  (let [field (fs/decode-field-spec F)
        register (m/get-register machine reg (d/new-data 5))
        content-M (m/get-memory machine M)
        new-word (fs/store-field-spec register content-M field)]
    (when DEBUG
      (println "-- ST* --")
      (println "field" field)
      (println "register" register)
      (println "content-M" content-M)
      (println "new-word" new-word)
      (println "---------"))
    (m/set-memory machine M new-word)))

(def sta (partial st* :A))
(def stx (partial st* :X))
(def st1 (partial st* [:I 1]))
(def st2 (partial st* [:I 2]))
(def st3 (partial st* [:I 3]))
(def st4 (partial st* [:I 4]))
(def st5 (partial st* [:I 5]))
(def st6 (partial st* [:I 6]))
(def stj (partial st* :J))

;; Store zero works by passing an invalid register.
;; This requires st* (above) to call get-register with a default value of +0
;; (represented as a MIX word)
;; This is fine, since st* is private, and is only used to create the partial functions
;; that are actually used.
(def stz (partial st* :NOT_A_REGISTER))


;; Arithmetic operations ------------------------------------------------------------------------

(defn add
  [M F machine]
  (let [field (fs/decode-field-spec F)
        contents-M (m/get-memory machine M)
        contents-A (m/get-register machine :A)
        field-M (fs/load-field-spec contents-M field)
        result (+ (d/data->num contents-A) (d/data->num field-M))
        new-word (d/num->data result 5)
        ;; IF result = 0, then the sign stays the same...
        sign-adj (if (= 0 result)
                   (d/set-data-sign new-word (:sign contents-A))
                   new-word)]
    (when DEBUG
      (println "field" field)
      (println "contents-M" contents-M)
      (println "contents-A" contents-A)
      (println "field-M" field-M)
      (println "result" result)
      (println "new-word" new-word)
      (println "sign-adj" sign-adj)
      (println "overflow" (> (d/count-bytes sign-adj) 5))
      )
    (-> machine
        ;; Check overflow - if more than 5 bytes, overflow
        (m/set-overflow (> (d/count-bytes sign-adj) 5))
        (m/set-register :A sign-adj))))

(defn sub
  [M F machine]
  )


;; Operation maps --------------------------------------------------------------------------------

;; All operations, keyed 2 ways (once by key, once by code)
;; NOTE - fmod is the default F-modification, which is per-instruction.
;; There's a couple things to note here:
;; 1. 0 is a valid fmod, which means for instructions where there is no F written,
;;    the default must be used.
;; 2. This default must come from the reader, or whatever process converts
;;    the code to machine instructions.
;;    Example: LDA 2000,1 => +|31|16|1|F|8
;;    The above example does not specify an fmod, so for LDA, we use the default 5:
;;                           +|31|16|1|5|8.
(def operations-by-key
  {
   ;; Load
   :LDA {:key :LDA :code 8 :fmod 5 :fn lda}
   :LDX {:key :LDX :code 15 :fmod 5 :fn ldx}
   :LD1 {:key :LD1 :code 9 :fmod 5 :fn ld1}
   :LD2 {:key :LD2 :code 10 :fmod 5 :fn ld2}
   :LD3 {:key :LD3 :code 11 :fmod 5 :fn ld3}
   :LD4 {:key :LD4 :code 12 :fmod 5 :fn ld4}
   :LD5 {:key :LD5 :code 13 :fmod 5 :fn ld5}
   :LD6 {:key :LD6 :code 14 :fmod 5 :fn ld6}
   :LDAN {:key :LDAN :code 16 :fmod 5 :fn ldan}
   :LDXN {:key :LDXN :code 23 :fmod 5 :fn ldxn}
   :LD1N {:key :LD1N :code 17 :fmod 5 :fn ld1n}
   :LD2N {:key :LD2N :code 18 :fmod 5 :fn ld2n}
   :LD3N {:key :LD3N :code 19 :fmod 5 :fn ld3n}
   :LD4N {:key :LD4N :code 20 :fmod 5 :fn ld4n}
   :LD5N {:key :LD5N :code 21 :fmod 5 :fn ld5n}
   :LD6N {:key :LD6N :code 22 :fmod 5 :fn ld6n}
   ;; Store
   :STA {:key :STA :code 24 :fmod 5 :fn sta}
   :STX {:key :STX :code 31 :fmod 5 :fn stx}
   :ST1 {:key :ST1 :code 25 :fmod 5 :fn st1}
   :ST2 {:key :ST2 :code 26 :fmod 5 :fn st2}
   :ST3 {:key :ST3 :code 27 :fmod 5 :fn st3}
   :ST4 {:key :ST4 :code 28 :fmod 5 :fn st4}
   :ST5 {:key :ST5 :code 29 :fmod 5 :fn st5}
   :ST6 {:key :ST6 :code 30 :fmod 5 :fn st6}
   :STJ {:key :STJ :code 32 :fmod 2 :fn stj}
   :STZ {:key :STZ :code 33 :fmod 5 :fn stz}
   ;; Arithmetic
   :ADD {:key :ADD :code 1 :fmod 5 :fn add}
   })

(def operations-by-code
  (into {} (map (fn [[_ v]] {(:code v) v}) operations-by-key)))


;; Instructions ----------------------------------------------------------------------------------

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

(defn create-indexed-address
  "Generate the indexed address (M) for an instruction.
  The SAA part of the instruction is added to the contents of
  the index register, if idx is not 0.
  (NOTE: if idx is 0, a new-word is used in place of an index register,
   which has the effect of adding 0.  This was done to simplify the code)"
  [machine sign a1 a2 idx]
  (+ (d/data->num sign [0 0 0 a1 a2])
     (d/data->num (m/get-register machine [:I idx] (d/new-data 2)))))

(defn decode-instruction
  "Decode an instruction, returning:
  M - the indexed address created by SAA + CONTENTS(I)
  F - the F modification (typically a field specification)
  op - the operation."
  [machine inst]
  (let [sign (:sign inst)
        [a1 a2 idx fmod code] (:bytes inst)
        M (create-indexed-address machine sign a1 a2 idx)]
    (when DEBUG
      (println "-- decode-instruction --")
      (println "address parts" sign a1 a2)
      (println "index" idx)
      (println "F modification" fmod)
      (println "code" code)
      (println "M" M)
      (println "------------------------"))
    {:M M
     :F fmod
     :op (operations-by-code code)}))

(defn execute-instruction
  "Execute an instruction.
  Decodes and executes a machine instruction,
  returning the new state of the machine."
  [machine inst]
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

(defn execute-program
  [machine program]
  ;; (console/print-machine machine)
  (println "Executing program...")
  (loop [m machine inst (first program) rem (rest program)]
    (if-not inst
      (do
        (println "Executing program...DONE")
        m)
      (let [m2 (execute-instruction m inst)]
        ;; We can do stuff here, like print the machine,
        ;; wait for input, etc...
        (console/print-machine m2)
        (recur m2 (first rem) (rest rem))))))
