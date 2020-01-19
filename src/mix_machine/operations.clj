(ns mix-machine.operations
  (:require [mix-machine.field-spec :as fs]
            [mix-machine.machine :as m]
            [mix-machine.data :as d]
            [mix-machine.console :as console]
            [clojure.math.numeric-tower :as math]))

(def DEBUG false)
(def PRINT true)

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
        content-M (m/get-memory machine (d/data->num M))
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
        content-M (m/get-memory machine (d/data->num M))
        new-word (d/negate (fs/load-field-spec content-M field))]
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
  "Basic STORE operation.
  The number of bytes from the right-most bytes of the register are shifted
  into the field of M.
  This is the exact opposite behavior of LOAD.
  NOTE: The m/get-register call has a default for STZ (store zero) to work."
  [reg M F machine]
  (let [field (fs/decode-field-spec F)
        register (-> (m/get-register machine reg (d/new-data 5))
                     ;; Extend smaller registers
                     (d/extend 5))
        content-M (m/get-memory machine (d/data->num M))
        new-word (fs/store-field-spec register content-M field)]
    (when DEBUG
      (println "-- ST* --")
      (println "field" field)
      (println "register" register)
      (println "M" (d/data->num M))
      (println "content-M" content-M)
      (println "new-word" new-word)
      (println "---------"))
    (m/set-memory machine (d/data->num M) new-word)))

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
;; where the register is already specified.
(def stz (partial st* :NOT_A_REGISTER))


;; Arithmetic operations ------------------------------------------------------------------------

(defn- add*
  "Performs MIX machine addition.
  machine is a MIX machine.
  op1 is the first operand.
  op2 is the second operand.
  reg is the register in which the result is stored.
  If the result is 0, the result's sign is the same as op1's sign.
  If the result is larger than a word, the overflow toggle is set,
  and the right-most 5 bytes of the result are stored in reg."
  [machine op1 op2 reg]
  (let [result (+ (d/data->num op1) (d/data->num op2))
        new-word (d/num->data result)
        ;; IF result = 0, then the sign stays the same.
        sign-adj (if (= 0 result)
                   (d/set-sign new-word (:sign op1))
                   new-word)
        ;; If result is too big, overflow is set, otherwise it stays the same.
        overflow (or (> (d/count-bytes sign-adj) 5)
                     (m/get-overflow machine))]
    (when DEBUG
      (println "op1" op1)
      (println "op2" op2)
      (println "reg" reg)
      (println "result" result)
      (println "new-word" new-word)
      (println "sign-adj" sign-adj)
      (println "overflow" overflow)
      )
    (-> machine
        (m/set-overflow overflow)
        (m/set-register reg sign-adj))))

(defn add
  "MIX Addition.
  The field of CONTENTS(M) is added to the Accumulator.
  If the result is 0, the Accumulator's sign is unchanged.
  If the result is too large to fit in the Accumulator,
  the overflow bit is set to true, and the Accumulator contains
  the right-most 5 bytes of the result represented in 6 bytes
  (i.e. [1 b1 b2 b3 b4 b5])."
  [M F machine]
  (let [field (fs/decode-field-spec F)
        contents-M (m/get-memory machine (d/data->num M))
        contents-A (m/get-register machine :A)
        V (fs/load-field-spec contents-M field)]
    (when DEBUG
      (println "field" field)
      (println "contents-M" contents-M)
      (println "contents-A" contents-A)
      (println "V" V)
      )
    (add* machine contents-A V :A)))

(defn sub
  "MIX Subtraction.
  The field of CONTENTS(M) is subtracted from the Accumulator.
  Works the same as ADD, except CONTENTS(M) is negated."
  [M F machine]
  (let [field (fs/decode-field-spec F)
        contents-M (m/get-memory machine (d/data->num M))
        contents-A (m/get-register machine :A)
        V (fs/load-field-spec contents-M field)]
    (when DEBUG
      (println "field" field)
      (println "contents-M" contents-M)
      (println "contents-A" contents-A)
      (println "V" V)
      )
    (add* machine contents-A (d/negate V) :A)))

(defn mul
  "The 10-byte product of V * rA is stored in rA and rX.
  Both rA and rX sign's are set to the resulting sign.
  V is the field of CONTENTS(M)"
  [M F machine]
  (let [field (fs/decode-field-spec F)
        contents-M (m/get-memory machine (d/data->num M))
        contents-A (m/get-register machine :A)
        V (fs/load-field-spec contents-M field)
        result (* (d/data->num contents-A) (d/data->num V))
        new-word (d/num->data result 10)
        [new-A new-X] (d/split new-word 5)]
    (when DEBUG
      (println "field" field)
      (println "contents-M" contents-M)
      (println "contents-A" contents-A)
      (println "V" V)
      (println "result" result)
      (println "new-word" new-word)
      (println "new-A" new-A)
      (println "new-X" new-X)
      )
    (-> machine
        (m/set-register :A new-A)
        (m/set-register :X new-X))))

(defn div
  "The 10-byte number created by rA and rX is divided by V.
  After division:
  rA contains the quotient.
  rX contains the remainder.
  The sign of rA is the sign of the result.
  The sign of rX is the previous sign of rA."
  [M F machine]
  (let [field (fs/decode-field-spec F)
        contents-M (m/get-memory machine (d/data->num M))
        contents-A (m/get-register machine :A)
        contents-X (m/get-register machine :X)
        V (d/data->num (fs/load-field-spec contents-M field))
        rAX (d/data->num (d/merge contents-A contents-X))]
    (when DEBUG
      (println "field" field)
      (println "contents-M" contents-M)
      (println "contents-A" contents-A)
      (println "contents-X" contents-X)
      (println "V" V)
      (println "rAX" rAX)
      )
    (if (= 0 V)
      ;; MIX says undefined information is filled in rA and rX,
      ;; and the overflow toggle is set
      ;; So we'll just leave whatever data is in there...
      (m/set-overflow machine true)
      (let [result-A (d/num->data (quot rAX V))
            result-X (-> (rem rAX V)
                         d/num->data
                         (d/set-sign (:sign contents-A)))
            ;; Need to also check for overflow here...
            overflow (or (> (d/count-bytes result-A) 5)
                         (m/get-overflow machine))]
        (when DEBUG
          (println "result-A" result-A)
          (println "result-X" result-X)
          (println "overflow" overflow)
          )
        (-> machine
            (m/set-overflow overflow)
            (m/set-register :A result-A)
            (m/set-register :X result-X))))))


;; Address transfer operations -------------------------------------------------------------------

;; TODO - TEST THESE

(defn ent*
  [reg M F machine]
  (m/set-register machine reg M))

(def enta (partial ent* :A))
(def entx (partial ent* :X))
(def ent1 (partial ent* [:I 1]))
(def ent2 (partial ent* [:I 2]))
(def ent3 (partial ent* [:I 3]))
(def ent4 (partial ent* [:I 4]))
(def ent5 (partial ent* [:I 5]))
(def ent6 (partial ent* [:I 6]))

(defn enn*
  [reg M F machine]
  (m/set-register machine reg (d/negate M)))

(def enna (partial enn* :A))
(def ennx (partial enn* :X))
(def enn1 (partial enn* [:I 1]))
(def enn2 (partial enn* [:I 2]))
(def enn3 (partial enn* [:I 3]))
(def enn4 (partial enn* [:I 4]))
(def enn5 (partial enn* [:I 5]))
(def enn6 (partial enn* [:I 6]))

(defn- inc*
  [reg M F machine]
  (let [contents-R (m/get-register machine reg)]
    (when DEBUG
      (println "contents-R" contents-R)
      )
    (add* machine contents-R M reg)))

(def inca (partial inc* :A))
(def incx (partial inc* :X))
(def inc1 (partial inc* [:I 1]))
(def inc2 (partial inc* [:I 2]))
(def inc3 (partial inc* [:I 3]))
(def inc4 (partial inc* [:I 4]))
(def inc5 (partial inc* [:I 5]))
(def inc6 (partial inc* [:I 6]))

(defn- dec*
  [reg M F machine]
  (let [contents-R (m/get-register machine reg)]
    (when DEBUG
      (println "contents-R" contents-R)
      )
    (add* machine contents-R (d/negate M) reg)))

(def deca (partial dec* :A))
(def decx (partial dec* :X))
(def dec1 (partial dec* [:I 1]))
(def dec2 (partial dec* [:I 2]))
(def dec3 (partial dec* [:I 3]))
(def dec4 (partial dec* [:I 4]))
(def dec5 (partial dec* [:I 5]))
(def dec6 (partial dec* [:I 6]))


;; Comparison operations -------------------------------------------------------------------------

(defn- cmp*
  [reg M F machine]
  (let [field (fs/decode-field-spec F)
        contents-R (-> (m/get-register machine reg)
                       ;; Need to shift the smaller registers before loading the field
                       (d/set-size 5)
                       (fs/load-field-spec field))
        contents-M (-> (m/get-memory machine (d/data->num M))
                       (fs/load-field-spec field))
        result (compare (d/data->num contents-R) (d/data->num contents-M))]
    (m/set-condition-indicator machine ({-1 :less 0 :equal 1 :greater} result))))

(def cmpa (partial cmp* :A))
(def cmpx (partial cmp* :X))
(def cmp1 (partial cmp* [:I 1]))
(def cmp2 (partial cmp* [:I 2]))
(def cmp3 (partial cmp* [:I 3]))
(def cmp4 (partial cmp* [:I 4]))
(def cmp5 (partial cmp* [:I 5]))
(def cmp6 (partial cmp* [:I 6]))


;; Jump operations -------------------------------------------------------------------------------

;; NOTE - The following operations do a few strange things worth noting:
;; 1. When setting the program counter from M, M is always decremented by 1.
;;    This is because the program counter is always increased by 1 at the end
;;    of each instruction.  So jump instruction can't load the actual address of the next
;;    instruction; they need to load address - 1
;; 2. When storing the program counter in the J register, it is incremented first.
;;    This makes sense, since jump operations (except JSJ) store the next instruction
;;    in J as a return address

(defn jmp
  [M F machine]
  (let [pc (m/get-program-counter machine)]
    (-> machine
        (m/set-program-counter (dec (d/data->num M)))
        (m/set-register :J (d/num->data (inc pc))))))

(defn jsj
  [M F machine]
  (m/set-program-counter machine (dec (d/data->num M))))

;; TODO - TEST THESE

(defn- jmpp
  [p M F machine]
  (if p
    (jmp M F machine)
    machine))

(defn- jov*
  [of M F machine]
  (let [overflow (m/get-overflow machine)
        m2 (m/set-overflow machine false)]
    (jmpp (= of overflow) M F m2)))

(def jov (partial jov* true))
(def jnov (partial jov* false))

(defn- jci*
  "Jump if the condition indicator conforms to ci-desc.
  ci-desc is a vec with a predicate, followed by a condition.
  The predicate can be = or not.
  Jumps if (predicate condition condition-indicator) returns true.
  So, for example, a ci-desc of [not :greater] will check
  if the condition indicator is not :greater, (i.e. :less or :equal)."
  [ci-desc M F machine]
  (let [ci (m/get-condition-indicator machine)
        [p c] ci-desc]
    (jmpp (p c ci) M F machine)))

(def jl (partial jci* [= :less]))
(def je (partial jci* [= :equal]))
(def jg (partial jci* [= :greater]))
(def jge (partial jci* [not :less]))
(def jne (partial jci* [not :equal]))
(def jle (partial jci* [not :greater]))

(defn- ja*
  [p M F machine]
  (let [contents-A (d/data->num (m/get-register machine :A))]
    (jmpp (p contents-A 0) M F machine)))

(def jan (partial ja* <))
(def jaz (partial ja* =))
(def jap (partial ja* >))
(def jann (partial ja* >=))
(def janz (partial ja* not=))
(def janp (partial ja* <=))

(defn- jx*
  [p M F machine]
  (let [contents-X (d/data->num (m/get-register machine :X))]
    (jmpp (p contents-X 0) M F machine)))

(def jxn (partial jx* <))
(def jxz (partial jx* =))
(def jxp (partial jx* >))
(def jxnn (partial jx* >=))
(def jxnz (partial jx* not=))
(def jxnp (partial jx* <=))

(defn- ji*
  [i p M F machine]
  (let [contents-I (d/data->num (m/get-register machine [:I i]))]
    (jmpp (p contents-I 0) M F machine)))

(def j1n (partial ji* 1 <))
(def j1z (partial ji* 1 =))
(def j1p (partial ji* 1 >))
(def j1nn (partial ji* 1 >=))
(def j1nz (partial ji* 1 not=))
(def j1np (partial ji* 1 <=))

(def j2n (partial ji* 2 <))
(def j2z (partial ji* 2 =))
(def j2p (partial ji* 2 >))
(def j2nn (partial ji* 2 >=))
(def j2nz (partial ji* 2 not=))
(def j2np (partial ji* 2 <=))

(def j3n (partial ji* 3 <))
(def j3z (partial ji* 3 =))
(def j3p (partial ji* 3 >))
(def j3nn (partial ji* 3 >=))
(def j3nz (partial ji* 3 not=))
(def j3np (partial ji* 3 <=))

(def j4n (partial ji* 4 <))
(def j4z (partial ji* 4 =))
(def j4p (partial ji* 4 >))
(def j4nn (partial ji* 4 >=))
(def j4nz (partial ji* 4 not=))
(def j4np (partial ji* 4 <=))

(def j5n (partial ji* 5 <))
(def j5z (partial ji* 5 =))
(def j5p (partial ji* 5 >))
(def j5nn (partial ji* 5 >=))
(def j5nz (partial ji* 5 not=))
(def j5np (partial ji* 5 <=))

(def j6n (partial ji* 6 <))
(def j6z (partial ji* 6 =))
(def j6p (partial ji* 6 >))
(def j6nn (partial ji* 6 >=))
(def j6nz (partial ji* 6 not=))
(def j6np (partial ji* 6 <=))


;; Shift operations ------------------------------------------------------------------------------

;; TODO - TEST THESE

(defn- sa*
  [shift M F machine]
  (let [contents-A (m/get-register machine :A)]
    ;; TODO - M must be nonnegative, but MIX doesn't define what happens if it isn't...
    (m/set-register machine :A (shift contents-A (d/data->num M)))))

(def sla (partial sa* d/shift-left))
(def sra (partial sa* d/shift-right))

(defn- sax*
  [shift M F machine]
  (let [contents-A (m/get-register machine :A)
        contents-X (m/get-register machine :X)
        sign-X (:sign contents-X)
        contents-AX (d/merge contents-A contents-X)
        shifted (shift contents-AX (d/data->num M))
        [rA rX] (d/split shifted 5)]
    (-> machine
        (m/set-register :A rA)
        (m/set-register :X (d/set-sign rX sign-X)))))

(def slax (partial sax* d/shift-left))
(def srax (partial sax* d/shift-right))
(def slc (partial sax* d/cycle-left))
(def src (partial sax* d/cycle-right))


;; Move ------------------------------------------------------------------------------------------

(defn move
  [M F machine]
  (if (= 0 F)
    machine
    (let [source (d/data->num M)
          dest (d/data->num (m/get-register machine [:I 1]))]
      (when DEBUG
        (println "-- MOVE --")
        (println "source" source)
        (println "dest" dest)
        (println "----------")
        )
      (as-> machine m
        (reduce (fn [m i]
                  (let [data (m/get-memory m (+ source i))]
                    (m/set-memory m (+ dest i) data)))
                m
                (range F))
        (m/inc-register m [:I 1] (d/num->data F))))))


;; Operation maps --------------------------------------------------------------------------------

;; All operations
;; NOTE - fmod is the default F-modification, which is per-instruction.
;; NOTE - fmod is occasionally used to distinguish between two operations with the same op code.
;; There's a couple things to note here:
;; 1. 0 is a valid fmod, which means for instructions where there is no F written,
;;    the default must be used.
;; 2. This default must come from the reader, or whatever process converts
;;    the code to machine instructions.
;;    Example: LDA 2000,1 => +|31|16|1|F|8
;;    The above example does not specify an fmod, so for LDA, we use the default 5:
;;                           +|31|16|1|5|8.
(def ^:private operations
  [
   ;; Load
   {:key :LDA :code 8 :fmod 5 :fn lda}
   {:key :LDX :code 15 :fmod 5 :fn ldx}
   {:key :LD1 :code 9 :fmod 5 :fn ld1}
   {:key :LD2 :code 10 :fmod 5 :fn ld2}
   {:key :LD3 :code 11 :fmod 5 :fn ld3}
   {:key :LD4 :code 12 :fmod 5 :fn ld4}
   {:key :LD5 :code 13 :fmod 5 :fn ld5}
   {:key :LD6 :code 14 :fmod 5 :fn ld6}
   {:key :LDAN :code 16 :fmod 5 :fn ldan}
   {:key :LDXN :code 23 :fmod 5 :fn ldxn}
   {:key :LD1N :code 17 :fmod 5 :fn ld1n}
   {:key :LD2N :code 18 :fmod 5 :fn ld2n}
   {:key :LD3N :code 19 :fmod 5 :fn ld3n}
   {:key :LD4N :code 20 :fmod 5 :fn ld4n}
   {:key :LD5N :code 21 :fmod 5 :fn ld5n}
   {:key :LD6N :code 22 :fmod 5 :fn ld6n}
   ;; Store
   {:key :STA :code 24 :fmod 5 :fn sta}
   {:key :STX :code 31 :fmod 5 :fn stx}
   {:key :ST1 :code 25 :fmod 5 :fn st1}
   {:key :ST2 :code 26 :fmod 5 :fn st2}
   {:key :ST3 :code 27 :fmod 5 :fn st3}
   {:key :ST4 :code 28 :fmod 5 :fn st4}
   {:key :ST5 :code 29 :fmod 5 :fn st5}
   {:key :ST6 :code 30 :fmod 5 :fn st6}
   {:key :STJ :code 32 :fmod 2 :fn stj}
   {:key :STZ :code 33 :fmod 5 :fn stz}
   ;; Arithmetic
   {:key :ADD :code 1 :fmod 5 :fn add}
   {:key :SUB :code 2 :fmod 5 :fn sub}
   {:key :MUL :code 3 :fmod 5 :fn mul}
   {:key :DIV :code 4 :fmod 5 :fn div}
   ;; Address transfer
   ;; NOTE - These require lookup by code and fmod
   {:key :ENTA :code 48 :fmod 2 :fn enta}
   {:key :ENTX :code 55 :fmod 2 :fn entx}
   {:key :ENT1 :code 49 :fmod 2 :fn ent1}
   {:key :ENT2 :code 50 :fmod 2 :fn ent2}
   {:key :ENT3 :code 51 :fmod 2 :fn ent3}
   {:key :ENT4 :code 52 :fmod 2 :fn ent4}
   {:key :ENT5 :code 53 :fmod 2 :fn ent5}
   {:key :ENT6 :code 54 :fmod 2 :fn ent6}
   {:key :ENNA :code 48 :fmod 3 :fn enna}
   {:key :ENNX :code 55 :fmod 3 :fn ennx}
   {:key :ENN1 :code 49 :fmod 3 :fn enn1}
   {:key :ENN2 :code 50 :fmod 3 :fn enn2}
   {:key :ENN3 :code 51 :fmod 3 :fn enn3}
   {:key :ENN4 :code 52 :fmod 3 :fn enn4}
   {:key :ENN5 :code 53 :fmod 3 :fn enn5}
   {:key :ENN6 :code 54 :fmod 3 :fn enn6}
   {:key :INCA :code 48 :fmod 0 :fn inca}
   {:key :INCX :code 55 :fmod 0 :fn incx}
   {:key :INC1 :code 49 :fmod 0 :fn inc1}
   {:key :INC2 :code 50 :fmod 0 :fn inc2}
   {:key :INC3 :code 51 :fmod 0 :fn inc3}
   {:key :INC4 :code 52 :fmod 0 :fn inc4}
   {:key :INC5 :code 53 :fmod 0 :fn inc5}
   {:key :INC6 :code 54 :fmod 0 :fn inc6}
   {:key :DECA :code 48 :fmod 1 :fn deca}
   {:key :DECX :code 55 :fmod 1 :fn decx}
   {:key :DEC1 :code 49 :fmod 1 :fn dec1}
   {:key :DEC2 :code 50 :fmod 1 :fn dec2}
   {:key :DEC3 :code 51 :fmod 1 :fn dec3}
   {:key :DEC4 :code 52 :fmod 1 :fn dec4}
   {:key :DEC5 :code 53 :fmod 1 :fn dec5}
   {:key :DEC6 :code 54 :fmod 1 :fn dec6}
   ;; Comparison
   {:key :CMPA :code 56 :fmod 5 :fn cmpa}
   {:key :CMPX :code 63 :fmod 5 :fn cmpx}
   {:key :CMP1 :code 57 :fmod 5 :fn cmp1}
   {:key :CMP2 :code 58 :fmod 5 :fn cmp2}
   {:key :CMP3 :code 59 :fmod 5 :fn cmp3}
   {:key :CMP4 :code 60 :fmod 5 :fn cmp4}
   {:key :CMP5 :code 61 :fmod 5 :fn cmp5}
   {:key :CMP6 :code 62 :fmod 5 :fn cmp6}
   ;; Jump
   {:key :JMP :code 39 :fmod 0 :fn jmp}
   {:key :JSJ :code 39 :fmod 1 :fn jsj}
   ;; Jump overflow
   {:key :JOV :code 39 :fmod 2 :fn jov}
   {:key :JNOV :code 39 :fmod 3 :fn jnov}
   ;; Jump condition indicator
   {:key :JL :code 39 :fmod 4 :fn jl}
   {:key :JE :code 39 :fmod 5 :fn je}
   {:key :JG :code 39 :fmod 6 :fn jg}
   {:key :JGE :code 39 :fmod 7 :fn jge}
   {:key :JNE :code 39 :fmod 8 :fn jne}
   {:key :JLE :code 39 :fmod 9 :fn jle}
   ;; Jump Accumulator
   {:key :JAN :code 40 :fmod 0 :fn jan}
   {:key :JAZ :code 40 :fmod 1 :fn jaz}
   {:key :JAP :code 40 :fmod 2 :fn jap}
   {:key :JANN :code 40 :fmod 3 :fn jann}
   {:key :JANZ :code 40 :fmod 4 :fn janz}
   {:key :JANP :code 40 :fmod 5 :fn janp}
   ;; Jump Extension
   {:key :JXN :code 47 :fmod 0 :fn jxn}
   {:key :JXZ :code 47 :fmod 1 :fn jxz}
   {:key :JXP :code 47 :fmod 2 :fn jxp}
   {:key :JXNN :code 47 :fmod 3 :fn jxnn}
   {:key :JXNZ :code 47 :fmod 4 :fn jxnz}
   {:key :JXNP :code 47 :fmod 5 :fn jxnp}
   ;; Jump I1
   {:key :J1N  :code 41 :fmod 0 :fn j1n}
   {:key :J1Z  :code 41 :fmod 1 :fn j1z}
   {:key :J1P  :code 41 :fmod 2 :fn j1p}
   {:key :J1NN :code 41 :fmod 3 :fn j1nn}
   {:key :J1NZ :code 41 :fmod 4 :fn j1nz}
   {:key :J1NP :code 41 :fmod 5 :fn j1np}
   ;; Jump I2
   {:key :J2N  :code 42 :fmod 0 :fn j2n}
   {:key :J2Z  :code 42 :fmod 1 :fn j2z}
   {:key :J2P  :code 42 :fmod 2 :fn j2p}
   {:key :J2NN :code 42 :fmod 3 :fn j2nn}
   {:key :J2NZ :code 42 :fmod 4 :fn j2nz}
   {:key :J2NP :code 42 :fmod 5 :fn j2np}
   ;; Jump I3
   {:key :J3N  :code 43 :fmod 0 :fn j3n}
   {:key :J3Z  :code 43 :fmod 1 :fn j3z}
   {:key :J3P  :code 43 :fmod 2 :fn j3p}
   {:key :J3NN :code 43 :fmod 3 :fn j3nn}
   {:key :J3NZ :code 43 :fmod 4 :fn j3nz}
   {:key :J3NP :code 43 :fmod 5 :fn j3np}
   ;; Jump I4
   {:key :J4N  :code 44 :fmod 0 :fn j4n}
   {:key :J4Z  :code 44 :fmod 1 :fn j4z}
   {:key :J4P  :code 44 :fmod 2 :fn j4p}
   {:key :J4NN :code 44 :fmod 3 :fn j4nn}
   {:key :J4NZ :code 44 :fmod 4 :fn j4nz}
   {:key :J4NP :code 44 :fmod 5 :fn j4np}
   ;; Jump I5
   {:key :J5N  :code 45 :fmod 0 :fn j5n}
   {:key :J5Z  :code 45 :fmod 1 :fn j5z}
   {:key :J5P  :code 45 :fmod 2 :fn j5p}
   {:key :J5NN :code 45 :fmod 3 :fn j5nn}
   {:key :J5NZ :code 45 :fmod 4 :fn j5nz}
   {:key :J5NP :code 45 :fmod 5 :fn j5np}
   ;; Jump I6
   {:key :J6N  :code 46 :fmod 0 :fn j6n}
   {:key :J6Z  :code 46 :fmod 1 :fn j6z}
   {:key :J6P  :code 46 :fmod 2 :fn j6p}
   {:key :J6NN :code 46 :fmod 3 :fn j6nn}
   {:key :J6NZ :code 46 :fmod 4 :fn j6nz}
   {:key :J6NP :code 46 :fmod 5 :fn j6np}
   ;; Shift
   {:key :SLA :code 6 :fmod 0 :fn sla}
   {:key :SRA :code 6 :fmod 1 :fn sra}
   {:key :SLAX :code 6 :fmod 2 :fn slax}
   {:key :SRAX :code 6 :fmod 3 :fn srax}
   {:key :SLC :code 6 :fmod 4 :fn slc}
   {:key :SRC :code 6 :fmod 5 :fn src}
   ;; Move
   {:key :MOVE :code 7 :fmod 1 :fn move}
   ;; No-op
   {:key :NOP :code 0 :fmod 0 :fn nil}
   ;; Halt
   {:key :HLT :code 5 :fmod 2 :fn nil}
   ])

(def ^:private operations-by-code (group-by :code operations))

(defn lookup-op
  "Look up the operation that corresponds to code and fmod.
  For most operations, the code is enough for lookup.
  However some operations share the same code (ENTA and INCA for example),
  and use the F-modification to distinguish between them."
  [code fmod]
  (let [possible-ops (get operations-by-code code)]
    (if (= 1 (count possible-ops))
      ;; Only one option, return it
      (first possible-ops)
      ;; Re-map possibe-ops by fmod to find the op we want
      (get (zipmap (map :fmod possible-ops) possible-ops) fmod))))


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
  (d/add (d/new-data sign [0 0 0 a1 a2])
         (m/get-register machine [:I idx] (d/new-data 2))))

(defn decode-instruction
  "Decode an instruction, returning:
  M - the indexed address created by SAA + CONTENTS(I)
  F - the F modification (typically a field specification)
  op - the operation."
  [machine inst]
  (let [sign (:sign inst)
        [a1 a2 idx fmod code] (:bytes inst)
        M (create-indexed-address machine sign a1 a2 idx)]
    (when PRINT
      (println "\nDecode Instruction")
      (println (format "-- %-7s %20d" "Address" (d/data->num sign [a1 a2])))
      (println (format "-- %-7s %20d" "Index" idx))
      (println (format "-- %-15s %12s" "F modification"
                       (str fmod " " (fs/field-spec-str fmod))))
      (println (format "-- %-7s %20d" "Code" code))
      )
    {:M M
     :F fmod
     :op (lookup-op code fmod)}))

(defn execute-next-instruction
  [machine]
  (let [pc (m/get-program-counter machine)
        inst (m/get-memory machine pc)
        {:keys [M F op]} (decode-instruction machine inst)
        {op-fn :fn key :key} op]
    (when PRINT
      (println "\nExecute Operation")
      (println (format "-- %-7s %20s" "Name" (name key)))
      (println (format "-- %-7s %20d" "M" (d/data->num M)))
      (println (format "-- %-7s %20s" "F" F (fs/field-spec-str F)))
      )
    (-> (case key
          :HLT [machine true]
          :NOP [machine false]
          [(op-fn M F machine) false])
        ;; Increment program counter
        (update 0 m/inc-program-counter))))

(defn load-program
  [machine program address]
  (when PRINT
    (println "\nLoad Program")
    (println (format "%-10s %20s" "Address" address))
    (console/print-data-numbered program))
  (reduce (fn [m [idx inst]]
            (m/set-memory m (+ address idx) inst))
          (m/set-program-counter machine address)
          (map-indexed vector program)))

(defn execute-program
  [machine]
  (when PRINT
    (println "\nExecuting program"))
  (loop [m machine]
    (console/print-machine m)
    (print "\nPress any key to continue ")
    (flush)
    (read-line)
    (println)
    (let [[m2 halted?] (execute-next-instruction m)]
      (if halted?
        (do
          (println "\nProgram terminated")
          m2)
        (recur m2)))))
