(ns mix-machine.machine
  (:require [clojure.math.numeric-tower :as math]))

;; Byte: 0-63
;; Sign: + or -
;; Word: Sign, 5 bytes

(defn valid-byte?
  [b]
  (<= 0 b 63))

(defn valid-bytes?
  [bs]
  (not (some (complement valid-byte?) bs)))

(def valid-sign? #{:plus :minus})

(defn new-word
  ([]
   (new-word :plus))
  ([sign]
   (new-word sign [0 0 0 0 0]))
  ([sign bytes]
   (assert (valid-sign? sign))
   (assert (valid-bytes? bytes))
   (assert (= 5 (count bytes)))
   {:sign sign :bytes bytes}))

(defn word->num
  ([word]
   (if (nil? word)
     0
     (word->num (:sign word) (:bytes word))))
  ([sign bytes]
   ;; Bytes can only contain values between 0-63,
   ;; i.e. 64 values.
   ;; So we can treat each byte as a potition in a base-64 number
   ;; Example:
   ;; 0|0|0|1|2 =
   ;; 0*64^4 + 0*64^3 + 0*64^2 + 1*64^1 + 2*64^0
   ;; 0 + 0 + 0 + 64 + 2 = 66
   (->> bytes
        reverse
        (map (fn [p b] (* (math/expt 64 p) b)) (range))
        (reduce + 0)
        (* ({:plus 1 :minus -1} sign)))))

;; Machine:
;; Registers:
;; - Accum (A) - Word
;; - Exten (X) - Word
;; - Index (I) - 6 registers (I1-I6), each a sign and 2 bytes
;; - Jump  (J) - 2 bytes, positive (i.e. fixed sign)

(defn new-index-reg
  ([]
   (new-index-reg :plus))
  ([sign]
   (new-index-reg sign [0 0]))
  ([sign bytes]
   (assert (valid-sign? sign))
   (assert (valid-bytes? bytes))
   (assert (= 2 (count bytes)))
   {:sign sign :bytes bytes}))

(def new-mix-machine
  {:registers {:A (new-word)
               :X (new-word)
               :I [nil
                   (new-index-reg)
                   (new-index-reg)
                   (new-index-reg)
                   (new-index-reg)
                   (new-index-reg)
                   (new-index-reg)]
               :J (new-index-reg)}
   :overflow false
   :condition-indicator :equal
   :memory (vec (repeat 4000 (new-word)))})

(def mix-condition-indicators #{:less :equal :greater})
