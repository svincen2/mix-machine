(ns mix-machine.machine
  (:require [clojure.math.numeric-tower :as math]))

;; NOTE - word is used rather loosely in here
;; Technically, a word is a sign and 5 bytes.
;; However, there isn't a defined term for a sign and 2 bytes (like the index register)
;; So, word ends up being used in functions that need to handle non-word data as well...

;; Byte: 0-63
;; Sign: + or -
;; Word: Sign, 5 bytes

(defn valid-byte?
  "Returns whether b is a valid MIX byte.
  Valid bytes are numeric values in range [0, 64)."
  [b]
  (<= 0 b 63))

(defn valid-bytes?
  "Returns whether bs is a collection of valid bytes."
  [bs]
  (not (some (complement valid-byte?) bs)))

(def valid-sign? #{:plus :minus})

(defn count-bytes
  "Return the number of bytes of the data"
  [data]
  (count (:bytes data)))

;; (defn valid-word?
;;   "Returns whether data is a valid MIX word."
;;   [data]
;;   (and (valid-sign? (:sign data))
;;        (valid-bytes? (:bytes data))
;;        (= 5 (count-bytes data))))

(defn new-data
  "Create new MIX data of a given size.
  In the second form, a sign and either a number of bytes
  or a vector of bytes is provided.
  If a number is given, the new data will contain that number of bytes,
  initialized to 0.  Otherwise, the bytes given will be used."
  ([num-bytes]
   (new-data :plus num-bytes))
  ([sign num-or-bytes]
   (when-not (valid-sign? sign)
     (throw (ex-info "new-data: Invalid sign" {:arg sign})))
   (cond
     (int? num-or-bytes) {:sign sign :bytes (vec (repeat num-or-bytes 0))}
     (vector? num-or-bytes) (if-not (valid-bytes? num-or-bytes)
                              (throw (ex-info "new-data: Invalid bytes"
                                              {:arg num-or-bytes}))
                              {:sign sign :bytes num-or-bytes})
     :else (throw (ex-info "new-data: Expected a number or vector"
                           {:arg num-or-bytes})))))

(defn data->num
  "Determine the numeric representation of a unit of MIX data.
  data can be any size (typically 5 or 2 bytes, and a sign)"
  ([data]
   (if (nil? data)
     0
     (data->num (:sign data) (:bytes data))))
  ([sign bytes]
   ;; Bytes can only contain values between 0-63,
   ;; i.e. 64 values.
   ;; So we can treat each byte as a potition in a base-64 number
   ;; Example:
   ;; 0|0|0|1|2 =
   ;; 0*64^4 + 0*64^3 + 0*64^2 + 1*64^1 + 2*64^0
   ;; 0 + 0 + 0 + 64 + 2 = 66
   ;; Then, we just need to incorporate the sign,
   ;; which can be done by multiplying by 1 or -1,
   ;; depending on if the sign is :plus or :minus (respectively).
   (->> bytes
        reverse
        (map (fn [p b] (* (math/expt 64 p) b)) (range))
        (reduce + 0)
        (* ({:plus 1 :minus -1} sign)))))

(defn negate-data
  "Negates the sign of a the data."
  [data]
  (update data :sign #({:plus :minus :minus :plus} %)))

(defn set-data-sign
  "Set the sign of word."
  [data sign]
  (assoc-in data :sign sign))

(defn- truncate-data
  "Truncates some data by dropping the first (left-most) bytes 1, 2, 3, etc...
  Example, truncate 5 to 2 bytes: S|b1|b2|b3|b4|b5 => S|b4|b5."
  [data size]
  (if (< size (count-bytes data))
    (update data :bytes #(->> % reverse (take size) reverse vec))
    data))

(defn- extend-data
  "Extend data to fit the new size (expected to be bigger).
  Extends by adding 0s to the left.
  Example, extend 2 to 5 bytes: S|b1|b2 => S|0|0|0|b1|b2."
  [data size]
  (if (> size (count-bytes data))
    (update data :bytes #(into (vec (repeat (- size (count %)) 0)) %))
    data))

(defn set-data-size
  "Fixes the data size to a new size, which can be bigger or smaller.
  To save information, extension (if necessary) is performed before truncation."
  [data size]
  (-> data (extend-data size) (truncate-data size)))

;; Machine:
;; Registers:
;; - Accum (A) - Word
;; - Exten (X) - Word
;; - Index (I) - 6 registers (I1-I6), each a sign and 2 bytes
;; - Jump  (J) - 2 bytes, positive (i.e. fixed sign)

(def new-machine
  {:registers {:A (new-data 5)
               :X (new-data 5)
               :I [nil ;; NOTE: This is intentional (for indexing purposes).
                   (new-data 2) ;; I1
                   (new-data 2) ;; I2
                   (new-data 2) ;; I3
                   (new-data 2) ;; I4
                   (new-data 2) ;; I5
                   (new-data 2)];; I6
               :J (new-data 2)}
   :overflow false
   :condition-indicator :equal
   :memory (vec (repeat 4000 (new-data 5)))})

;; NOTE UNUSED
(def condition-indicator? #{:less :equal :greater})

(defn- get-index-register
  "Returns the index register i, if possible.
  i should be in range [1, 6].
  If i is not in range, default is returned if provided, otherwise nil is returned."
  ([machine i]
   (get-index-register machine i nil))
  ([machine i default]
   (or (get-in machine [:registers :I i])
       default)))

(defn get-register
  ([machine r]
   (get-register machine r nil))
  ([machine r default]
   (cond
     (coll? r) (let [[_ i] r] (get-index-register machine i default))
     :else (or (get-in machine [:registers r])
               default))))

(defn- set-index-register
  [machine i data]
  (if (<= 1 i 6)
    (assoc-in machine [:registers :I i] (set-data-size data 2))
    (throw (ex-info "set-index-register: Unknown register" {:i i :data data}))))

(defn- set-jump-register
  [machine data]
  (assoc-in machine [:registers :J] (-> data
                                        (set-data-sign :plus)
                                        (set-data-size 2))))

(defn set-register
  [machine r data]
  (cond
    (coll? r) (let [[_ i] r] (set-index-register machine i data))
    (= :J r) (set-jump-register machine data)
    (#{:A :X} r) (assoc-in machine [:registers r] (set-data-size data 5))
    :else (throw (ex-info "set-register: Unknown register" {:r r :data data}))))

(defn get-memory
  [machine m]
  (get-in machine [:memory m]))

(defn set-memory
  [machine m data]
  (assert (<= 0 m 3999))
  (assoc-in machine [:memory m] (set-data-size data 5)))
