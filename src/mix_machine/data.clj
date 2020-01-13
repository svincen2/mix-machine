(ns mix-machine.data
  (:require [clojure.math.numeric-tower :as math]))

;; NOTE - word is used rather loosely in here
;; Technically, a word is a sign and 5 bytes.
;; However, there isn't a defined term for a sign and 2 bytes (like the index register)
;; So, word ends up being used in functions that need to handle non-word data as well...

;; Byte: 0-63
;; Sign: + or -
;; Word: Sign, 5 bytes

(def byte-size 64)

(defn valid-byte?
  "Returns whether b is a valid MIX byte.
  Valid bytes are numeric values in range [0, 64)."
  [b]
  (and (int? b)
       (<= 0 b (dec byte-size))))

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

(defn negate-data
  "Negates the sign of a the data."
  [data]
  (update data :sign #({:plus :minus :minus :plus} %)))

(defn set-data-sign
  "Set the sign of word."
  [data sign]
  (assoc-in data :sign sign))

(defn truncate-data
  "Truncates some data by dropping the first (left-most) bytes 1, 2, 3, etc...
  Example, truncate 5 to 2 bytes: S|b1|b2|b3|b4|b5 => S|b4|b5."
  [data size]
  (if (< size (count-bytes data))
    (update data :bytes #(->> % reverse (take size) reverse vec))
    data))

(defn extend-data
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
        (map (fn [p b] (* (math/expt byte-size p) b)) (range))
        (reduce + 0)
        (* ({:plus 1 :minus -1} sign)))))

(defn num->data
  "Determine the MIX data representation of an integer number.
  In the first form, the returned MIX data has exactly as many bytes
  as are needed to represent num.
  In the second form, the data returned can be extended to a certain size,
  however size cannot be used to truncate.  If you need to do that,
  pass the result of this function into a call to truncate-data. "
  ([num]
   (num->data num nil))
  ([num size]
   (let [sign (if (< num 0) :minus :plus)
         bytes (loop [bytes (list) n (math/abs num)]
                 (let [q (quot n byte-size)
                       r (rem n byte-size)]
                   (if (< q byte-size)
                     (vec (concat (list q r) bytes))
                     (recur (cons r bytes) q))))
         data (new-data sign bytes)]
     (if size
       (extend-data data size)
       data))))
