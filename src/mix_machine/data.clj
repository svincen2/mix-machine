(ns mix-machine.data
  (:refer-clojure :rename {merge clj-merge
                           extend clj-extend})
  (:require [clojure.math.numeric-tower :as math]
            [mix-machine.char :as ch]))

;; NOTE - word is used rather loosely in here
;; Technically, a word is a sign and 5 bytes.
;; However, there isn't a defined term for a sign and 2 bytes (like the index register)
;; So, word ends up being used in functions that need to handle non-word data as well...

;; Byte: 0-63
;; Sign: + or -
;; Word: Sign, 5 bytes

(def byte-size 64)
(def byte-max (dec byte-size))

(defn valid-byte?
  "Returns whether b is a valid MIX byte.
  Valid bytes are numeric values in range [0, 64)."
  [b]
  (and (int? b)
       (<= 0 b byte-max)))

(defn valid-bytes?
  "Returns whether bs is a collection of valid bytes."
  [bs]
  (not (some (complement valid-byte?) bs)))

(def valid-sign? #{:plus :minus})

(defn count-bytes
  "Return the number of bytes of the data"
  [data]
  (count (:bytes data)))

(defn negate
  "Negates the sign of a the data."
  [data]
  (update data :sign #({:plus :minus :minus :plus} %)))

(defn set-sign
  "Set the sign of word."
  [data sign]
  (assoc data :sign sign))

(defn truncate
  "Truncates some data by dropping the first (left-most) bytes 1, 2, 3, etc...
  Example, truncate 5 to 2 bytes: S|b1|b2|b3|b4|b5 => S|b4|b5."
  [data size]
  (if (< size (count-bytes data))
    (update data :bytes #(->> % reverse (take size) reverse vec))
    data))

(defn extend
  "Extend data to fit the new size (expected to be bigger).
  Extends by adding 0s to the left.
  Example, extend 2 to 5 bytes: S|b1|b2 => S|0|0|0|b1|b2."
  [data size]
  (if (> size (count-bytes data))
    (update data :bytes #(into (vec (repeat (- size (count %)) 0)) %))
    data))

(defn set-size
  "Fixes the data size to a new size, which can be bigger or smaller.
  To save information, extension (if necessary) is performed before truncation."
  [data size]
  (-> data (extend size) (truncate size)))

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
     (sequential? num-or-bytes) (if-not (valid-bytes? num-or-bytes)
                                  (throw (ex-info "new-data: Invalid bytes"
                                                  {:arg num-or-bytes}))
                                  {:sign sign :bytes (vec num-or-bytes)})
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
  pass the result of this function into a call to truncate. "
  ([num]
   (num->data num nil))
  ([num size]
   (let [sign (if (< num 0) :minus :plus)
         bytes (loop [bytes (list) n (math/abs num)]
                 (let [q (quot n byte-size)
                       r (rem n byte-size)]
                   (cond
                     (= 0 q) (cons r bytes)
                     (< q byte-size) (concat (list q r) bytes)
                     :else (recur (cons r bytes) q))))
         data (new-data sign bytes)]
     (if size
       (extend data size)
       data))))

(defn add
  "Adds MIX data together, returning MIX data.
  This function does not check for overflow!
  This function returns (only) as many bytes as is necessary to represent the result!
  If the result is 0, the sign of the first data element is used as the sign of the result."
  [& data]
  (let [sign (:sign (first data))
        result (reduce + 0 (map data->num data))]
    ;; If the result is 0, we'll keep the sign of the first data element
    (if (= 0 result)
      (-> (num->data result)
          (set-sign sign))
      (num->data result))))

(defn split
  "Split the data into chunks, each with size number of bytes.
  Each chunk will have the same sign as data.
  If data's bytes cannot be split up evenly, the last chunk will
  have the remaining bytes."
  [data size]
  (let [{:keys [sign bytes]} data]
    (loop [parts [] rem bytes]
      (if (empty? rem)
        parts
        (recur (conj parts (new-data sign (take size rem)))
               (drop size rem))))))

(defn merge
  "Merge data together, keeping the sign of the first."
  [& data]
  (let [sign (:sign (first data))]
    (new-data sign (apply into (map :bytes data)))))

(defn shift-left
  "Shift n bytes left.
  Left-most bytes are shifted out.
  Zeros replace right-most bytes.
  Sign is unchanged.
  Example: +|1|2|3|4|5 shift 3 => +|4|5|0|0|0."
  [data n]
  (let [{:keys [sign bytes]} data
        shift (min (count bytes) n)]
    (new-data sign (concat (drop shift bytes)
                           (repeat shift 0)))))

(defn shift-right
  "Shift n bytes right.
  Right-most bytes are shifted out.
  Zeros replace left-most bytes.
  Sign is unchanged.
  Example: +|1|2|3|4|5 shift 3 => +|0|0|0|1|2."
  [data n]
  (let [{:keys [sign bytes]} data
        num-bytes (count bytes)
        shift (min num-bytes n)
        keep (- num-bytes shift)]
    (new-data sign (concat (repeat shift 0)
                           (take keep bytes)))))

(defn cycle-left
  "Cycle n bytes left.
  Right-most bytes are replaced by left-most bytes.
  Sign is unchanged.
  Example: +|1|2|3|4|5 cycle 3 => +|4|5|1|2|3."
  [data n]
  (let [{:keys [sign bytes]} data
        shift (mod n (count bytes))]
    (new-data sign (concat (drop shift bytes)
                           (take shift bytes)))))

(defn cycle-right
  "Cycle n bytes right.
  Left-most bytes are replaced by right-most bytes.
  Sign is unchanged.
  Example: +|1|2|3|4|5 cycle 3 => +|3|4|5|1|2."
  [data n]
  (let [{:keys [sign bytes]} data
        num-bytes (count bytes)
        shift (mod n (count bytes))
        keep (- num-bytes shift)]
    (new-data sign (concat (drop keep bytes)
                           (take keep bytes)))))

;; (defn max-value
;;   "Return MIX data of size representing the max value.
;;   NOTE A \"maximum value\" is defined for this implementation only!
;;        MIX makes no assumptions about max values and byte implementations.
;;        (e.g. 6-bit, 2-decimal, 4-ternary, etc...)"
;;   [size]
;;   (new-data :plus (repeat size byte-max)))

(defn data->chars
  [data]
  (vec (map ch/code->char (:bytes data))))

(defn chars->data
  [chs]
  (new-data :plus (map ch/char->code chs)))
