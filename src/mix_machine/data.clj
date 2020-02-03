(ns mix-machine.data
  (:refer-clojure :rename {merge core-merge
                           extend core-extend})
  (:require [clojure.math.numeric-tower :as math]
            [mix-machine.char :as ch]
            [mix-machine.debug :as debug]
            [mix-machine.spec :as spec]))

;; NOTE - word is used rather loosely in here
;; Technically, a word is a sign and 5 bytes.
;; However, there isn't a defined term for a sign and 2 bytes (like the index register)
;; So, word ends up being used in functions that need to handle non-word data as well...

;; Byte: 0-63
;; Sign: + or -
;; Word: Sign, 5 bytes

;; Specs and validation fns ----------------------------------------------------------------------

(def sign-spec
  [[#{:plus :minus} ":plus or :minus"]])

(def valid-sign? (partial spec/valid? sign-spec))

(def byte-size 64)

(def word-size 5)

(def byte-spec
  [[int? "integer"]
   [#(< -1 % byte-size) "range [0, 63)"]])

(def valid-byte? (partial spec/valid? byte-spec))

(def byte-seq-spec
  [[sequential? "sequence"]
   [#(not (some (complement valid-byte?) %)) "valid bytes"]])

(def valid-bytes? (partial spec/valid? byte-seq-spec))

(def data-spec
  [[map? "map"]
   [#(valid-sign? (:sign %)) "valid sign"]
   [#(valid-bytes? (:bytes %)) "valid bytes"]])

(def valid-data? (partial spec/valid? data-spec))

(def data-seq-spec
  [[sequential? "sequence"]
   [#(not (some (complement valid-data?) %)) "valid data"]])

(def valid-data-seq? (partial spec/valid? data-seq-spec))

(def non-neg-int-spec
  [[int? "integer"]
   [#(not (neg? %)) "non-negative"]])

(def non-neg-int? (partial spec/valid? non-neg-int-spec))

(def char-seq-spec
  [[sequential? "sequence"]
   [#(not (some (complement char?) %)) "characters"]])

(def valid-char-seq? (partial spec/valid? char-seq-spec))

;; -----------------------------------------------------------------------------------------------

;; Assertions ------------------------------------------------------------------------------------

;; NOTE - These are macros so that the error is thrown from the calling function.
;; This simplifies the stack a bit, since the macro is expanded before evaluation.
(defmacro assert-valid-sign
  [sign]
  `(assert (valid-sign? ~sign)
           (str "Expected " (spec/explain ~sign-spec ~sign) ", found " ~sign)))

(defmacro assert-valid-byte
  [byte]
  `(assert (valid-byte? ~byte)
           (str "Expected " (spec/explain ~byte-spec ~byte) ", found " ~byte)))

(defmacro assert-valid-bytes
  [bytes]
  `(assert (valid-bytes? ~bytes)
           (str "Expected " (spec/explain ~byte-seq-spec ~bytes) ", found " ~bytes)))

(defmacro assert-valid-data
  [data]
  `(assert (valid-data? ~data)
           (str "Expected " (spec/explain ~data-spec ~data) ", found " ~data)))

(defmacro assert-data-seq
  [ds]
  `(assert (valid-data-seq? ~ds)
           (str "Expected " (spec/explain ~data-seq-spec ~ds) ", found " ~ds)))

(defmacro assert-non-neg-int
  [n]
  `(assert (non-neg-int? ~n)
           (str "Expected " (spec/explain ~non-neg-int-spec ~n) ", found " ~n)))

(defmacro assert-pos-int
  [n]
  `(assert (pos-int? ~n)
           (str "Expected positive integer, found " ~n)))

(defmacro assert-char-seq
  [chs]
  `(assert (valid-char-seq? ~chs)
           (str "Expected " (spec/explain ~char-seq-spec ~chs) ", found " ~chs)))

;; -----------------------------------------------------------------------------------------------

(defn get-sign
  "Returns the sign of data."
  [data]
  (assert-valid-data data)
  (:sign data))

(defn set-sign
  "Set the sign of data."
  [data sign]
  (assert-valid-data data)
  (assert-valid-sign sign)
  (assoc data :sign sign))

(defn update-sign
  "Updates the sign of data by applying f to the current sign,
  followed by args (i.e. as if by (apply f (get-sign data) args))."
  [data f & args]
  (assert-valid-data data)
  (apply update data :sign f args))

(defn get-bytes
  "Returns the bytes of data."
  [data]
  (assert-valid-data data)
  (:bytes data))

(defn update-bytes
  "Updates the bytes of data by applying f to the current bytes,
  followed by args (i.e. as if by (apply f (get-bytes data) args))."
  [data f & args]
  (assert-valid-data data)
  (apply update data :bytes f args))

(defn count-bytes
  "Return the number of bytes of the data"
  [data]
  (assert-valid-data data)
  (count (get-bytes data)))

(defn negate
  "Negates the sign of a the data."
  [data]
  (assert-valid-data data)
  (update-sign data #({:plus :minus :minus :plus} %)))

(defn- truncate
  "Truncates some data by dropping the first (left-most) bytes 1, 2, 3, etc...
  Example, truncate 5 to 2 bytes: S|b1|b2|b3|b4|b5 => S|b4|b5."
  [data size]
  (let [n (count-bytes data)]
    (if (< size n)
      (update-bytes data #(vec (drop (- n size) %)))
      data)))

(defn- extend
  "Extend data to fit the new size (expected to be bigger).
  Extends by adding 0s to the left.
  Example, extend 2 to 5 bytes: S|b1|b2 => S|0|0|0|b1|b2."
  [data size]
  (let [n (count-bytes data)]
    (if (> size n)
      (update-bytes data #(into (vec (repeat (- size n) 0)) %))
      data)))

(defn resize
  "Resizes the data to size bytes.
  If size is less than (count-bytes data), data is truncated.
  If size is greater than (count-bytes data), data is extended.
  Otherwise data is returned unchanged.
  The smallest size is 0.
  The largest size is theoretically unlimited."
  [data size]
  (assert-valid-data data)
  (assert-non-neg-int size)
  (let [s (compare size (count-bytes data))]
    (case s
      -1 (truncate data size)
       0 data
       1 (extend data size))))

(defn new-data
  "Create new MIX data of a given size.
  In the second form, a sign and either a number of bytes
  or a vector of bytes is provided.
  If a number is given, the new data will contain that number of bytes,
  initialized to 0.  Otherwise, the bytes given will be used.
  In the first form, the sign is assumed to be :plus."
  ([num-bytes]
   (new-data :plus num-bytes))
  ([sign num-or-bytes]
   (assert-valid-sign sign)
   (assert (or (non-neg-int? num-or-bytes)
               (valid-bytes? num-or-bytes))
           (format "Expected %s, found %s"
                   "integer or valid bytes"
                   (with-out-str (pr num-or-bytes))))
   (cond
     (int? num-or-bytes) {:sign sign :bytes (vec (repeat num-or-bytes 0))}
     (sequential? num-or-bytes) {:sign sign :bytes (vec num-or-bytes)}
     ;; NOTE - This shouldn't happen (below)
     ;; But just in case I'll leave it in for now...
     :else (throw (ex-info "new-data: Second Argument Invalid"
                           {:sign sign :num-or-bytes num-or-bytes})))))

(defn data->num
  "Determine the numeric representation of a unit of MIX data.
  data can be any size (typically 5 or 2 bytes, and a sign)"
  [data]
  (assert-valid-data data)
  (let [{:keys [sign bytes]} data]
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
   (assert (int? num) (format "Expected integer, found %s" num))
   (let [sign (if (< num 0) :minus :plus)
         bytes (loop [bytes (list) n (math/abs num)]
                 (let [q (quot n byte-size)
                       r (rem n byte-size)]
                   (cond
                     (= 0 q) (cons r bytes)
                     (< q byte-size) (concat (list q r) bytes)
                     :else (recur (cons r bytes) q))))]
     (new-data sign bytes)))
  ([num size]
   (assert-non-neg-int size)
   (let [data (num->data num)]
     (extend data size))))

(defn add
  "Adds MIX data together, returning MIX data.
  This function does not check for overflow!
  This function returns (only) as many bytes as is necessary to represent the result!
  If the result is 0, the sign of the first data element is used as the sign of the result."
  [& data]
  (assert-data-seq data)
  (let [sign (get-sign (first data))
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
  (assert-valid-data data)
  (assert-pos-int size)
  (let [{:keys [sign bytes]} data]
    (loop [parts [] rem bytes]
      (if (empty? rem)
        parts
        (recur (conj parts (new-data sign (take size rem)))
               (drop size rem))))))

(defn merge
  "Merge data together, keeping the sign of the first."
  [& data]
  (assert-data-seq data)
  (let [sign (get-sign (first data))]
    (new-data sign (apply concat (map get-bytes data)))))

(defn shift-left
  "Shift n bytes left.
  Left-most bytes are shifted out.
  Zeros replace right-most bytes.
  Sign is unchanged.
  Example: +|1|2|3|4|5 shift 3 => +|4|5|0|0|0."
  [data n]
  (assert-valid-data data)
  (assert-non-neg-int n)
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
  (assert-valid-data data)
  (assert-non-neg-int n)
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
  (assert-valid-data data)
  (assert-non-neg-int n)
  (if (empty? (get-bytes data))
    data
    (let [{:keys [sign bytes]} data
          shift (mod n (count bytes))]
      (new-data sign (concat (drop shift bytes)
                             (take shift bytes))))))

(defn cycle-right
  "Cycle n bytes right.
  Left-most bytes are replaced by right-most bytes.
  Sign is unchanged.
  Example: +|1|2|3|4|5 cycle 3 => +|3|4|5|1|2."
  [data n]
  (assert-valid-data data)
  (assert-non-neg-int n)
  (if (empty? (get-bytes data))
    data
    (let [{:keys [sign bytes]} data
          num-bytes (count bytes)
          shift (mod n (count bytes))
          keep (- num-bytes shift)]
      (new-data sign (concat (drop keep bytes)
                             (take keep bytes))))))

;; (defn max-value
;;   "Return MIX data of size representing the max value.
;;   NOTE A \"maximum value\" is defined for this implementation only!
;;        MIX makes no assumptions about max values and byte implementations.
;;        (e.g. 6-bit, 2-decimal, 4-ternary, etc...)"
;;   [size]
;;   (new-data :plus (repeat size (dec byte-size))))

(defn data->chars
  [data]
  (assert-valid-data data)
  (vec (map ch/code->char (:bytes data))))

(defn chars->data
  [chs]
  (assert-char-seq chs)
  (new-data :plus (map ch/char->code chs)))
