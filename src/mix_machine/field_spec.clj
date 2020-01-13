(ns mix-machine.field-spec)

;; Field specification:
;; Usually of the form (L:R), where L is the left-most byte, and R is the right-most byte
;; that will be used in the operation.  However, F (field specification) is operation dependant,
;; and may not have anything to do with word byte ranges.
;; Example, (0:5) is the whole word, sign and all 5 bytes
;; (1:5) is the absolute value of the word (sign assumed to be positive)
;; (0:0) just the sign
;; In all cases, except for the sign, the bytes are right shifted:
;; -|1|2|3|4|5 (1:3) -> +|0|0|1|2|3
;; -|1|2|3|4|5 (0:3) -> -|0|0|1|2|3
;; F is represented 'in the machine' as 8L + R.
;; Example: (1:3) = 8*1 + 3 = 11
;;          (0:3) = 8*0 + 3 = 3
;; To go from machine F to (L:R) form, quot by 8 for the left part, and rem by 8 for right.
;; Example: F = 11, L = (quot 11 8) = 1, R = (rem 11 8) = 3.

;; Used to encode/decode a (L:R) field spec
(def field-spec-encode-value 8)

(defn decode-field-spec
  "Decodes the F-modification to a field specification (L:R)."
  [fmod]
  {:left (quot fmod field-spec-encode-value)
   :right (rem fmod field-spec-encode-value)})

(defn encode-field-spec
  [left right]
  (-> left
      (* field-spec-encode-value)
      (+ right)))

(defn- load-field-bytes
  [b left right]
  (let [num-bytes (count b)
        range-size (- right left)
        pad (repeat (- num-bytes range-size) 0)]
    (vec (concat pad (subvec b left right)))))

(defn load-field-spec
  [word fs]
  (let [{:keys [left right]} fs
        {:keys [sign bytes]} word]
    (cond
      (and (= 0 left) (= 0 right)) {:sign sign :bytes []}
      (= 0 left) {:sign sign :bytes (load-field-bytes bytes 0 right)}
      :else {:sign :plus :bytes (load-field-bytes bytes (dec left) right)})))

(defn store-field-bytes
  [b left right]
  )

(defn store-field-spec
  [data fs]
  )
