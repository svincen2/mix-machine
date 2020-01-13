(ns mix-machine.field-spec
  (:require [mix-machine.data :as d]))

(def DEBUG true)

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

(defn load-field-spec
  "LOAD application of a field spec.
  Returns the field of the word, right shifted.
  NOTE: This function does not left-pad zeros to ensure
  a certain number of bytes."
  [word field]
  (let [{:keys [left right]} field
        {:keys [sign bytes]} word]
    (d/new-data (if (= 0 left) sign :plus)
                (subvec bytes (max 0 (dec left)) right))))

(defn store-field-spec
  "STORE application of a field spec.
  Replaces the field in dest with the right-most bytes of source.
  If the field includes the sign, the sign is also replaced.
  NOTE: Nothing is actually 'replaced' - this function simply returns new data,
  leaving source, dest, and field untouched."
  [source dest field]
  (let [{:keys [left right]} field
        {s1 :sign b1 :bytes} source
        {s2 :sign b2 :bytes} dest
        left-byte (max 0 (dec left))
        range-size (- right left-byte)
        nb1 (drop (- (count b1) range-size) b1)]
    (d/new-data (if (= 0 left) s1 s2)
                (vec (concat (take left-byte b2) nb1 (drop right b2))))))
