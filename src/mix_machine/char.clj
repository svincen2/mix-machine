(ns mix-machine.char
  (:require [clojure.string :as string]))

(def ^{:doc "Mapping from char to MIX character code."}
  char->code
  {\space 0
   \A 1
   \B 2
   \C 3
   \D 4
   \E 5
   \F 6
   \G 7
   \H 8
   \I 9
   \Δ 10
   \J 11
   \K 12
   \L 13
   \M 14
   \N 15
   \O 16
   \P 17
   \Q 18
   \R 19
   \Σ 20
   \Π 21
   \S 22
   \T 23
   \U 24
   \V 25
   \W 26
   \X 27
   \Y 28
   \Z 29
   \0 30
   \1 31
   \2 32
   \3 33
   \4 34
   \5 35
   \6 36
   \7 37
   \8 38
   \9 39
   \. 40
   \, 41
   \( 42
   \) 43
   \+ 44
   \- 45
   \* 46
   \/ 47
   \= 48
   \$ 49
   \< 50
   \> 51
   \@ 52
   \; 53
   \: 54
   \' 55})

(def ^{:doc "Mapping from MIX character code to char"}
  code->char
  (reduce-kv (fn [m k v] (assoc m v k)) {} char->code))

(defn str->code
  "Convert the string s into a seq of MIX character code.
  If s is not a string, returns nil."
  [s]
  (if (string? s)
    (map char->code (seq (string/upper-case s)))
    nil))
