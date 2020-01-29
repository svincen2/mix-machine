(ns mix-machine.char-test
  (:require [mix-machine.char :as sut]
            [clojure.test :as t]))

;; char->code is just a map, but code->char is based on it
(t/deftest code->char-test
  (t/testing "has same keys as (vals char->code)"
    (t/is (= (set (keys sut/code->char))
             (set (vals sut/char->code)))))
  (t/testing "has same vals as (keys char->code)"
    (t/is (= (set (vals sut/code->char))
             (set (keys sut/char->code))))))

(t/deftest str->code-test
  (t/testing "returns nil for non-strings"
    (t/is (= nil (sut/str->code 0)))
    (t/is (= nil (sut/str->code [])))
    (t/is (= nil (sut/str->code {})))
    (t/is (= nil (sut/str->code #{})))
    (t/is (= nil (sut/str->code nil))))
  (t/testing "produces a sequence of ints"
    (t/is (sequential? (sut/str->code "hello"))))
  (t/testing "returns nil for unknown characters"
    (t/is (= [33 nil 47] (sut/str->code "3#/"))))
  (t/testing "is case-insensitive"
    (t/is (= [8 5 13 13 16] (sut/str->code "HelLo")))))
