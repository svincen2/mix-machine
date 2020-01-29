(ns mix-machine.data-test
  (:require [mix-machine.data :as sut]
            [clojure.test :as t]
            [clojure.math.numeric-tower :as math]))

;; TODO - Type checking here
;; Does this help really?
;; Against:
;;  * Every function has a pre and post
;;    * Maybe the pre and post checks are wrong?  Testing can help catch that

(def bad-sign
  ["+" "plus" \+ + '+ :Plus :+ :PLUS
   "-" "minus" \- - '- :Minus :- :MINUS
   [0 0 0] 100 "hello" 3.14])
(def good-sign [:plus :minus])

(def bad-byte-arg [-1 "hello" \space 3.14 {:a 1}
                   [100] [-1] [64 -100]])
(def good-byte-arg [0 5 10 100
                    [] [0] [1 63] [2 33 44 55]])

(def bad-mix-data
  ["hello"
   \space
   100
   3.14
   [1 2 3 4 5]
   {:sign :plus :bytes [0]}                 ;; incorrect keys
   {::sut/sign :plus}                       ;; missing bytes
   {::sut/bytes [1 2 3 4 5]}                ;; missing sign
   {::sut/sign :plus ::sut/bytes [64 -1]}   ;; invalid byte
   {::sut/sign "+" ::sut/bytes [1 2 3 4 5]} ;; invalid sign
   {::sut/sign :minus ::sut/bytes nil}      ;; invalid bytes
   {::sut/sign :plus ::sut/bytes {:a 1}}    ;; invalid bytes
   ])

(def good-mix-data
  [{::sut/sign :plus ::sut/bytes [1 2 3 4 5]}
   {::sut/sign :minus ::sut/bytes [1 2 3 4 5]}
   {::sut/sign :plus ::sut/bytes []}
   {::sut/sign :plus ::sut/bytes [0 0]}
   {::sut/sign :plus ::sut/bytes [63 63 63 63 63]}])

;; TODO - When these fail, it's hard to tell what the failing data was
(defn test-throws-assertion-error
  [msg bad-data good-data f & args]
  (t/testing msg
    (dorun
     (for [d bad-data]
       (t/is (thrown? AssertionError (apply f d args)))))
    (dorun
     (for [d good-data]
       (t/is (apply f d args))))))

;; TODO - When these fail, it's hard to tell what the failing data was
(defn test-throws-exception
  [msg bad-data good-data f & args]
  (t/testing msg
    (dorun
     (for [d bad-data]
       (t/is (thrown? Exception (apply f d args)))))
    (dorun
     (for [d good-data]
       (t/is (apply f d args))))))

(def test-throws-invalid-mix-data
  (partial test-throws-assertion-error
           "throws if not valid MIX data"
           bad-mix-data
           good-mix-data))

(def test-throws-negative-size
  (partial test-throws-assertion-error
           "throws if size is negative"
           [-1 -2 -10 -100]
           [0 1 2 10 100]))

(t/deftest new-data
  (test-throws-exception "throws if sign is invalid"
                         bad-sign
                         good-sign
                         sut/new-data [0])
  (test-throws-exception "throws if second arg is not an int or valid bytes"
                         bad-byte-arg
                         good-byte-arg
                         (partial sut/new-data :plus))
  (t/testing "single-arity form"
    (t/testing "assumes :plus sign"
      (t/is (= :plus (sut/sign (sut/new-data 10))))
      (t/is (= :plus (sut/sign (sut/new-data [1 2 3 4 5]))))))
  (t/testing "if bytes-arg is an int"
    (t/testing "returns bytes-arg number of bytes"
      (t/is (= 10 (count (sut/bytes (sut/new-data :plus 10)))))
      (t/is (= 10 (count (sut/bytes (sut/new-data 10))))))
    (t/testing "bytes are initialized to 0"
      (t/is (= [0 0 0] (sut/bytes (sut/new-data :minus 3))))
      (t/is (= [0 0 0] (sut/bytes (sut/new-data 3)))))))

(t/deftest count-bytes-test
  (test-throws-invalid-mix-data sut/count-bytes)
  (t/testing "returns number of bytes"
    (t/is (= 10 (sut/count-bytes (sut/new-data :minus 10))))
    (t/is (= 5 (sut/count-bytes (sut/new-data [0 1 0 1 0]))))
    (t/is (= 3 (sut/count-bytes (sut/new-data :plus [63 62 61]))))
    (t/is (= 0 (sut/count-bytes (sut/new-data :plus []))))))

(t/deftest negate-test
  (test-throws-invalid-mix-data sut/negate)
  (t/testing "converts :plus to :minus and vice-versa"
    (t/is (= :minus (sut/sign (sut/negate (sut/new-data :plus 10)))))
    (t/is (= :plus (sut/sign (sut/negate (sut/new-data :minus [1 2 3])))))))

(t/deftest set-sign-test
  (test-throws-invalid-mix-data sut/set-sign :plus)
  (test-throws-assertion-error "throws if not valid sign"
                               bad-sign
                               good-sign
                               (partial sut/set-sign (sut/new-data 5)))
  (t/testing "returned data has the new sign"
    (t/is (= :plus (sut/sign (sut/set-sign (sut/new-data :minus [0]) :plus))))
    (t/is (= :plus (sut/sign (sut/set-sign (sut/new-data :plus [0]) :plus))))
    (t/is (= :minus (sut/sign (sut/set-sign (sut/new-data :plus [0]) :minus))))
    (t/is (= :minus (sut/sign (sut/set-sign (sut/new-data :minus [0]) :minus))))))

(t/deftest truncate-test
  (test-throws-invalid-mix-data sut/truncate 10)
  (test-throws-negative-size (partial sut/truncate (sut/new-data 5)))
  (t/testing "drops left-most bytes"
    (t/is (= [2 3 4 5] (sut/bytes (sut/truncate (sut/new-data [1 2 3 4 5]) 4))))
    (t/is (= [3 4 5] (sut/bytes (sut/truncate (sut/new-data [1 2 3 4 5]) 3))))
    (t/is (= [4 5] (sut/bytes (sut/truncate (sut/new-data [1 2 3 4 5]) 2))))
    (t/is (= [5] (sut/bytes (sut/truncate (sut/new-data [1 2 3 4 5]) 1)))))
  (t/testing "drops all bytes, if size = 0"
    (t/is (= [] (sut/bytes (sut/truncate (sut/new-data 10) 0))))
    (t/is (= [] (sut/bytes (sut/truncate (sut/new-data :minus [1 2 3]) 0)))))
  (t/testing "does nothing if size >= number of bytes"
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/truncate (sut/new-data :minus [1 2 3]) 3)))
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/truncate (sut/new-data :minus [1 2 3]) 4)))
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/truncate (sut/new-data :minus [1 2 3]) 100)))))

(t/deftest extend-test
  (test-throws-invalid-mix-data sut/extend 10)
  (test-throws-negative-size (partial sut/extend (sut/new-data 5)))
  (t/testing "fills left-most bytes with 0s"
    (t/is (= [0 1 2 3] (sut/bytes (sut/extend (sut/new-data [1 2 3]) 4))))
    (t/is (= [0 0 1 2 3] (sut/bytes (sut/extend (sut/new-data [1 2 3]) 5))))
    (t/is (= [0 0 0 1 2 3] (sut/bytes (sut/extend (sut/new-data [1 2 3]) 6)))))
  (t/testing "does nothing if size <= number of bytes"
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/extend (sut/new-data :minus [1 2 3]) 3)))
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/extend (sut/new-data :minus [1 2 3]) 2)))
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/extend (sut/new-data :minus [1 2 3]) 0)))))

(t/deftest set-size-test
  (test-throws-invalid-mix-data sut/set-size 10)
  (test-throws-negative-size (partial sut/set-size (sut/new-data 5)))
  (t/testing "extends if size > number of bytes"
    (t/is (= (sut/extend (sut/new-data :minus [1 2 3]) 5)
             (sut/set-size (sut/new-data :minus [1 2 3]) 5))))
  (t/testing "truncates if size < number of bytes"
    (t/is (= (sut/truncate (sut/new-data :minus [1 2 3]) 1)
             (sut/set-size (sut/new-data :minus [1 2 3]) 1))))
  (t/testing "does nothing if size = number of bytes"
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/set-size (sut/new-data :minus [1 2 3]) 3)))))

(t/deftest data->num-test
  (test-throws-invalid-mix-data sut/data->num)
  (t/testing "returns 0 if no bytes"
    (t/is (= 0 (sut/data->num (sut/new-data :plus [])))))
  (t/testing "returns 0 if all bytes are 0"
    (t/is (= 0 (sut/data->num (sut/new-data :plus [0]))))
    (t/is (= 0 (sut/data->num (sut/new-data :minus [0 0]))))
    (t/is (= 0 (sut/data->num (sut/new-data :plus [0 0 0 0 0]))))
    (t/is (= 0 (sut/data->num (sut/new-data :minus [0 0 0 0 0 0 0 0 0 0])))))
  (t/testing "bytes are read as a base-64 number"
    (t/is (= (math/expt 64 0) (sut/data->num (sut/new-data :plus [1]))))
    (t/is (= (math/expt 64 1) (sut/data->num (sut/new-data :plus [1 0]))))
    (t/is (= (math/expt 64 2) (sut/data->num (sut/new-data :plus [1 0 0]))))
    (t/is (= (math/expt 64 3) (sut/data->num (sut/new-data :plus [1 0 0 0]))))
    (t/is (= (math/expt 64 4) (sut/data->num (sut/new-data :plus [1 0 0 0 0]))))
    (t/is (= (+ (math/expt 64 1)
                (* 8 (math/expt 64 2)))
             (sut/data->num (sut/new-data :plus [8 1 0]))))
    (t/is (= (+ (* 63 (math/expt 64 4))
                (* 8 (math/expt 64 2)))
             (sut/data->num (sut/new-data :plus [63 0 8 0 0])))))
  (t/testing "returns the correct sign"
    (t/is (neg? (sut/data->num (sut/new-data :minus [1]))))
    (t/is (neg? (sut/data->num (sut/new-data :minus [63 0 8 0 0]))))
    (t/is (pos? (sut/data->num (sut/new-data :plus [63 0 8 0 0]))))))

(t/deftest num->data-test
  (t/testing "first form"
    (test-throws-assertion-error "throws if not an int"
                                 [3.14 "hello" :100 -4.6 7/11 \a [] {}]
                                 [0 10 -1 -20 100 -10000 1234 5678900]
                                 sut/num->data)
    ;; returns only as much bytes as needed
    ;; returns the correct sign
    )
  (t/testing "second form"
    ;; throws if negative size
    ;; returns size number of bytes, if greater than bytes needed
    ;; returns the correct sign
    )
  )
