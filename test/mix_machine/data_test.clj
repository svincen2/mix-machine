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
  [["+"]
   ["plus"]
   [\+]
   [+]
   ['+]
   [:Plus]
   [:+]
   [:PLUS]
   ["-"]
   ["minus"]
   [\-]
   [-]
   ['-]
   [:Minus]
   [:-]
   [:MINUS]
   [[0 0 0]]
   [100]
   ["hello"]
   [3.14]])

(def good-sign [[:plus]
                [:minus]])

(def bad-byte-arg [[-1]
                   ["hello"]
                   [\space]
                   [3.14]
                   [{:a 1}]
                   [[100]]
                   [[-1]]
                   [[64 -100]]])

(def good-byte-arg [[0] [5] [10] [100]
                    [[]] [[0]] [[1 63]] [[2 33 44 55]]])

(def bad-mix-data
  [["hello"]
   [\space]
   [100]
   [3.14]
   [[1 2 3 4 5]]
   [{:Sign :plus :Bytes [0]}]
   [{:sign :plus}]
   [{:bytes [1 2 3 4 5]}]
   [{:sign :plus :bytes [64 -1]}]
   [{:sign "+" :bytes [1 2 3 4 5]}]
   [{:sign :minus :bytes nil}]
   [{:sign :plus :bytes {:a 1}}]])

(def good-mix-data
  [[{:sign :plus :bytes [1 2 3 4 5]}]
   [{:sign :minus :bytes [1 2 3 4 5]}]
   [{:sign :plus :bytes []}]
   [{:sign :plus :bytes [0 0]}]
   [{:sign :plus :bytes [63 63 63 63 63]}]])

(def bad-data-seq
  [[-1]
   ["hello"]
   [\space]
   [3.14]
   [1 2 3]
   ["hello" "goodbye"]
   [3.14 5.26]
   {:a 1 :b 2}
   #{3 4 5}])

(def good-data-seq
  [[{:sign :plus :bytes [1 0]} {:sign :minus :bytes [63 63 63 63]}]
   (list {:sign :minus :bytes [30]})
   [{:sign :plus :bytes [10 20]} {:sign :plus :bytes [0]} {:sign :plus :bytes []}]])

;; ;; TODO - When these fail, it's hard to tell what the failing data was
(defn test-throws-assertion-error
  [msg bad-data good-data f]
  (t/testing msg
    (dorun
     (for [d bad-data]
       (t/is (thrown? AssertionError (apply f d)))))
    (dorun
     (for [d good-data]
       (t/is (apply f d))))))

(def test-throws-invalid-mix-data
  (partial test-throws-assertion-error
           "throws if not valid MIX data"
           bad-mix-data
           good-mix-data))

(def test-throws-negative-size
  (partial test-throws-assertion-error
           "throws if size is negative"
           [[-1] [-2] [-10] [-100]]
           [[0] [1] [2] [10] [100]]))

(def test-throws-invalid-data-seq
  (partial test-throws-assertion-error
           "throws if args is not a seq of data"
           bad-data-seq
           good-data-seq))

(t/deftest new-data-test
  (test-throws-assertion-error
   "throws if sign is invalid"
   bad-sign
   good-sign
   #(sut/new-data % [0]))
  (test-throws-assertion-error
   "throws if second arg is not a non-negative int or valid bytes"
   bad-byte-arg
   good-byte-arg
   (partial sut/new-data :plus))
  (t/testing "single-arity form"
    (t/testing "assumes :plus sign"
      (t/is (= :plus (sut/get-sign (sut/new-data 10))))
      (t/is (= :plus (sut/get-sign (sut/new-data [1 2 3 4 5]))))))
  (t/testing "if bytes-arg is an int"
    (t/testing "returns bytes-arg number of bytes"
      (t/is (= 10 (count (sut/get-bytes (sut/new-data :plus 10)))))
      (t/is (= 10 (count (sut/get-bytes (sut/new-data 10))))))
    (t/testing "bytes are initialized to 0"
      (t/is (= [0 0 0] (sut/get-bytes (sut/new-data :minus 3))))
      (t/is (= [0 0 0] (sut/get-bytes (sut/new-data 3)))))))

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
    (t/is (= :minus (sut/get-sign (sut/negate (sut/new-data :plus 10)))))
    (t/is (= :plus (sut/get-sign (sut/negate (sut/new-data :minus [1 2 3])))))))

(t/deftest set-sign-test
  (test-throws-invalid-mix-data #(sut/set-sign % :plus))
  (test-throws-assertion-error "throws if not valid sign"
                               bad-sign
                               good-sign
                               (partial sut/set-sign (sut/new-data 5)))
  (t/testing "returned data has the new sign"
    (t/is (= :plus (sut/get-sign (sut/set-sign (sut/new-data :minus [0]) :plus))))
    (t/is (= :plus (sut/get-sign (sut/set-sign (sut/new-data :plus [0]) :plus))))
    (t/is (= :minus (sut/get-sign (sut/set-sign (sut/new-data :plus [0]) :minus))))
    (t/is (= :minus (sut/get-sign (sut/set-sign (sut/new-data :minus [0]) :minus))))))

(t/deftest resize-test
  (test-throws-invalid-mix-data #(sut/resize % 10))
  (test-throws-negative-size (partial sut/resize (sut/new-data 5)))
  (t/testing "if size > number of bytes"
    (t/testing "fills left-most bytes with 0s"
      (t/is (= [0 1 2 3] (sut/get-bytes (sut/resize (sut/new-data [1 2 3]) 4))))
      (t/is (= [0 0 1 2 3] (sut/get-bytes (sut/resize (sut/new-data [1 2 3]) 5))))
      (t/is (= [0 0 0 1 2 3] (sut/get-bytes (sut/resize (sut/new-data [1 2 3]) 6))))))
  (t/testing "if size < number of bytes"
    (t/testing "drops left-most bytes"
      (t/is (= [2 3 4 5] (sut/get-bytes (sut/resize (sut/new-data [1 2 3 4 5]) 4))))
      (t/is (= [3 4 5] (sut/get-bytes (sut/resize (sut/new-data [1 2 3 4 5]) 3))))
      (t/is (= [4 5] (sut/get-bytes (sut/resize (sut/new-data [1 2 3 4 5]) 2))))
      (t/is (= [5] (sut/get-bytes (sut/resize (sut/new-data [1 2 3 4 5]) 1)))))
    (t/testing "drops all bytes, if size = 0"
      (t/is (= [] (sut/get-bytes (sut/resize (sut/new-data 10) 0))))
      (t/is (= [] (sut/get-bytes (sut/resize (sut/new-data :minus [1 2 3]) 0))))))
  (t/testing "does nothing if size = number of bytes"
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/resize (sut/new-data :minus [1 2 3]) 3)))))

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
                                 [[3.14]
                                  ["hello"]
                                  [:100]
                                  [-4.6]
                                  [7/11]
                                  [\a]
                                  [[]]
                                  [{}]]
                                 [[0]
                                  [10]
                                  [-1]
                                  [-20]
                                  [100]
                                  [-10000]
                                  [1234]
                                  [5678900]]
                                 sut/num->data)
    (t/testing "returns (only) as many bytes as are needed"
      (t/is (= 1 (count (sut/get-bytes (sut/num->data (math/expt 64 0))))))
      (t/is (= 2 (count (sut/get-bytes (sut/num->data (math/expt 64 1))))))
      (t/is (= 3 (count (sut/get-bytes (sut/num->data (math/expt 64 2))))))
      (t/is (= 4 (count (sut/get-bytes (sut/num->data (math/expt 64 3))))))
      (t/is (= 5 (count (sut/get-bytes (sut/num->data (math/expt 64 4)))))))
    (t/testing "returns the correct sign"
      (t/is (= :minus (sut/get-sign (sut/num->data -1))))
      (t/is (= :minus (sut/get-sign (sut/num->data -100))))
      (t/is (= :plus (sut/get-sign (sut/num->data 100)))))
    (t/testing "sign is :plus for 0"
      (t/is (= :plus (sut/get-sign (sut/num->data 0))))))
  (t/testing "second form"
    (test-throws-negative-size (partial sut/num->data 1))
    (t/testing "if size greater than bytes needed, left-pads with 0s"
      (t/is (= [0 1] (sut/get-bytes (sut/num->data 1 2))))
      (t/is (= [0 0 1] (sut/get-bytes (sut/num->data 1 3)))))
    (t/testing "returns the correct sign"
      (t/is (= :minus (sut/get-sign (sut/num->data -1 5))))
      (t/is (= :minus (sut/get-sign (sut/num->data -100 5))))
      (t/is (= :plus (sut/get-sign (sut/num->data 100 5)))))))

(t/deftest add-test
  (test-throws-invalid-data-seq sut/add)
  (t/testing "returns the addition result, in MIX data"
    (t/is (= (sut/new-data :plus [1 1 0])
             (sut/add (sut/new-data [1 0])
                      (sut/new-data [1 0 0]))))
    (t/is (= (sut/new-data :plus [10 20])
             (sut/add (sut/new-data [10 10])
                      (sut/new-data [10]))))
    (t/is (= (sut/new-data :plus [1 0])
             (sut/add (sut/new-data [63])
                      (sut/new-data [1]))))
    (t/is (= (sut/new-data :plus [1 10 11])
             (sut/add (sut/new-data [60 60])
                      (sut/new-data [13 15])))))
  (t/testing "adds negative numbers correctly"
    (t/is (= (sut/new-data :plus [0])
             (sut/add (sut/new-data :plus [1])
                      (sut/new-data :minus [1]))))
    (t/is (= (sut/new-data :minus [1])
             (sut/add (sut/new-data :plus [10])
                      (sut/new-data :minus [11]))))
    (t/is (= (sut/new-data :minus [63])
             (sut/add (sut/new-data :minus [1 0])
                      (sut/new-data :plus [1])))))
  (t/testing "if result is 0, keeps sign of first data"
    (t/is (= (sut/new-data :plus [0])
             (sut/add (sut/new-data :plus [1])
                      (sut/new-data :minus [1]))))
    (t/is (= (sut/new-data :minus [0])
             (sut/add (sut/new-data :minus [1])
                      (sut/new-data :plus [1])))))
  (t/testing "returns the argument if only one"
    (t/is (= (sut/new-data :plus [30 31 32 33])
             (sut/add (sut/new-data :plus [30 31 32 33]))))))

(t/deftest split-test
  ;; when splitting into chunks
  ;; - if even split, each chunk has size bytes
  ;; - if uneven split, last chunk has only remaining bytes
  ;; - each chunk has same sign as data
  (test-throws-invalid-mix-data #(sut/split % 5))
  (test-throws-assertion-error
   "throws if size not positive integer"
   [["hello"]
    [3.14]
    [\space]
    [[1 2 3]]
    [{:a 1 :b 2}]
    [#{1 2 3}]
    [-1]
    [0]]
   [[1]
    [2]
    [3]
    [5]
    [10]
    [1000]]
   #(sut/split (sut/new-data :minus 10) %))
  (t/testing "when splitting into chunks"
    (t/testing "if even split, each chunk has size bytes"
      (let [d1 (sut/new-data :plus [1 2 3 4 5 6 7 8 9 0])
            size 2
            chunks (sut/split d1 size)]
        (t/is (= 5 (count chunks)))
        (dorun
         (map #(t/is (= %1 %2))
              chunks
              [(sut/new-data :plus [1 2])
               (sut/new-data :plus [3 4])
               (sut/new-data :plus [5 6])
               (sut/new-data :plus [7 8])
               (sut/new-data :plus [9 0])]))))
    (t/testing "if uneven split, last chunk has only remaining bytes"
      (let [d1 (sut/new-data :plus [1 2 3 4 5 6 7 8 9])
            size 5
            chunks (sut/split d1 size)]
        (t/is (= 2 (count chunks)))
        (t/is (= (sut/new-data :plus [1 2 3 4 5]) (first chunks)))
        (t/is (= (sut/new-data :plus [6 7 8 9]) (last chunks)))))
    (t/testing "returns [data] if size >= (count-bytes data)"
      (let [d (sut/new-data :plus [1 2 3 4 5])]
        (t/is (= [d] (sut/split d 5)))
        (t/is (= [d] (sut/split d 6)))
        (t/is (= [d] (sut/split d 10)))
        (t/is (= [d] (sut/split d 1000)))))
    (t/testing "each chunk has same sign as data"
      (let [d (sut/new-data :minus [1 2 3 4 5 6 7 8 9 0])
            chunks (sut/split d 2)]
        (dorun
         (map #(t/is (= :minus (sut/get-sign %)))
              chunks))))))

(t/deftest merge-test
  (test-throws-invalid-data-seq sut/merge)
  (t/testing "merged bytes are in same order as arguments"
    (let [d1 (sut/new-data [1 2])
          d2 (sut/new-data [3 4])
          d3 (sut/new-data [5 6])]
      (t/is (= [1 2 3 4] (sut/get-bytes (sut/merge d1 d2))))
      (t/is (= [3 4 5 6] (sut/get-bytes (sut/merge d2 d3))))
      (t/is (= [1 2 3 4 5 6] (sut/get-bytes (sut/merge d1 d2 d3))))
      (t/is (= [5 6 3 4 1 2] (sut/get-bytes (sut/merge d3 d2 d1))))))
  (t/testing "result has sign of the first argument"
    (let [d1 (sut/new-data :minus [1 2])
          d2 (sut/new-data :plus [3 4])
          d3 (sut/new-data :plus [5 6])]
      (t/is (= :minus (sut/get-sign (sut/merge d1 d2 d3))))
      (t/is (= :plus (sut/get-sign (sut/merge d2 d1 d3))))
      (t/is (= :plus (sut/get-sign (sut/merge d3 d2 d1)))))))

(t/deftest shift-left-test
  (test-throws-invalid-mix-data #(sut/shift-left % 1))
  (test-throws-negative-size #(sut/shift-left (sut/new-data 5) %))
  (t/testing "left bytes are shifted out"
    (let [d (sut/new-data [1 2 3 4 5])]
      (t/is (= 2 (first (sut/get-bytes (sut/shift-left d 1)))))
      (t/is (= 3 (first (sut/get-bytes (sut/shift-left d 2)))))
      (t/is (= 4 (first (sut/get-bytes (sut/shift-left d 3)))))
      (t/is (= 5 (first (sut/get-bytes (sut/shift-left d 4)))))))
  (t/testing "zeros are shifted into right bytes"
    (let [d (sut/new-data [1 2 3 4 5])]
      (t/is (= [2 3 4 5 0] (sut/get-bytes (sut/shift-left d 1))))
      (t/is (= [3 4 5 0 0] (sut/get-bytes (sut/shift-left d 2))))
      (t/is (= [4 5 0 0 0] (sut/get-bytes (sut/shift-left d 3))))
      (t/is (= [5 0 0 0 0] (sut/get-bytes (sut/shift-left d 4))))
      (t/is (= [0 0 0 0 0] (sut/get-bytes (sut/shift-left d 5))))
      (t/is (= [0 0 0 0 0] (sut/get-bytes (sut/shift-left d 1000))))))
  (t/testing "the sign is unchanged"
    (let [d (sut/new-data :minus [1 2 3 4 5])]
      (t/is (= :minus (sut/get-sign (sut/shift-left d 1))))
      (t/is (= :minus (sut/get-sign (sut/shift-left d 2))))
      (t/is (= :minus (sut/get-sign (sut/shift-left d 3))))
      (t/is (= :minus (sut/get-sign (sut/shift-left d 4))))
      (t/is (= :minus (sut/get-sign (sut/shift-left d 5))))
      (t/is (= :minus (sut/get-sign (sut/shift-left d 1000)))))))

(t/deftest shift-right-test
  (test-throws-invalid-mix-data #(sut/shift-right % 1))
  (test-throws-negative-size #(sut/shift-right (sut/new-data 5) %))
  (t/testing "zeros are shifted into left bytes"
    (let [d (sut/new-data [1 2 3 4 5])]
      (t/is (= '(0 1) (take 2 (sut/get-bytes (sut/shift-right d 1)))))
      (t/is (= '(0 0 1) (take 3 (sut/get-bytes (sut/shift-right d 2)))))
      (t/is (= '(0 0 0 1) (take 4 (sut/get-bytes (sut/shift-right d 3)))))
      (t/is (= '(0 0 0 0 1) (take 5 (sut/get-bytes (sut/shift-right d 4)))))
      (t/is (= '(0 0 0 0 0) (take 5 (sut/get-bytes (sut/shift-right d 5)))))))
  (t/testing "right bytes are shifted out"
    (let [d (sut/new-data [1 2 3 4 5])]
      (t/is (= [0 1 2 3 4] (sut/get-bytes (sut/shift-right d 1))))
      (t/is (= [0 0 1 2 3] (sut/get-bytes (sut/shift-right d 2))))
      (t/is (= [0 0 0 1 2] (sut/get-bytes (sut/shift-right d 3))))
      (t/is (= [0 0 0 0 1] (sut/get-bytes (sut/shift-right d 4))))
      (t/is (= [0 0 0 0 0] (sut/get-bytes (sut/shift-right d 5))))
      (t/is (= [0 0 0 0 0] (sut/get-bytes (sut/shift-right d 1000))))))
  (t/testing "the sign is unchanged"
    (let [d (sut/new-data :minus [1 2 3 4 5])]
      (t/is (= :minus (sut/get-sign (sut/shift-right d 1))))
      (t/is (= :minus (sut/get-sign (sut/shift-right d 2))))
      (t/is (= :minus (sut/get-sign (sut/shift-right d 3))))
      (t/is (= :minus (sut/get-sign (sut/shift-right d 4))))
      (t/is (= :minus (sut/get-sign (sut/shift-right d 5))))
      (t/is (= :minus (sut/get-sign (sut/shift-right d 1000)))))))

(t/deftest cycle-left-test
  (test-throws-invalid-mix-data #(sut/cycle-left % 1))
  (test-throws-negative-size #(sut/cycle-left (sut/new-data 5) %))
  (t/testing "if bytes are empty, returns data"
    (t/is (= (sut/new-data []) (sut/cycle-left (sut/new-data []) 10))))
  (t/testing "cycles left bytes to right bytes"
    (let [d (sut/new-data [1 2 3 4 5])]
      (t/is (= [2 3 4 5 1] (sut/get-bytes (sut/cycle-left d 1))))
      (t/is (= [3 4 5 1 2] (sut/get-bytes (sut/cycle-left d 2))))
      (t/is (= [4 5 1 2 3] (sut/get-bytes (sut/cycle-left d 3))))
      (t/is (= [5 1 2 3 4] (sut/get-bytes (sut/cycle-left d 4))))
      (t/is (= [1 2 3 4 5] (sut/get-bytes (sut/cycle-left d 5))))))
  (t/testing "if n > (count-bytes data), shifts by (mod size (count-bytes data))"
    (let [d (sut/new-data [1 2 3 4 5])]
      (t/is (= [2 3 4 5 1] (sut/get-bytes (sut/cycle-left d 6))))
      (t/is (= [3 4 5 1 2] (sut/get-bytes (sut/cycle-left d 7))))
      (t/is (= [4 5 1 2 3] (sut/get-bytes (sut/cycle-left d 13))))
      (t/is (= [5 1 2 3 4] (sut/get-bytes (sut/cycle-left d 19))))
      (t/is (= [1 2 3 4 5] (sut/get-bytes (sut/cycle-left d 50)))))))

(t/deftest cycle-right-test
  (test-throws-invalid-mix-data #(sut/cycle-right % 1))
  (test-throws-negative-size #(sut/cycle-right (sut/new-data 5) %))
  (t/testing "if bytes are empty, returns data"
    (t/is (= (sut/new-data []) (sut/cycle-right (sut/new-data []) 10))))
  (t/testing "cycles right bytes to left bytes"
    (let [d (sut/new-data [1 2 3 4 5])]
      (t/is (= [5 1 2 3 4] (sut/get-bytes (sut/cycle-right d 1))))
      (t/is (= [4 5 1 2 3] (sut/get-bytes (sut/cycle-right d 2))))
      (t/is (= [3 4 5 1 2] (sut/get-bytes (sut/cycle-right d 3))))
      (t/is (= [2 3 4 5 1] (sut/get-bytes (sut/cycle-right d 4))))
      (t/is (= [1 2 3 4 5] (sut/get-bytes (sut/cycle-right d 5))))))
  (t/testing "if n > (count-bytes data), shifts by (mod size (count-bytes data))"
    (let [d (sut/new-data [1 2 3 4 5])]
      (t/is (= [5 1 2 3 4] (sut/get-bytes (sut/cycle-right d 6))))
      (t/is (= [4 5 1 2 3] (sut/get-bytes (sut/cycle-right d 7))))
      (t/is (= [3 4 5 1 2] (sut/get-bytes (sut/cycle-right d 13))))
      (t/is (= [2 3 4 5 1] (sut/get-bytes (sut/cycle-right d 19))))
      (t/is (= [1 2 3 4 5] (sut/get-bytes (sut/cycle-right d 50)))))))

(t/deftest data->chars-test
  (test-throws-invalid-mix-data sut/data->chars)
  (t/testing "returns empty vector if bytes are empty"
    (t/is (= [] (sut/data->chars (sut/new-data [])))))
  (t/testing "returns vector of chars represting the MIX character code"
    (let [d (sut/new-data [8 5 13 13 16
                           30 31 32 33 34 35 36 37 38 39])]
      (t/is (= [\H \E \L \L \O
                \0 \1 \2 \3 \4 \5 \6 \7 \8 \9]
               (sut/data->chars d)))))
  (t/testing "returns nil for unknown character codes"
    (let [d (sut/new-data [50 51 52 53 54 55 56 57 58 59 60 61 62 63])]
      (t/is (= [\< \> \@ \; \: \' nil nil nil nil nil nil nil nil]
               (sut/data->chars d))))))

(t/deftest chars->data-test
  (test-throws-assertion-error
   "throws if invalid character sequence"
   [[["hello"]]
    [[123]]
    [[3.14]]
    [[{:a 1 :b 2}]]
    [[#{1 2 3}]]
    [["A" "B" "C"]]]
   [[[\A \B \C]]
    [[\0 \1 \2]]
    [[\space \* \+]]]
   sut/chars->data)
  (test-throws-assertion-error
   "throws if chs contains an unknown MIX character code"
   [[[\_ \A \B]]
    [[\A \B \_]]
    [[\* \+ \`]]]
   [[[\A \B \C]]
    [[\0 \1 \2]]
    [[\* \+ \-]]]
   sut/chars->data)
  (t/testing "returned data's sign is always :plus"
    (t/is (= :plus (sut/get-sign (sut/chars->data [\0 \1 \2]))))
    (t/is (= :plus (sut/get-sign (sut/chars->data [\A \B \C]))))
    (t/is (= :plus (sut/get-sign (sut/chars->data [\* \+ \-])))))
  (t/testing "returns MIX data representing char codes"
    (let [chs [\H \E \L \L \O \0 \1 \2 \3 \4 \5 \6 \7 \8 \9]]
      (t/is (= (sut/new-data [8 5 13 13 16
                              30 31 32 33 34 35 36 37 38 39])
               (sut/chars->data chs))))))
