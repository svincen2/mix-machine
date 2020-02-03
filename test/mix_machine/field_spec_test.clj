(ns mix-machine.field-spec-test
  (:require [mix-machine.field-spec :as sut]
            [mix-machine.data :as d]
            [clojure.test :as t]))

(t/deftest decode-field-spec-test
  (t/testing "throws if fmod is not a MIX byte"
    (doseq [bad-byte [-1 -100 100 64]]
      (t/is (thrown? AssertionError (sut/decode-field-spec bad-byte)))))
  (t/testing "decodes to (L:R) format"
    (t/is (= {:left 0 :right 0} (sut/decode-field-spec 0)))
    (t/is (= {:left 0 :right 1} (sut/decode-field-spec 1)))
    (t/is (= {:left 0 :right 2} (sut/decode-field-spec 2)))
    (t/is (= {:left 0 :right 3} (sut/decode-field-spec 3)))
    (t/is (= {:left 0 :right 4} (sut/decode-field-spec 4)))
    (t/is (= {:left 0 :right 5} (sut/decode-field-spec 5)))
    (t/is (= {:left 1 :right 0} (sut/decode-field-spec 8)))
    (t/is (= {:left 1 :right 1} (sut/decode-field-spec 9)))
    (t/is (= {:left 1 :right 5} (sut/decode-field-spec 13)))
    (t/is (= {:left 5 :right 5} (sut/decode-field-spec 45)))))

(t/deftest encode-field-spec-test
  (t/testing "throws if left is not in range [0, 5]"
    (doseq [bad-left [-1 -100 6 10 100]]
      (t/is (thrown? AssertionError (sut/encode-field-spec bad-left 0)))))
  (t/testing "throws if right is not in range [0, 5]"
    (doseq [bad-right [-1 -100 6 10 100]]
      (t/is (thrown? AssertionError (sut/encode-field-spec 0 bad-right)))))
  (t/testing "encodes as 8L + R"
    (t/is (= 0 (sut/encode-field-spec 0 0)))
    (t/is (= 1 (sut/encode-field-spec 0 1)))
    (t/is (= 2 (sut/encode-field-spec 0 2)))
    (t/is (= 3 (sut/encode-field-spec 0 3)))
    (t/is (= 4 (sut/encode-field-spec 0 4)))
    (t/is (= 5 (sut/encode-field-spec 0 5)))
    (t/is (= 8 (sut/encode-field-spec 1 0)))
    (t/is (= 9 (sut/encode-field-spec 1 1)))
    (t/is (= 10 (sut/encode-field-spec 1 2)))
    (t/is (= 11 (sut/encode-field-spec 1 3)))
    (t/is (= 12 (sut/encode-field-spec 1 4)))
    (t/is (= 13 (sut/encode-field-spec 1 5)))
    (t/is (= 45 (sut/encode-field-spec 5 5)))))

(t/deftest field-spec-str-test
  (t/testing "throws if fmod is not a MIX byte"
    (doseq [bad-byte [-1 -100 100 64]]
      (t/is (thrown? AssertionError (sut/decode-field-spec bad-byte)))))
  (t/testing "returns a string in (L:R) format"
    (t/is (= "(0:0)" (sut/field-spec-str 0)))
    (t/is (= "(0:1)" (sut/field-spec-str 1)))
    (t/is (= "(1:2)" (sut/field-spec-str 10)))
    (t/is (= "(3:3)" (sut/field-spec-str 27)))
    (t/is (= "(2:4)" (sut/field-spec-str 20)))
    (t/is (= "(0:5)" (sut/field-spec-str 5)))
    (t/is (= "(5:5)" (sut/field-spec-str 45)))))

(t/deftest load-field-spec-test
  (t/testing "throws if invalid MIX data"
    (let [fs {:left 0 :right 0}]
      (t/is (thrown? AssertionError (sut/load-field-spec "hello" fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec 100 fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec \space fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec [] fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec {} fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec #{} fs)))
      (t/is (sut/load-field-spec (d/new-data 5) fs))))
  (t/testing "throws if not a 5-byte MIX word"
    (let [fs {:left 0 :right 0}]
      (t/is (thrown? AssertionError (sut/load-field-spec (d/new-data []) fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec (d/new-data [1]) fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec (d/new-data [1 2]) fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec (d/new-data [1 2 3]) fs)))
      (t/is (thrown? AssertionError (sut/load-field-spec (d/new-data [1 2 3 4 5 6]) fs)))
      (t/is (sut/load-field-spec (d/new-data [1 2 3 4 5]) fs))))
  (t/testing "throws if invalid field"
    (let [d (d/new-data [1 2 3 4 5])]
      (t/is (thrown? AssertionError (sut/load-field-spec d "hello")))
      (t/is (thrown? AssertionError (sut/load-field-spec d 100)))
      (t/is (thrown? AssertionError (sut/load-field-spec d [1 2 3])))
      (t/is (thrown? AssertionError (sut/load-field-spec d {:a 1 :b 2})))
      (t/is (thrown? AssertionError (sut/load-field-spec d #{})))
      (t/is (thrown? AssertionError (sut/load-field-spec d \space)))
      (t/is (sut/load-field-spec d {:left 0 :right 0}))))
  (t/testing "uses the sign, if left = 0"
    (let [d (d/new-data :minus [1 2 3 4 5])]
      (t/is (= :minus (d/get-sign (sut/load-field-spec d {:left 0 :right 0}))))
      (t/is (= :minus (d/get-sign (sut/load-field-spec d {:left 0 :right 1}))))
      (t/is (= :minus (d/get-sign (sut/load-field-spec d {:left 0 :right 5}))))))
  (t/testing "numbers bytes from left to right, starting at 1"
    (let [d (d/new-data :plus [1 2 3 4 5])]
      (t/is (= [1] (d/get-bytes (sut/load-field-spec d {:left 1 :right 1}))))
      (t/is (= [1 2] (d/get-bytes (sut/load-field-spec d {:left 1 :right 2}))))
      (t/is (= [2 3 4 5] (d/get-bytes (sut/load-field-spec d {:left 2 :right 5})))))))

(t/deftest store-field-spec-test
  (t/testing "throws if source is invalid MIX data"
    (let [fs {:left 0 :right 0}
          d (d/new-data [1 2 3 4 5])]
      (t/is (thrown? AssertionError (sut/store-field-spec "hello" d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec 100 d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec \space d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec [] d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec {} d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec #{} d fs)))
      (t/is (sut/store-field-spec (d/new-data 5) d fs))))
  (t/testing "throws if dest is invalid MIX data"
    (let [fs {:left 0 :right 0}
          d (d/new-data [1 2 3 4 5])]
      (t/is (thrown? AssertionError (sut/store-field-spec d "hello" fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d 100 fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d \space fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d [] fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d {} fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d #{} fs)))
      (t/is (sut/store-field-spec (d/new-data 5) d fs))))
  (t/testing "throws if source is not a 5-byte MIX word"
    (let [fs {:left 0 :right 0}
          d (d/new-data [1 2 3 4 5])]
      (t/is (thrown? AssertionError (sut/store-field-spec (d/new-data []) d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec (d/new-data [1]) d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec (d/new-data [1 2]) d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec (d/new-data [1 2 3]) d fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec (d/new-data [1 2 3 4 5 6]) d fs)))
      (t/is (sut/store-field-spec (d/new-data [1 2 3 4 5]) d fs))))
  (t/testing "throws if dest is not a 5-byte MIX word"
    (let [fs {:left 0 :right 0}
          d (d/new-data [1 2 3 4 5])]
      (t/is (thrown? AssertionError (sut/store-field-spec d (d/new-data []) fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d (d/new-data [1]) fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d (d/new-data [1 2]) fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d (d/new-data [1 2 3]) fs)))
      (t/is (thrown? AssertionError (sut/store-field-spec d (d/new-data [1 2 3 4 5 6]) fs)))
      (t/is (sut/store-field-spec d (d/new-data [1 2 3 4 5]) fs))))
  (t/testing "throws if invalid field"
    (let [d (d/new-data [1 2 3 4 5])]
      (t/is (thrown? AssertionError (sut/store-field-spec d d "hello")))
      (t/is (thrown? AssertionError (sut/store-field-spec d d 100)))
      (t/is (thrown? AssertionError (sut/store-field-spec d d [1 2 3])))
      (t/is (thrown? AssertionError (sut/store-field-spec d d {:a 1 :b 2})))
      (t/is (thrown? AssertionError (sut/store-field-spec d d #{})))
      (t/is (thrown? AssertionError (sut/store-field-spec d d \space)))
      (t/is (sut/store-field-spec d d {:left 0 :right 0}))))
  (t/testing "uses the sign, if left = 0"
    (let [d (d/new-data :minus [1 2 3 4 5])]
      (t/is (= :minus (d/get-sign (sut/store-field-spec d d {:left 0 :right 0}))))
      (t/is (= :minus (d/get-sign (sut/store-field-spec d d {:left 0 :right 1}))))
      (t/is (= :minus (d/get-sign (sut/store-field-spec d d {:left 0 :right 5}))))))
  (t/testing "replaces field in dest with right-bytes of source"
    (let [d1 (d/new-data :minus [1 2 3 4 5])
          d2 (d/new-data :plus [6 7 8 9 0])]
      (t/is (= (d/new-data :minus [1 2 3 4 5]) (sut/store-field-spec d1 d2 {:left 0 :right 5})))
      (t/is (= (d/new-data :minus [5 7 8 9 0]) (sut/store-field-spec d1 d2 {:left 0 :right 1})))
      (t/is (= (d/new-data :plus [5 7 8 9 0]) (sut/store-field-spec d1 d2 {:left 1 :right 1})))
      (t/is (= (d/new-data :plus [3 4 5 9 0]) (sut/store-field-spec d1 d2 {:left 1 :right 3})))
      (t/is (= (d/new-data :plus [6 7 8 4 5]) (sut/store-field-spec d1 d2 {:left 4 :right 5})))
      (t/is (= (d/new-data :plus [6 7 8 5 0]) (sut/store-field-spec d1 d2 {:left 4 :right 4}))))))
