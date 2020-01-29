(ns mix-machine.data-test
  (:require [mix-machine.data :as sut]
            [clojure.test :as t]))

(defn test-throws-invalid-mix-data
  [f & args]
  (t/testing "throws if not valid mix data"
    (t/is (thrown? AssertionError (apply f "hello" args)))
    (t/is (thrown? AssertionError (apply f \space args)))
    (t/is (thrown? AssertionError (apply f 100 args)))
    (t/is (thrown? AssertionError (apply f 3.14 args)))
    (t/is (thrown? AssertionError (apply f [0 1 2 3] args)))
    (t/is (thrown? AssertionError (apply f {::sut/sign :plus} args)))
    (t/is (thrown? AssertionError (apply f {::sut/bytes [0 0 0]} args)))
    (t/is (apply f {::sut/sign :plus ::sut/bytes [1 2 3]} args))))

(t/deftest new-data
  (t/testing "throws if sign is invalid"
    (t/is (thrown? AssertionError (sut/new-data :bad-sign 5)))
    (t/is (thrown? AssertionError (sut/new-data "-" [0 0 0])))
    (t/is (thrown? AssertionError (sut/new-data + [0 0 0])))
    (t/is (thrown? AssertionError (sut/new-data \+ [0 0 0])))
    (t/is (thrown? AssertionError (sut/new-data :Minus [0 0 0])))
    (t/is (sut/new-data :plus [0]))
    (t/is (sut/new-data :minus [0])))
  (t/testing "throws if second arg is not an int or valid bytes"
    (t/is (thrown? Exception (sut/new-data :plus "hello")))
    (t/is (thrown? Exception (sut/new-data :plus ["0" "1" "2"])))
    (t/is (thrown? Exception (sut/new-data :minus "10")))
    (t/is (thrown? Exception (sut/new-data :minus [0.1 0.2 0.3])))
    (t/is (sut/new-data :minus [1 2 3]))
    (t/is (sut/new-data :plus 10)))
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
    (t/is (= 3 (sut/count-bytes (sut/new-data :plus [63 62 61]))))))

(t/deftest negate-test
  (test-throws-invalid-mix-data sut/negate)
  (t/testing "converts :plus to :minus and vice-versa"
    (t/is (= :minus (sut/sign (sut/negate (sut/new-data 10)))))
    (t/is (= :plus (sut/sign (sut/negate (sut/new-data :minus [1 2 3])))))))

(t/deftest set-sign-test
  (test-throws-invalid-mix-data sut/set-sign :plus)
  (t/testing "throws if not valid sign"
    (t/is (thrown? AssertionError (sut/set-sign (sut/new-data 5) "hello")))
    (t/is (thrown? AssertionError (sut/set-sign (sut/new-data 5) 100)))
    (t/is (thrown? AssertionError (sut/set-sign (sut/new-data 5) \space)))
    (t/is (thrown? AssertionError (sut/set-sign (sut/new-data 5) \+)))
    (t/is (thrown? AssertionError (sut/set-sign (sut/new-data 5) "+")))
    (t/is (thrown? AssertionError (sut/set-sign (sut/new-data 5) [0 0 0])))
    (t/is (sut/set-sign (sut/new-data 5) :plus))
    (t/is (sut/set-sign (sut/new-data 5) :minus)))
  (t/testing "returned data has the new sign"
    (t/is (= :plus (sut/sign (sut/set-sign (sut/new-data :minus [0]) :plus))))
    (t/is (= :plus (sut/sign (sut/set-sign (sut/new-data :plus [0]) :plus))))
    (t/is (= :minus (sut/sign (sut/set-sign (sut/new-data :plus [0]) :minus))))
    (t/is (= :minus (sut/sign (sut/set-sign (sut/new-data :minus [0]) :minus))))))

(t/deftest truncate-test
  (test-throws-invalid-mix-data sut/truncate 10)
  (t/testing "throws if size is negative"
    (t/is (thrown? AssertionError (sut/truncate (sut/new-data 5) -1)))
    (t/is (thrown? AssertionError (sut/truncate (sut/new-data 5) -10000000)))
    (t/is (sut/truncate (sut/new-data 5) 0))
    (t/is (sut/truncate (sut/new-data 5) 1)))
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
  (t/testing "throws if size is negative"
    (t/is (thrown? AssertionError (sut/extend (sut/new-data 5) -1)))
    (t/is (thrown? AssertionError (sut/extend (sut/new-data 5) -10000000)))
    (t/is (sut/extend (sut/new-data 5) 0))
    (t/is (sut/extend (sut/new-data 5) 1)))
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
  (t/testing "throws if size is negative"
    (t/is (thrown? AssertionError (sut/extend (sut/new-data 5) -1)))
    (t/is (thrown? AssertionError (sut/extend (sut/new-data 5) -10000000)))
    (t/is (sut/extend (sut/new-data 5) 0))
    (t/is (sut/extend (sut/new-data 5) 1)))
  (t/testing "extends if size > number of bytes"
    (t/is (= (sut/extend (sut/new-data :minus [1 2 3]) 5)
             (sut/set-size (sut/new-data :minus [1 2 3]) 5))))
  (t/testing "truncates if size < number of bytes"
    (t/is (= (sut/truncate (sut/new-data :minus [1 2 3]) 1)
             (sut/set-size (sut/new-data :minus [1 2 3]) 1))))
  (t/testing "does nothing if size = number of bytes"
    (t/is (= (sut/new-data :minus [1 2 3])
             (sut/set-size (sut/new-data :minus [1 2 3]) 3)))))
