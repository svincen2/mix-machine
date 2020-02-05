(ns mix-machine.machine-test
  (:require [mix-machine.machine :as sut]
            [clojure.test :as t]
            [clojure.data :refer [diff]]
            [mix-machine.data :as d]
            [mix-machine.machine :as m]
            [clojure.set :as set]))

;; TODO I think we need a fixture here,
;; with a pre-setup machine.

;; TODO What about checking if the machine argument is valid?
;; We'll need a machine spec, and valid-machine? and assert-valid-machine functions

(t/deftest valid-condition-indicator-test
  (t/testing "only allows :less, :equal, and :greater"
    (t/is (sut/valid-condition-indicator? :less))
    (t/is (sut/valid-condition-indicator? :equal))
    (t/is (sut/valid-condition-indicator? :greater))
    (t/is (not (sut/valid-condition-indicator? :blah)))
    (t/is (not (sut/valid-condition-indicator? 100)))
    (t/is (not (sut/valid-condition-indicator? "hello")))
    (t/is (not (sut/valid-condition-indicator? <)))))

(t/deftest get-register-test
  (t/testing "first form"
    (t/testing "returns nil for unknown registers"
      (doseq [ur [:a :x :j :i1 :I1 "hello" [:i 1] [:I 0]]]
        (t/is (nil? (sut/get-register sut/new-machine ur))))))
  (t/testing "second form"
    (t/testing "returns default for unknown register"
      (let [d :default-returned]
        (doseq [ur [:a :x :j :i1 :I1 "hello" [:i 1] [:I 0]]]
          (t/is (= d (sut/get-register sut/new-machine ur d)))))))
  (t/testing "returns non-nil for valid register keys #{:A :X :J [:I i[1,6]]}"
    (doseq [r [:A :X :J [:I 1] [:I 2] [:I 6]]]
      (t/is (not (nil? (sut/get-register sut/new-machine r))))))
  (t/testing "returned registers are the correct registers"
    (let [m {:registers {:A (d/new-data :plus [1 2 3 4 5])
                         :X (d/new-data :minus [6 7 8 9 0])
                         :I [nil ;; always nil
                             (d/new-data :plus [1 2])
                             (d/new-data :plus [2 3])
                             (d/new-data :plus [3 4])
                             (d/new-data :plus [4 5])
                             (d/new-data :plus [5 6])
                             (d/new-data :plus [6 7])]
                         :J (d/new-data :plus [0 1])}}]
      (t/is (= (d/new-data :plus [1 2 3 4 5]) (sut/get-register m :A)))
      (t/is (= (d/new-data :minus [6 7 8 9 0]) (sut/get-register m :X)))
      (t/is (= (d/new-data :plus [1 2]) (sut/get-register m [:I 1])))
      (t/is (= (d/new-data :plus [2 3]) (sut/get-register m [:I 2])))
      (t/is (= (d/new-data :plus [3 4]) (sut/get-register m [:I 3])))
      (t/is (= (d/new-data :plus [4 5]) (sut/get-register m [:I 4])))
      (t/is (= (d/new-data :plus [5 6]) (sut/get-register m [:I 5])))
      (t/is (= (d/new-data :plus [6 7]) (sut/get-register m [:I 6])))
      (t/is (= (d/new-data :plus [0 1]) (sut/get-register m :J))))))

(t/deftest set-register-test
  (t/testing "throws if data is invalid"
    (doseq [bd ["hello" \space 100 3.14 [1 2 3] #{1 2 3} {:sign :blah :bytes ["hello"]}]]
      (t/is (thrown? AssertionError (sut/set-register sut/new-machine :A bd)))))
  (t/testing "throws if register is invalid"
    (doseq [br [:a :x :j "A" \A :i1 [:i 1] [:I 0]]]
      (t/is (thrown? AssertionError (sut/set-register sut/new-machine br (d/new-data 5))))))
  ;; TODO - throws if machine is invalid?
  (t/testing "returns new machine with register updated"
    (let [d (d/new-data :minus [1 2 3 4 5])
          r :A
          m (sut/set-register sut/new-machine r d)
          df (diff sut/new-machine m)]
      (t/is (= d (sut/get-register m r)))
      ;; Result of diff should only have the register has being different
      (t/is (= {:registers {r (sut/get-register m/new-machine r)}}
               (first df)))
      (t/is (= {:registers {r d}}
               (second df)))
      (t/is (= (update sut/new-machine :registers dissoc r)
               (last df))))
    (let [d (d/new-data :plus [1 1])
          r [:I 1]
          m (sut/set-register sut/new-machine r d)
          df (diff sut/new-machine m)]
      (t/is (= d (sut/get-register m r)))
      ;; Result of diff should only have the register has being different
      (t/is (= {:registers {:I [nil
                                {:bytes [0 0]}]}}
               (first df)))
      (t/is (= {:registers {:I [nil
                                {:bytes [1 1]}]}}
               (second df)))
      (t/is (= (-> sut/new-machine
                   (update-in [:registers :I] assoc 1 {:sign :plus}))
               (last df)))))
  (t/testing "resizes data to fit the register"
    (let [d (d/new-data :minus [1 2 3 4 5])
          m (sut/set-register sut/new-machine [:I 6] d)]
      (t/is (= (d/new-data :minus [4 5]) (sut/get-register m [:I 6]))))
    (let [d (d/new-data :minus [1 2])
          m (sut/set-register sut/new-machine :A d)]
      (t/is (= (d/new-data :minus [0 0 0 1 2]) (sut/get-register m :A)))))
  (t/testing "does not set sign of jump register"
    (let [d (d/new-data :minus [1 2])
          m (sut/set-register sut/new-machine :J d)]
      (t/is (= (d/new-data :plus [1 2]) (sut/get-register m :J))))))

(t/deftest inc-register-test
  (t/testing "throws if data is invalid"
    (doseq [bd ["hello" \space 100 3.14 [1 2 3] #{1 2 3} {:sign :blah :bytes ["hello"]}]]
      (t/is (thrown? AssertionError (sut/inc-register sut/new-machine :A bd)))))
  (t/testing "throws if register is invalid"
    (doseq [br [:a :x :j "A" \A :i1 [:i 1] [:I 0]]]
      (t/is (thrown? AssertionError (sut/inc-register sut/new-machine br (d/new-data 5))))))
  (t/testing "adds data to register"
    (let [d (d/new-data :plus [1 2])
          m1 (sut/set-register sut/new-machine :A (d/new-data :plus [1 2]))
          m2 (sut/inc-register m1 :A d)]
      (t/is (= (d/new-data :plus [0 0 0 2 4]) (sut/get-register m2 :A))))
    ;; Jump register is interesting, since the sign doesn't change
    (let [d (d/new-data :plus [1 2])
          m1 (sut/set-register sut/new-machine :J (d/new-data :plus [3 4]))
          m2 (sut/inc-register m1 :J d)]
      (t/is (= (d/new-data :plus [4 6]) (sut/get-register m2 :J))))
    (let [d (d/new-data :minus [1 2])
          m1 (sut/set-register sut/new-machine :J (d/new-data :plus [3 4]))
          m2 (sut/inc-register m1 :J d)]
      (t/is (= (d/new-data :plus [2 2]) (sut/get-register m2 :J))))
    ;; NOTE Below, the result of addition should be {:minus [1 2]}, since we
    ;; are subtracting from {:plus [0 0]} - however, the jump register is always positive,
    ;; so after the increment, the sign is reset.
    (let [d (d/new-data :minus [1 2])
          m1 (sut/inc-register sut/new-machine :J d)]
      (t/is (= (d/new-data :plus [1 2]) (sut/get-register m1 :J))))))

;; The rest is primarily get/set stuff, so it should be fairly straightforward...
;; Except set-memory, since that has logic to handle setting ranges

(t/deftest get-memory-test
  (t/testing "throws if m is an invalid memory location"
    (doseq [bm ["hello" 3.14 {:a 1 :b 2} #{1 2 3} [1 2 3]]]
      (t/is (thrown? AssertionError (sut/get-memory sut/new-machine bm)))))
  (t/testing "throws if size is not positive int"
    (doseq [bs ["hello" \space 3.14 -1 0 {:a 1 :b 2} [1 2 3]]]
      (t/is (thrown? AssertionError (sut/get-memory sut/new-machine 0 bs)))))
  (t/testing "throws if requesting an invalid memory range"
    (t/is (thrown? AssertionError (sut/get-memory sut/new-machine 3999 2))) ;; [3999, 4000]
    (t/is (thrown? AssertionError (sut/get-memory sut/new-machine 3000 1001))) ;; [3000, 4000]
    (t/is (sut/get-memory sut/new-machine 3000 1000)) ;; OK, [3000, 3999]
    )
  (t/testing "returns the memory"
    (let [d (d/new-data :minus [1 2 3 4 5])
          m (sut/set-memory sut/new-machine 10 d)]
      (t/is (= d (sut/get-memory m 10))))))

(t/deftest set-memory-test
  (t/testing "throws if data is invalid"
    (doseq [bd ["hello" \space 100 3.14 [1 2 3] #{1 2 3} {:sign :blah :bytes ["hello"]}]]
      (t/is (thrown? AssertionError (sut/set-memory sut/new-machine 0 bd)))))
  (t/testing "throws if m is invalid memory location"
    (doseq [bm ["hello" 3.14 [1 2 3] :yada #{1 2 3} {:a 1 :b 2} -1 sut/mem-limit]]
      (t/is (thrown? AssertionError (sut/set-memory sut/new-machine bm (d/new-data 5))))))
  (t/testing "throws if not enough room to fit all data"
    (let [d (repeat 10 (d/new-data :minus [1 2 3 4 5]))]
      (t/is (thrown? AssertionError (sut/set-memory sut/new-machine (dec sut/mem-limit) d)))))
  (t/testing "data can be a single element"
    (t/is (= (d/new-data 5) (sut/get-memory sut/new-machine 0)))
    (let [d (d/new-data :minus [1 2 3 4 5])
          m (sut/set-memory sut/new-machine 0 d)]
      (t/is (= d (sut/get-memory m 0)))))
  (t/testing "data can be a sequence of elements"
    (t/is (= (repeat 10 (d/new-data 5))
             (sut/get-memory sut/new-machine 0 10)))
    (let [d (repeat 10 (d/new-data :minus [1 2 3 4 5]))
          m (sut/set-memory sut/new-machine 0 d)]
      (t/is (= d (sut/get-memory m 0 10)))))
  (t/testing "m can be in the middle"
    (t/is (= (d/new-data 5) (sut/get-memory sut/new-machine 10)))
    (let [d (d/new-data :minus [1 2 3 4 5])
          m (sut/set-memory sut/new-machine 10 d)
          df (diff sut/new-machine m)]
      (t/is (= d (sut/get-memory m 10)))
      (t/is (= {:memory (concat (repeat 10 nil)
                                [(d/new-data 5)])}
               (first df)))
      (t/is (= {:memory (concat (repeat 10 nil)
                                [d])}
               (second df))))
    (t/is (= (repeat 10 (d/new-data 5)) (sut/get-memory sut/new-machine 1000 10)))
    (let [d (repeat 10 (d/new-data :minus [1 2 3 4 5]))
          m (sut/set-memory sut/new-machine 100 d)
          df (diff sut/new-machine m)]
      (t/is (= d (sut/get-memory m 100 10)))
      (t/is (= {:memory (concat (repeat 100 nil)
                                (repeat 10 (d/new-data 5)))}
               (first df)))
      (t/is (= {:memory (concat (repeat 100 nil)
                                d)}
               (second df)))))
  (t/testing "resizes data to 5-byte words"
    (let [d (d/new-data :minus [1 2])
          m (sut/set-memory sut/new-machine 10 d)]
      (t/is (= (d/new-data :minus [0 0 0 1 2]) (sut/get-memory m 10))))
    (let [d (repeat 100 (d/new-data :minus [1 2]))
          m (sut/set-memory sut/new-machine 3000 d)]
      (t/is (= (repeat 100 (d/new-data :minus [0 0 0 1 2])) (sut/get-memory m 3000 100))))
    (let [d (repeat 100 (d/new-data :minus [1 2 3 4 5 6 7]))
          m (sut/set-memory sut/new-machine 3899 d)]
      (t/is (= (repeat 100 (d/new-data :minus [3 4 5 6 7])) (sut/get-memory m 3899 100))))))

;; (t/deftest get-overflow-test)

(t/deftest set-overflow-test
  (t/testing "throws if overflow is invalid"
    (doseq [bof ["hello" \space 100 3.14 [1 2 3]]]
      (t/is (thrown? AssertionError (sut/set-overflow sut/new-machine bof)))))
  (t/testing "returns new machine with updated overflow"
    (t/is (= false (sut/get-overflow sut/new-machine)))
    (let [m (sut/set-overflow sut/new-machine true)
          d (diff sut/new-machine m)]
      (t/is (= true (sut/get-overflow m)))
      (t/is (= {:overflow false} (first d)))
      (t/is (= {:overflow true} (second d)))
      (t/is (= (dissoc sut/new-machine :overflow) (last d))))))

;; (t/deftest get-condition-indicator-test)

(t/deftest set-condition-indicator-test
  (t/testing "throws if condition-indicator is invalid"
    (doseq [bci ["hello" \space 3.14 100 [1 2 3] #{:less}]]
      (t/is (thrown? AssertionError (sut/set-condition-indicator sut/new-machine bci)))))
  (t/testing "returns new machine with updated condition-indicator"
    (t/is (= :equal (sut/get-condition-indicator sut/new-machine))) ;; 100th assertion!
    (let [m (sut/set-condition-indicator sut/new-machine :greater)
          d (diff sut/new-machine m)]
      (t/is (= :greater (sut/get-condition-indicator m)))
      (t/is (= {:condition-indicator :equal} (first d)))
      (t/is (= {:condition-indicator :greater} (second d)))
      (t/is (= (dissoc sut/new-machine :condition-indicator) (last d))))))

;; (t/deftest get-program-counter-test)

(t/deftest set-program-counter-test
  (t/testing "throws if pc is invalid memory location"
    (doseq [bpc ["hello" 3.14 [1 2 3] :yada #{1 2 3} {:a 1 :b 2} -1 4000]]
      (t/is (thrown? AssertionError (sut/set-program-counter sut/new-machine bpc)))))
  (t/testing "returns new machine with updated program counter"
    (t/is (= 0 (sut/get-program-counter sut/new-machine)))
    (let [m (sut/set-program-counter sut/new-machine 100)
          d (diff sut/new-machine m)]
      (t/is (= 100 (sut/get-program-counter m)))
      (t/is (= {:program-counter 0} (first d)))
      (t/is (= {:program-counter 100} (second d)))
      (t/is (= (dissoc sut/new-machine :program-counter) (last d))))))

(t/deftest inc-program-counter-test
  (t/testing "returns new machine with program counter increased by 1"
    (t/is (= 0 (sut/get-program-counter sut/new-machine)))
    (let [m (sut/inc-program-counter sut/new-machine)
          d (diff sut/new-machine m)]
      (t/is (= 1 (sut/get-program-counter m)))
      (t/is (= {:program-counter 0} (first d)))
      (t/is (= {:program-counter 1} (second d)))
      (t/is (= (dissoc sut/new-machine :program-counter) (last d))))))

;; TODO - These are subject to change, so we'll leave them alone for now.
(t/deftest add-device-test)
(t/deftest lookup-device-test)
(t/deftest get-device-test)
(t/deftest update-device-test)
