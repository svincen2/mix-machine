(ns mix-machine.machine
  (:require [mix-machine.data :as d]))


;; Machine:
;; Registers:
;; - Accum (A) - Word
;; - Exten (X) - Word
;; - Index (I) - 6 registers (I1-I6), each a sign and 2 bytes
;; - Jump  (J) - 2 bytes, positive (i.e. fixed sign)

(def new-machine
  {:registers {:A (d/new-data 5)
               :X (d/new-data 5)
               :I [nil ;; NOTE: This is intentional (for indexing purposes).
                   (d/new-data 2) ;; I1
                   (d/new-data 2) ;; I2
                   (d/new-data 2) ;; I3
                   (d/new-data 2) ;; I4
                   (d/new-data 2) ;; I5
                   (d/new-data 2)];; I6
               :J (d/new-data 2)}
   :overflow false
   :condition-indicator :equal
   :memory (vec (repeat 4000 (d/new-data 5)))
   ;; NOTE - For now, we'll just assume the program is loaded into memory 0,
   ;; in order (i.e. memory 0 contains first instruction, memory 1 contains the second, etc...)
   :program-counter 0
   })

(def valid-condition-indicator? #{:less :equal :greater})

(defn- get-index-register
  "Returns the index register i, if possible.
  i should be in range [1, 6].
  If i is not in range, default is returned if provided, otherwise nil is returned."
  ([machine i]
   (get-index-register machine i nil))
  ([machine i default]
   (or (get-in machine [:registers :I i])
       default)))

(defn get-register
  ([machine r]
   (get-register machine r nil))
  ([machine r default]
   (cond
     (coll? r) (let [[_ i] r] (get-index-register machine i default))
     :else (or (get-in machine [:registers r])
               default))))

(defn- set-index-register
  [machine i data]
  (if (<= 1 i 6)
    (assoc-in machine [:registers :I i] (d/set-size data 2))
    (throw (ex-info "set-index-register: Unknown register" {:i i :data data}))))

(defn- set-jump-register
  [machine data]
  (assoc-in machine [:registers :J] (-> data
                                        (d/set-sign :plus)
                                        (d/set-size 2))))

(defn set-register
  [machine r data]
  (cond
    (coll? r) (let [[_ i] r] (set-index-register machine i data))
    (= :J r) (set-jump-register machine data)
    (#{:A :X} r) (assoc-in machine [:registers r] (d/set-size data 5))
    :else (throw (ex-info "set-register: Unknown register" {:r r :data data}))))

(defn inc-register
  [machine r data]
  (let [contents-R (get-register machine r)]
    (set-register machine r (d/add contents-R data))))

(defn get-memory
  [machine m]
  (get-in machine [:memory m]))

(defn set-memory
  [machine m data]
  (assert (<= 0 m 3999))
  (assoc-in machine [:memory m] (d/set-size data 5)))

(defn get-overflow
  [machine]
  (get machine :overflow))

(defn set-overflow
  [machine of]
  (assert (boolean? of))
  (assoc machine :overflow of))

(defn get-condition-indicator
  [machine]
  (get machine :condition-indicator))

(defn set-condition-indicator
  [machine ci]
  (assert (valid-condition-indicator? ci))
  (assoc machine :condition-indicator ci))

(defn get-program-counter
  [machine]
  (get machine :program-counter))

(defn set-program-counter
  [machine pc]
  (assert (int? pc))
  (assert (<= 0 pc 3999))
  (assoc machine :program-counter pc))

(defn inc-program-counter
  [machine]
  (update machine :program-counter inc))
