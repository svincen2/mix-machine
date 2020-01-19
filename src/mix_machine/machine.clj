(ns mix-machine.machine
  (:require [mix-machine.data :as d]
            [mix-machine.device :as dev]))


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
   ;; NOTE - You'll need to add the devices you want with add-device
   :devices {:tape (vec (repeat 8 nil))
             :disk (vec (repeat 8 nil))
             :card-reader nil
             :card-punch nil
             :line-printer nil
             :typewriter nil
             :paper-tape nil}
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
  ([machine m]
   (get-in machine [:memory m]))
  ([machine m size]
   (subvec (:memory machine) m (+ m size))))

(defn set-memory
  [machine m data]
  (assert (<= 0 m 3999))
  (if (sequential? data)
    (let [sized-data (map #(d/set-size % 5) data)
          data-len (count data)
          mem (:memory machine)
          front (take m mem)
          back (drop (+ m data-len) mem)
          mem2 (take 4000 (concat front sized-data back))]
      (assoc machine :memory (vec mem2)))
    (assoc-in machine [:memory m] (d/set-size data 5))))

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

(defn add-device
  [machine loc device]
  (assoc-in machine (cons :devices loc) device))

(defn- lookup-device
  "Find the machine location of the device identified by device-code."
  [device-code]
  (cond
    (<= 0 device-code 7) [:tape device-code]
    (<= 8 device-code 15) [:disk (- device-code 8)]
    (= 16 device-code) :card-reader
    (= 17 device-code) :card-punch
    (= 18 device-code) :line-printer
    (= 19 device-code) :typewriter
    (= 20 device-code) :paper-tape
    :else (throw (ex-info "lookup-device: Unknown device code" {:code device-code}))))

(defn get-device
  "Return the device identified by device.
  device can be either a device code, or a device machine location."
  [machine code-or-loc]
  (let [device-loc (if (int? code-or-loc) (lookup-device code-or-loc) code-or-loc)]
    (get-in machine (cons :devices device-loc))))

(defn update-device
  "Update device identified by device-code,
  by calling f with the device as the first argument, followed by args.
  This is equivalent to (apply f device args), but returns a machine
  with the device update in-place."
  [machine device-code f & args]
  (let [device-loc (lookup-device device-code)]
    (apply update-in machine (cons :devices device-loc) f args)))
