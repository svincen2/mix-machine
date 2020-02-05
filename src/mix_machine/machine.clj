(ns mix-machine.machine
  (:require [mix-machine.data :as d]
            [mix-machine.device :as dev]))


;; TODO
;
;; Validate machine argument to these functions?
;; Or at least validate the parts of machine they need?
;; (i.e. set-register validates that machine has the paths [:registers :A],
;; [:registers :X], [:registers :I 1], etc...) - as it is now, we don't do that



;; Machine:
;; Registers:
;; - Accum (A) - Word
;; - Exten (X) - Word
;; - Index (I) - 6 registers (I1-I6), each a sign and 2 bytes
;; - Jump  (J) - 2 bytes, positive (i.e. fixed sign)

(def mem-limit 4000)
(def word 5)
(def half-word 2)
(def tape-ports 8)
(def disk-ports 8)

(def new-machine
  {:registers {:A (d/new-data word)
               :X (d/new-data word)
               :I [nil ;; NOTE: This is intentional (for indexing purposes).
                   (d/new-data half-word) ;; I1
                   (d/new-data half-word) ;; I2
                   (d/new-data half-word) ;; I3
                   (d/new-data half-word) ;; I4
                   (d/new-data half-word) ;; I5
                   (d/new-data half-word)];; I6
               :J (d/new-data half-word)}
   :overflow false
   :condition-indicator :equal
   :memory (vec (repeat mem-limit (d/new-data word)))
   ;; NOTE - You'll need to add the devices you want with add-device
   :devices {:tape (vec (repeat tape-ports nil))
             :disk (vec (repeat disk-ports nil))
             :card-reader nil
             :card-punch nil
             :line-printer nil
             :typewriter nil
             :paper-tape nil}
   ;; NOTE - NOT DEFINED BY MIX
   ;; For now, we'll just assume the program is loaded into memory 0,
   ;; in order (i.e. memory 0 contains first instruction, memory 1 contains the second, etc...)
   :program-counter 0
   })

(def valid-condition-indicator? #{:less :equal :greater})

(defn- get-index-register
    "Returns the index register i, if possible.
    i should be in range [1, 6].
    If i is not in range, default is returned if provided, otherwise nil is returned."
  ([machine r]
   (get-index-register r nil))
  ([machine r default]
   ;; NOTE - (or) here because we want default for r = [:I 0]
   ;; If we fed default into get-in, [:I 0] would return nil.
   (or (get-in machine (cons :registers r))
       default)))

(defn get-register
  "Returns the register r, if possible.
  r should be one of :A, :X, :J or an index register.
  An index register takes the form [:I i], where i is an integer in range [1, 6]
  (i.e. [:I 1] would return register I1, [:I 2] would return I2, etc).
  The second form returns default if r is not a known register."
  ([machine r]
   (get-register machine r nil))
  ([machine r default]
   (cond
     (coll? r) (get-index-register machine r default)
     ;; TODO - We don't need to use (or) here
     :else (or (get-in machine [:registers r])
               default))))

(defn- set-index-register
  "Sets index register r to data, provided r is of the form [:I i],
  where i in range [1, 6].  If r is not in this form, an exception is thrown.
  data is resized to 2-bytes if necessary to fit the index register data size.
  A new machine with the register set correctly is returned."
  [machine r data]
  (assert (and (= 2 (count r))
               (= :I (first r))
               (<= 1 (second r) 6))
          (str "set-index-register: Unknown register " r))
  (let [[k i] r]
    (assoc-in machine [:registers :I i] (d/resize data half-word))))

(defn- set-jump-register
  "Sets the jump register to data.
  data is resized to 2-bytes to fit the register.
  The sign is ignored (jump register is always :plus).
  A new machine with the register set correctly is returned."
  [machine data]
  (assoc-in machine [:registers :J] (-> data
                                        (d/set-sign :plus)
                                        (d/resize half-word))))

(defmacro assert-valid-register
  [reg]
  `(assert (or (#{:A :X :J} ~reg)
               (sequential? ~reg))
           (str "Unknown register " ~reg)))

(defn set-register
  "Sets register r to data, resizing if necessary to fit the data size of the register.
  Returns a new machine with register r set to (resize data (count-bytes r)).
  Throws if data is invalid MIX data (as defined in data namespace).
  Does not set the sign of the jump register, as it is defined to always be :plus."
  [machine r data]
  (d/assert-valid-data data)
  (assert-valid-register r)
  (cond
    (sequential? r) (set-index-register machine r data)
    (= :J r) (set-jump-register machine data)
    (#{:A :X} r) (assoc-in machine [:registers r] (d/resize data word))))

(defn inc-register
  "Increase register r by adding data.
  Throws if r is an unknown register.
  Throws if data is invalid."
  [machine r data]
  (assert-valid-register r)
  (d/assert-valid-data data)
  (let [contents-R (get-register machine r)]
    (set-register machine r (d/add contents-R data))))

(defmacro assert-valid-memory
  [m]
  `(assert (and (int? ~m)
                (< -1 ~m ~mem-limit))
           (str "Invalid memory location " ~m)))

(defn get-memory
  "Returns the contents of memory.
  The first form returns the contents of a single memory index.
  The second form returns a range of memory, starting at m, containing size elements.
  Throws if m is an invalid memory location.
  Throws if size is not a positive integer.
  THrows if [m, m + size] is an invalid memory location range.
  (i.e. (get-memory 3999 10) yields the invalid range [3999, 4009])"
  ([machine m]
   (assert-valid-memory m)
   (get-in machine [:memory m]))
  ([machine m size]
   (assert-valid-memory m)
   (assert (pos-int? size) (str "Expected positive size, found " size))
   (assert (<= (+ m size) mem-limit) (format "Memory range invalid [%d, %d)" m (+ m size)))
   (subvec (:memory machine) m (+ m size))))

(defn set-memory
  "Sets machine's memory to data, starting at m.
  data can be a single data element, or a sequence of data elements.
  The first (and possibly only) element is stored in memory at m,
  the second at m+1, etc...
  Throws if m is an invalid memory location.
  Throws if data is invalid MIX data.
  Throws if there is not enough room to fit the data
  (e.g. trying to store 2 items starting at 3999)."
  [machine m data]
  (assert-valid-memory m)
  (if (sequential? data)
    (do
      (assert (< (+ m (count data)) mem-limit)
              (str "Unable to store " (count data) " elements starting at " m))
      (let [sized-data (map #(d/resize % word) data)
            data-len (count data)
            mem (:memory machine)
            front (take m mem)
            back (drop (+ m data-len) mem)
            mem2 (take mem-limit (concat front sized-data back))]
        (assoc machine :memory (vec mem2))))
    (assoc-in machine [:memory m] (d/resize data word))))

(defn get-overflow
  [machine]
  (get machine :overflow))

(defn set-overflow
  "Sets the overflow of machine to of.
  Throws if of is an invalid overflow (i.e. (not (boolean? of)) ).
  Returns a new machine with the updated overflow."
  [machine of]
  (assert (boolean? of) (str "Expected boolean overflow, found " of))
  (assoc machine :overflow of))

(defn get-condition-indicator
  [machine]
  (get machine :condition-indicator))

(defn set-condition-indicator
  "Sets the condition indicator of machine to ci.
  Throws if ci is not a valid condition indicator (i.e. (not (valid-condition-indicator? ci)) ).
  Returns a new machine with the updated condition indicator."
  [machine ci]
  (assert (valid-condition-indicator? ci))
  (assoc machine :condition-indicator ci))

(defn get-program-counter
  [machine]
  (get machine :program-counter))

(defn set-program-counter
  [machine pc]
  (assert-valid-memory pc)
  (assoc machine :program-counter pc))

;; TODO - What about when PC = 3999? Wrap around to 0?
(defn inc-program-counter
  "Increment machine's program counter by 1.
  Returns a new machine with the updated program counter."
  [machine]
  (update machine :program-counter inc))

;; TODO - UNTESTED
;; There will likely be changes to devices after MIXAL is implemented, and
;; we revisit how programs are ran.
(defn add-device
  [machine loc device]
  (assoc-in machine (cons :devices loc) device))

(defn- lookup-device
  "Find the machine location of the device identified by device-code."
  [device-code]
  (cond
    (<= 0 device-code 7) [:tape device-code]
    (<= 8 device-code 15) [:disk (- device-code 8)]
    (= 16 device-code) [:card-reader]
    (= 17 device-code) [:card-punch]
    (= 18 device-code) [:line-printer]
    (= 19 device-code) [:typewriter]
    (= 20 device-code) [:paper-tape]
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
