(ns mix-machine.device
  (:require [mix-machine.data :as d]
            [mix-machine.debug :refer [Debuggable debug DefaultDebuggable]]
            [mix-machine.char :as ch]))

;; TODO - ready? isn't used at all


;; Type accepts input from the user
(defprotocol UserInput
  (read-console [_]))

;; Type accepts string as input
(defprotocol StringInput
  (read-str [_ s]))

;; Type is a MIX I/O device
(defprotocol IODevice
  (in [_])
  (out [_ data]))

;; Type is 'seekable' - as in you can move forward and backward through the type
;; (whatever 'move forward and backward' means for the implementing type...)
(defprotocol Seekable
  (seek [_ n]))

;; Type is 'indexable' - as in you can jump to any point in the type using an integer index.
;; (whatever 'jump to any point' means for the implementing type...)
(defprotocol Indexable
  (index [_ i]))

;; A MIX I/O device that reads/writes complete words (sign and 5 bytes)
(defrecord WordDevice [type dir size block ready?]
  IODevice
  (in [_]
    ;; NOTE - For now, we'll just write the whole block
    (when (not= :out dir)
      block))
  (out [_ b]
    (when (not= :in dir)
      ;; NOTE - For now, we'll just read the whole block
      (assert (= size (count b)) (count b))
      (assoc _ :block (vec b)))))

;; A MIX I/O device that reads/writes MIX characters
;; CharDevices can also accept input from strings and stdin.
;; NOTE - Input from string or stdin is primarily for debugging.
;;        Although, with a UI in front, we'll need some way to interact
;;        with the devices like typewriter, so we'll probably keep StringInput impl.
(defrecord CharDevice [type dir size block ready?]
  IODevice
  (in [_]
    (when (not= :out dir)
      ;; NOTE - For now, we'll just write the whole block
      ;; Convert to words, with a plus sign.
      (vec (map d/chars->data block))))
  (out [_ b]
    (when (not= :in dir)
      ;; NOTE - For now, we'll just read the whole block
      (assert (= size (count b)))
      (assoc _ :block (vec (map d/data->chars b)))))
  UserInput
  (read-console [_]
    (read-str _ (read-line)))
  StringInput
  (read-str [_ s]
    (let [chs (map str (seq s))
          chs-read (count chs)
          rem (- (* size 5) chs-read)
          input (partition 5 (concat chs (repeat rem " ")))]
      (assoc _ :block (mapv vec input)))))

;; Add debugging

(extend WordDevice
  Debuggable
  DefaultDebuggable)

(extend CharDevice
  Debuggable
  DefaultDebuggable)

;; A MIX I/O device that can seek forward and backward (tapes)
(defrecord SeekableDevice [device size index make-block blocks]
  Seekable
  (seek [_ n]
    (cond
      (< n 0) (let [i (max 0 (- index n))
                    old-block (:block device)
                    new-block (get blocks n)]
                (-> _
                    (assoc-in [:device :block] (or new-block (make-block)))
                    (assoc-in [:blocks index] old-block)
                    (assoc :index i)))
      (= n 0) (let [old-block (:block device)
                    new-block (get blocks n)]
                (-> _
                    (assoc-in [:device :block] (or new-block (make-block)))
                    (assoc-in [:blocks index] old-block)
                    (assoc :index n)))
      (> n 0) (let [i (min (dec (count blocks)) (+ index n))
                    old-block (:block device)
                    new-block (get blocks n)]
                (println "i" i)
                (debug device)
                (-> _
                    (assoc-in [:device :block] (or new-block (make-block)))
                    (assoc-in [:blocks index] old-block)
                    (assoc :index i)))))
  IODevice
  (in [_]
    (in device))
  (out [_ b]
    (-> _
        (update :device out b)
        (seek 1))))

;; A MIX I/O device that can be indexed (drum / disk)
(defrecord IndexableDevice [device size index make-block blocks]
  Indexable
  (index [_ n]
    (assert (<= 0 n (dec size)))
    (let [old-block (:block device)
          new-block (get blocks n)]
      (-> _
          (assoc-in [:device :block] (or new-block (make-block)))
          (assoc-in [:blocks index] old-block)
          (assoc :index n))))
  IODevice
  (in [_]
    (in device))
  (out [_ b]
    (update _ :device out b)))

(extend SeekableDevice
  Debuggable
  DefaultDebuggable)

(extend IndexableDevice
  Debuggable
  DefaultDebuggable)

;; MIX devices
(def devices [{:type :tape :dir :both :mode :word :block-size 100}
              {:type :disk :dir :both :mode :word :block-size 100}
              {:type :card-reader :dir :in :mode :char :block-size 16}
              {:type :card-punch :dir :out :mode :char :block-size 16}
              {:type :line-printer :dir :out :mode :char :block-size 24}
              {:type :typewriter :dir :in :mode :char :block-size 14}
              {:type :paper-tape :dir :both :mode :char :block-size 14}])

(defn- extract-property
  [maps k p]
  (reduce (fn [m v]
            (assoc m (k v) (p v)))
          {}
          maps))

(def block-size (extract-property devices :type :block-size))
(def device-dir (extract-property devices :type :dir))
(def device-mode (extract-property devices :type :mode))

(def valid-device-type? (set (map :type devices)))

(defn- make-word-block
  [size]
  (vec (repeat size (d/new-data 5))))

(defn- make-char-block
  [size]
  (vec (repeat size (d/data->chars (d/new-data 5)))))

(defn- make-block
  [mode size]
  (case mode
    :word (make-word-block size)
    :char (make-char-block size)
    (throw (ex-info "make-block: Unknown mode" {:mode mode}))))

(defn- make-device
  [device-type]
  (assert (valid-device-type? device-type))
  (let [mode (device-mode device-type)
        dir (device-dir device-type)
        size (block-size device-type)
        block (make-block mode size)]
    (case mode
      :word (->WordDevice device-type dir size block true)
      :char (->CharDevice device-type dir size block true))))

(defn- make-seekable-device
  [device-type size]
  (let [device (make-device device-type)
        blocks (vec (repeat size nil))
        make-block #(:block device)]
    (->SeekableDevice device size 0 make-block blocks)))

(defn- make-indexable-device
  [device-type size]
  (let [device (make-device device-type)
        blocks (vec (repeat size nil))
        make-block #(:block device)]
    (->IndexableDevice device size 0 make-block blocks)))

(defn new-tape [size] (make-seekable-device :tape size))
(defn new-disk [size] (make-indexable-device :disk size))
(def new-card-reader (make-device :card-reader))
(def new-card-punch (make-device :card-punch))
(def new-line-printer (make-device :line-printer))
(def new-typewriter (make-device :typewriter))
(def new-paper-tape (make-device :paper-tape))

