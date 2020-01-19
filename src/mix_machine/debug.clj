(ns mix-machine.debug
  (:require [clojure.pprint :refer [pprint]]))

(defprotocol Debuggable
  (debug [_]))

(def DefaultDebuggable {:debug #(do (println (type %))
                                    (pprint %))})
