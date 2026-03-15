(ns iris-layout.css
  (:require [clojure.java.io :as io]))

(defmacro inline-css []
  (slurp (io/resource "iris_layout/css/styles.css")))
