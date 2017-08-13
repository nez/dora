(ns dora.p.buda
  (:use [cemerick.url :refer :all]
        [formaterr.core :refer :all])
  (:require [clojure.string :as str]))

(defn buda-api [dataset & args]
  (let [laurl (url "http://api.datos.gob.mx/v1/" dataset)
        first-result (json (slurp (str (if args
                                         (apply assoc laurl args)
                                         laurl))))]
    ))

(defn legible-dataset [m]
  (dissoc (assoc m :tags (str/join "," (map :display-name (:tags m)))
                 :resources (str/join "," (map :name (:resources m)))
                 :organization (:name (:organization m)))
          :extras))
