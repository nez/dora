(ns dora.p.data-core
  "Copy and update the collections that are used in data fusion"
  (:require [chime :refer [chime-at chime-ch]]
            [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [clojure.core.async :as a :refer [<! go-loop]]
            [clojure.set :refer :all]
            [clojure.string :as s]
            [digitalize.core :refer :all]
            ;[dgm-analytics.core :refer :all]
            [dora.data :refer :all]
            [dora.p.adela :refer :all]
            [dora.p.agente-web :refer :all]
            [dora.p.ckan :refer :all]
            [dora.p.zendesk :refer :all]
            [dora.pro-file :refer :all]
            [monger.operators :refer :all]
            [mongerr.core :refer :all]
            [nillib.formats :refer :all])
  (:import [org.joda.time DateTimeConstants DateTimeZone]))

(defn dc-add-query
  [campo value-fn]
  (db-update :data-core
             {:campo campo}
             {:query value-fn}))

(defn dc-update
  ([campo value-fn]
   (try
     (db-update :data-core
                {:campo campo}
                {:value  (if (fn? value-fn)
                           (value-fn)
                           (eval (read-string value-fn)))})
     (catch Exception e (println "caught-exception: dc-update ->>"))))
  ([e]
   (dc-update (:campo e) (:query e)))
  ([]
   (map dc-update  (remove #(nil? (:query %)) (db-find :data-core)))))

(def drive-files {:instituciones "https://docs.google.com/feeds/download/spreadsheets/Export?key=1swzmgetabUT25eog-g6pdgRlc8x9uqz3iCNoruhdnxE&exportFormat=csv&gid=2050308732"
                  :ipda "https://docs.google.com/feeds/download/spreadsheets/Export?key=1swzmgetabUT25eog-g6pdgRlc8x9uqz3iCNoruhdnxE&exportFormat=csv&gid=1077082165"})

(defn update-db [coll f]
  (try
    (let [data (remove-nils (if (fn? f) (f) f))]
      (db-delete coll)
      (db-insert coll data))
    (catch Exception e (println "Error updating: " coll "\n\n" e))))

;; from http://stackoverflow.com/questions/1217131/recursive-doall-in-clojure
(defn doall-recur [s]
  (if (seq? s)
    (doall (map doall-recur
                s))
    s))

(defn api-catalog
  "Store the collections names in `api-catalog`"
  []
  (update-db :api-catalog
             (map #(hash-map :endpoint %)
                  (db))))

(defn flatten-adela-catalogs []
  "genera las apis adela-datasets y adela-resources"
  (let [datasets (flatten (map (fn [catalogo] (map #(assoc % :slug (:slug catalogo))
                                                  (:dataset catalogo)))
                               (db :adela-catalogs)))
        resources (flatten (map (fn [dataset] (map #(merge (dissoc dataset :distribution)
                                                          %)
                                                  (dataset :distribution)))
                                datasets))]
    [(update-db :adela-datasets datasets)
     (println "datasets in adela: " (count datasets))
     (update-db :adela-resources resources)
     (println "resources in adela: " (count (db :adela-resources)))]))

(defn update-adela []
  (doall-recur [(println "updating catalogs")

                (update-db :adela-catalogs adela-catalogs)
                                        ;(update-db :adela-plans adela-plans)
                (println "updating organizations")
                (update-db :adela-organizations organizations-req)
                (println "updating inventories")
                (update-db :adela-inventories adela-inventory)
                (println "flattening catalogs")
                (flatten-adela-catalogs)
                ]))

(defn fusion [] (update-db :data-fusion data-fusion))

(defn data-core []
  (doall-recur
   [(println "updating ckan data to api")
    (update-all-ckan)
    (println "ckan data updated")
    ;(update-db :instituciones instituciones)
    (println "updating zendesk")
    (update-db :zendesk-tickets all-tickets)
    (update-db :zendesk-organizations all-organizations)
    (update-db :zendesk-satisfaction all-satisfaction)
    (update-db :zendesk-users all-users)
    (flatten-adela-catalogs)
    (update-adela)
                                        ;(update-db :google_analytics download-data)
    (println "cleaning up old files")
    (mv-old-file)               ;(get-status-1)
    (println "running save-broken-links")
    (save-broken-links)
    ;(validate-dgm)
    (println "updating data-fusion")
    (fusion)
    (dc-update)]))

(defn data-core-lite
  "update ckan and adela data"
  []
  (doall-recur
   [(update-all-ckan)
    (update-adela)
    (dc-update)]))

(defn metrics []
  (doall-recur
   [(update-db :ckan-organizations (map #(hash-map :name %) (ckan-organizations)))]))

(defn today-at
  ([] (today-at 0 0 0 0))
  ([h] (today-at h 0 0 0))
  ([h m] (today-at h m 0 0))
  ([h m s] (today-at h m s 0))
  ([h m s mil]
   (.. (t/now)
       (withZone (DateTimeZone/forID "America/Mexico_City"))
       (withTime h m s mil))))

(defn schedule
  [time lapse f]
  (chime-at (periodic-seq (apply today-at time)
                          lapse)
            (fn [time]
              (f)
              (println "At: " time))
            {:error-handler (fn [e] (println "at: " (t/now) ", Error: " e))}))

(defn daily-schedule
  ([f]
   (daily-schedule [] f))
  ([time f]
   (schedule time (t/days 1) f)))

(defn schedule-data-core
  "Run data core everyday at 12am"
  []
  (daily-schedule data-core))

(defn metricas
  "Despliega las Métricas de Data Core"
  []
  (map #(vector (:value %) (:campo %))
       (db-find :data-core {:value {$exists true}})))
