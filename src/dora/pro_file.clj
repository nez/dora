(ns dora.pro-file
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.java.shell :refer :all]
            [clojure.repl :refer :all]
            [clojure.set :refer :all]
            [clojure.string :as s]
            [digitalize.core :refer :all]
            [mongerr.core :refer :all]
            [dora.data :refer :all]
            [dora.p.adela :refer :all]
            [dora.p.calificacion :refer :all]
            [dora.p.agente-web :refer :all]
            [dora.util :refer :all]
            [formaterr.core :refer :all]
            [nillib.text :refer :all]
            [environ.core :refer [env]]
            [ring.util.codec :as c]))

(defn shsh
  "Execute command in shell"
  [& command]
  (let [result (sh "/bin/sh" "-c" (s/join " " command))]
    (str (:err result) (:out result))))

(defmacro try-catch [body]
  `(try
     ~body
     (catch Exception e# (str "error " e#))))

(defn nils-csv-validation
  "Are there elements in a rel with too many nils?"
  [rel]
  (let [total (count (first rel))
        totals (map #(count (remove-nils (vals %))) rel)
        mini (apply min totals)]
    (if (> total 2)
      (if (> 3 mini)
        true
        false)
      false)))

(defn number-weird-format?
  "Does the number have a weird format
  such as '$' 'Kg' or anything that is not a number?"
  [s]
  (if-let [num (first (re-find number-regex s))]
    (not= s num)
    false))

(defn has-weird-format-numbers?
  [rel]
  (or* (map number-weird-format? (vals (first rel)))))

(defn rel-sample
  "take some random elements from the rel"
  ([rel]
    (rel-sample 20 rel))
  ([n rel]
    (take n (shuffle rel))))

(defn has-numbers?
  "The string has numbers in it?"
  [s]
  (if (re-find number-regex s)
      true
      false))

(defn vec-has-mixed-formats
  "Does a vector have a mix of numbers and strings?"
  [v]
  (apply not= (remove-nils (map has-numbers? v))))

(defn has-mixed-formats
  "Does the rel contain a mix of numbers and strings in some field?"
  [rel]
  (let [rel (rel-sample rel)
        ks (keys (first rel))]
    (or* (map vec-has-mixed-formats
              (map (fn [k] (map k rel))
                   ks)))))

(def metas
  "Vector of 'validations' or scripts whose output we collect"
  ["head -n 1" ;first line
   "file"      ;file type, encoding
   "wc -l"     ;line count
   "du -h"     ;file size
   "validaciones/IDMX/code/prep_proc"
   "validaciones/repetidos" ;conteo de lineas repetidas
   ])

(def csv-validations
  "Vector of validations aplicable only to rels generated from CSV"
  [{:name "nils-csv-validation" :fn nils-csv-validation}
   {:name "has-weird-format-numbers" :fn has-weird-format-numbers?}
   {:name "has-mixed-formats" :fn has-mixed-formats}])

(defn eval-csv-validation
  "Instead of just running the function, handle its exceptions"
  [f rel]
  (try (f rel)
       (catch Exception e "error")))

(defn csv-engine
  "Run validations specific to CSV"
  [file]
  (try
    (let [rel (take 1000 (csv file))]          ;just a 1000 rows sample for performance
      (zipmap (map :name csv-validations)
              (map #(eval-csv-validation % rel)
                   (map :fn csv-validations))))
    (catch Exception e {:csv-engine-error (str e)})))

(comment(defn dora-insert
          "Insert a newly read url into db"
          [url]
          (db-insert :dora {:url url :active true})))

(defn download-if-url
  "If name is a url, download and return relative path.
  nil if any error"
  [url]
  (try
    (if (re-find url-regex url)
      (let [file-name (str "/tmp/" (rand))
            tmp (spit file-name (clean-get url))]
        file-name)
      url)
    (catch Exception e)))

(defn execute-validations
  "Execution engine ;)"
  [file-name url]
  (remove-nils (merge (zipmap metas
                              (pmap #(try                   ;(if (string? %)
                                       (shsh % file-name)   ;    (% data))
                                       (catch Exception e (str e)))
                                    metas))
                      (if (re-find #"csv|CSV" url)
                        (csv-engine file-name)))))

(defn trim-macugly-line
  "Take the first line whith line breaks as \r
  whch are generated on old mac systems"
  [s]
  (re-find #"[^\r]+" s))

(defn trimming
  "Trim strings for head -n 1"
  [s]
  (s/join (take 500 (trim-macugly-line s))))

(defn first-numbers
  "Take only the first numeric substring"
  [s]
  (re-find #"[0-9]+" s))

(defn sfirst
  "second first"
  [coll]
  (second (first coll)))

(defn update-all-in
  "update-in with each key and fn pair"
  [m updates]
  (loop [m m
         updates updates]
    (if (empty? updates)
      m
      (recur (update-in m [(ffirst updates)] (sfirst updates))
             (rest updates)))))

(defn trim-file-script
  [s]
  (second (re-seq #"[^;]+" s)))

(defn process-validations
  "Post process validations, clean them and shit"
  [m]
  (update-all-in m {"head -n 1" trimming
                    "wc -l" first-numbers
                    "du -h" first-numbers
                    "validaciones/repetidos" first-numbers
                    "file" trim-file-script
                    }))

(defn to-validate []
  (difference (file-names) (set (map :id (db :resource-metadata)))))

(defn validate-dgm
  ([names]
   (doall (pmap #(db-update :resource-metadata
                            {:id %}
                            (assoc (process-validations (execute-validations (data-directory %) "csv"))
                                   :id %))
                names)))
  ([]
   (validate-dgm (to-validate))))

(defn validate-dgm-batched
  ([]
   (validate-dgm-batched (to-validate)))
  ([names] (loop [names names]
             (if (not (empty? names))
               (validate-dgm (take 100 names))
               (recur (drop 100 names))))))

(defn broken-link-recommendation
  "If the URL was reported as broken today, raise this alert"
  [url todays-broken]
  (if (not (empty? (filter #(= url %) (map :url todays-broken))))
    {:name "La URL no pudo ser leída por el robot"
     :description "Esto puede significar que la URL está caída, cargue demasiado lento, o no sea accesible para robots."
     :more-info "http://datos.gob.mx/guia/publica/paso-2-1.html"
     :clave "d01"
     :categoria "disponibilidad"}))

(def acento-regex #"[áéíóúÁÉÍÓÚñÑ]")

(defn acento-recommendation
  "Si la string tiene acentos, emite una recomendacion de no usar acentos en la URL"
  [s]
  (if (re-find acento-regex s)
    {:name "La URL contiene acentos"
     :description "Es recomendable que las urls no contengan acentos."
     :clave "i11"
     :categoria "interoperabilidad"}))

(defn encoding-recommendation
  "Check file encoding against a list of blacklisted encodings"
  [metadata]
  (if (re-find #"8859" (metadata "file"))
    {:name "El archivo está en una codificación no universal"
     :description "Es recomendable utilizar la codificación UTF-8"
     :clave "i12"
     :categoria "interoperabilidad"}))

(defn duplicated-url-recommendation
  "Checks if there are more than one resource with this url"
  [url]
  (if (> (count (re-find :resources {:url url})) 1)
    {:name "Hay mas de un recurso de datos con esta URL"
     :description "No es necesario que los mismos datos estén dados de alta mas de una vez. Revisar otras áreas, o dependencias que tengan estos datos publicados."
     :clave "c42"
     :categoria "calidad"}))

(defn resource-description-recommendation
  "Checks if the resource has no description"
  [resource]
  (if (empty? (:description resource))
    {:name "El recurso no tiene descripción"
     :description "Es necesario agregar una descripción al recurso, para que sea fácil entender la finalidad del recurso"
     :clave "u22"
     :categoria "documentacion"}))

(defn has-mixed-formats-recommendation
  [m]
  (if (seq (filter #(and (re-find #"has_mixed_formats" (:meta %))
                         (true? (:data %)))
                   m))
    {:name "El archivo contiene columnas en las que algunos elementos tienen números y otros no"
     :description "Evitar tener valores de diferentes tipos (como texto y número) para una columna en diferentes registros o filas."
     :clave "c41"
     :categoria "calidad"}))

(defn nils-csv-validation-recommendation
  [m]
  (if (seq (filter #(and (re-find #"nils-csv-validation" (:meta %))
                         (true? (:data %)))
                   m))
    {:name "El archivo contiene filas vacías"
     :description "Eliminar filas y columnas vacías, al igual que cálculos adicionales en formatos tabulares, p. ej. una celda “Total” con la suma de una de las columnas."
     :clave "m31"
     :categoria "legibilidad"}))

(defn has-weird-format-numbers?-recommendation
  [m]
  (if (seq (filter #(and (re-find #"has-weird-format-numbers?" (:meta %))
                         (true? (:data %)))
                   m))
    {:name "Hay campos numéricos que contienen caracteres no numéricos"
     :description "Los campos numéricos, incluyendo los monetarios y magnitudes, deben permanecer en un formato numérico de tipo entero o flotante. Es decir, evitar agregar símbolos monetarios o de unidades de medición, p. ej. “200 m2” o “£20”, haciendo explícitas las unidades en el nombre de la columna, o en una segunda columna. P. ej. “Monto en MXN”, “Distancia en KM”."
     :clave "m33"
     :categoria "legibilidad"}))

(comment
  (defn -recommendation
    [m]
    (if (seq (filter #(and (re-find #"" (:meta %))
                           (true? (:data %)))
                     m))
      {:name ""
       :description ""
       :clave ""})))

(defn recommendations
  "Generate recommendations from a file url and its metadata"
  [url metadata resource todays-broken]
  (remove-nils (flatten [(try-catch (broken-link-recommendation url todays-broken))
                         (try-catch (acento-recommendation url))
                         (try-catch (encoding-recommendation metadata))
                         (try-catch (duplicated-url-recommendation url))
                         (try-catch (resource-description-recommendation resource))
                         (if (and (not (nil? url)) (re-find #"csv|CSV" url))
                           [(try-catch (has-mixed-formats-recommendation metadata))
                            (try-catch (nils-csv-validation-recommendation metadata))
                            (try-catch (has-weird-format-numbers?-recommendation metadata))])])))

(def resources (db :resources))

(defn find-resource-by-url [url]
  (first (filter #(= url (:url %))
                 resources)))

(defn resource-metadata
  "Find stored metadata for a id"
  [id]
  (db-findf :resource-metadata {:id id}))

(def datasets (db :datasets))

(defn find-resources-dataset
  "Find stored dataset metadata on a resource"
  [resource]
  (first (filter #(= (:id %)
                     (:dataset_id resource))
                 datasets)))

(defn dataset
  "Given a Resource or dataset name, return its dataset metadata"
  [o]
  (if (map? o)
    (if-let [dataset-id (:dataset_id o)] (db-findf :datasets {:id dataset-id}))
    (db-findf :datasets {:name o})))

(defn analytics
  "get the stored google analytics history"
  ([url]
   (try (:value (db-findf :google_analytics {:url url}))
        (catch java.lang.NullPointerException e)))
  ([url analytics-data]
   (if-not (empty? url)
     (apply + (map :value (filter #(try (re-find (re-pattern url) (:url %))
                                        (catch Exception e (= url (:url %))))
                                  analytics-data))))))

(defn inventory-resources
  []
  (mapcat :distribution (mapcat :dataset (db-find :adela-inventories))))

(defn flatten-inventory [i]
  (map #(hash-map :dataset %
                  :inventory (dissoc i :dataset))
       (:dataset i)))

(defn flatten-inventory-dataset
  [d]
  (map #(hash-map :dataset (dissoc (:dataset d) :distribution)
                  :inventory (:inventory d)
                  :resource %)
       (:distribution (:dataset d))))

(def ^:dynamic *adela-data-collection* (or (env :adela-data-collection) :adela-inventories))
;;:adela-catalogs ;:adela-inventories;; TODO: cuando entre en vigor el nuevo adela, cambiar a: :adela-inventories

(defn inventory-resources-denormalized
  []
  (mapcat flatten-inventory-dataset
          (mapcat flatten-inventory
                  (db *adela-data-collection*))))

(defn ieda?
  [url]
  (if (and (not (nil? url))
           (db-findf :ieda {:url url}))
    true
    false))

(defn dora-view-inventory    ;will expect entries from inventory-resources-denormalized
  "hace la fusion de datos de los recursos de adela"
  ([]
   (let [analytics  (db :google_analytics)
         analytics-views (db :google_analytics_views)
         broken (broken-today)]
     (map #(dora-view-inventory % analytics analytics-views broken)
          (inventory-resources-denormalized))))
  ([result analytics-data analytics-data-views todays-broken]
   (try (let [url (:downloadURL (:resource result))
              resource (find-resource-by-url url)
              dataset (find-resources-dataset resource)
              metadata (resource-metadata (:id resource))
              recommendations (remove string?
                                      (recommendations url metadata (:resource result) todays-broken))]
          (assoc {:adela result}
                 :ckan {:resource resource
                        :dataset (dissoc dataset :resources)}
                 :analytics {:downloads {:total (analytics url analytics-data)}
                             ;:pageviews {:total (analytics (:id resource) analytics-data-views)}
                             }
                 :file-metadata metadata
                 :recommendations recommendations
                 :id (str (:id (:dataset result)))
                 ;:ieda (ieda? url)
                 :calificacion (calificacion result recommendations)))
        (catch Exception e (println "Exception: " e)))))

(defn orphan-resources [inventories]
  (let [inv-urls (map #(:downloadURL (:resource (:adela %))) inventories)]
    (filter #(empty? (filter (fn [url] (= url (% :url)))
                             inv-urls))
            (db :resources))))

(defn dora-view-resources
  "Hace la fusion de datos para los recursos huerfanos, osea los que estan en ckan pero no en adela"
  ([inventories]
   (let [analytics  (db :google_analytics)
         analytics-views (db :google_analytics_views)
         broken (broken-today)]
     (map #(dora-view-resources %
                                analytics
                                analytics-views
                                broken)
          (orphan-resources inventories))))
  ([resource analytics-data analytics-data-views todays-broken]
   (try (let [url (:url resource)
              dataset (find-resources-dataset resource)
              metadata (resource-metadata (:id resource))
              recommendations (remove string?
                                      (recommendations url metadata resource todays-broken))]
          {:ckan {:resource resource
                  :dataset (dissoc dataset :resources)}
           :analytics {:downloads {:total (analytics url analytics-data)}
                      ;:pageviews {:total (analytics (:id resource) analytics-data-views)}
                       }
           :file-metadata metadata
           :recommendations recommendations})
        (catch Exception e (println "Exception: " e)))))

(defn data-fusion []
  (let [inventories (dora-view-inventory)]
    (concat inventories (dora-view-resources inventories))))

(defn data-fusion-analytics& []
  (let [data (db :data-fusion)]
    (println "resources table: " (count (db :resources)))
    (println "adela inventories table: " (count (db :adela-inventories)))
    (println "whole data-fusion: " (count data))
    (println "data-fusion ckan: " (count (filter #(not (empty? (:resource (:ckan %)))) data)))
    (println "data-fusion adela: " (count (filter :adela data)))
    (println "data-fusion ckan & adela: " (count (filter :adela (filter #(not (empty? (:resource (:ckan %)))) data))))))
