(ns dora.refineria
  (:require [clojure.java.shell :refer :all]
            [digitalize.core :refer :all]
            [dora.data :refer :all]
            [dora.p.ckan :refer :all]
            [dora.pro-file :refer :all]
            [dora.p.data-core :refer :all]
            [dora.util :refer :all]
            [environ.core :refer [env]]
            [formaterr.core :refer :all]
            [monger.core :as mg]
            [mongerr.core :refer :all]
            [tentacles.repos :refer [create-org-repo]]
            [clojure.string :as str]
            [clj-time.core :as t]))

(def mirrored-files
  {:proyectos-opa "http://www.transparenciapresupuestaria.gob.mx/work/models/PTP/OPA/datosabiertos/proyectos_opa.csv"
   :prog-avance-de-indicadores "http://www.transparenciapresupuestaria.gob.mx/work/models/PTP/DatosAbiertos/Calendarios_MIR_2008-2014/prog_avance_de_indicadores.csv"
   :prueba "http://prueba.com/archivo.csv"})

(defn apify-files []
  (doall
   (map #(try (update-db (first %) (csv (second %)))
              (catch Exception e (println "Exception on endpoint " (first %) ", file " (second %) "\n\n" e)))
        mirrored-files)))

;; Por ahora los recursos vip. despues sera (db :resources)
(defn mirrored-resources []
  (map resource ["http://web.coneval.gob.mx/Informes/Pobreza/Datos_abiertos/Rezago-social-2000-2005-2010_mun_DA.csv" "http://www.cofepris.gob.mx/Transparencia/Documents/datos_abiertos/Agua_Calidad.csv" "http://201.116.60.46/DatosAbiertos/Datos_de_calidad_del_agua_de_5000_sitios_de_monitoreo.zip" "http://www.siap.gob.mx/datosAbiertos/Estadist_Produc_Agricola/Agt_cierre_2013_2014.csv" "http://www.datos.economia.gob.mx/RegulacionMinera/ConcesionesMineras.csv" "http://dsiappsdev.semarnat.gob.mx/datos/aire/datos%20RETC.csv" "http://www.sectur.gob.mx/DATOS/0116/19-Inventario-Turistico-por-entidad-federativa.csv" "http://www.sectur.gob.mx/DATOS/0116/24-Localidades-que-cuentan-con-el-nombramiento-de-Pueblo-Magico.csv" "http://www.datatur.beta.sectur.gob.mx/Documentos%20compartidos/6_1_td.csv" "http://catalogo.datos.gob.mx/dataset/54ae9a90-418c-4088-88d0-edea59814826/resource/ffc1323a-bf46-4d9d-86a8-237315c2036e/download/matriculaporinstitucionyentidadfederativa.csv" "http://data.sct.gob.mx:8082/datos/datos/abiertos/11601MexicoConectado.xlsx" "http://www.correosdemexico.gob.mx/datosabiertos/cp/cpdescarga.txt" "http://www.correosdemexico.gob.mx/datosabiertos/poligonos/mapapoligonos.zip"]))

(defn mirrored-datasets []
  (mapcat #(:resources (dataset %))
          ["directorio-estadistico-nacional-de-unidades-economicas-denue-por-actividad-economica"
           "indicadores-urbanos"
           "censo-de-escuelas-maestros-y-alumnos-de-educacion-basica-y-especial"
           "proyecciones-de-la-poblacion-de-mexico"
           "regionalizacion-funcional-de-mexico"]))

(defn resource-urls [ds]
  (:resources (dataset ds)))

(def ^:dynamic refineria-dir "/Users/nex/mirrors/")
(def ^:dynamic gh-org "mxabierto-mirror")

(defn emap [& args]
  (doall (apply map args)))

(defn resource-name [resource]
  (str/join (take 100 (standard-name (:name resource)))))

(defn mirror-dir [resource]
  (let [eldir (str refineria-dir (resource-name resource))]
    (sh "mkdir" eldir)
    eldir))

(defn clone-mirror [resource]
  (let [le-name (resource-name resource)]
    (clone (str gh-org "/" le-name)
           (str refineria-dir le-name))))

(defn repo-mirror [resource]
  (let [le-name (resource-name resource)]
    (create-org-repo gh-org
                     le-name
                     {:auth (env :gh)
                      :description (:description resource)})))

(defn repo [resource]
  (try (clone-mirror resource)
       (catch Exception e
         (if (= (:status (ex-data e)) :non-existant)
           (do
             (repo-mirror resource)
             (clone-mirror resource))
           (do
             (checkout (mirror-dir resource) "master")
             (pull (mirror-dir resource) "origin" (branch)))))))

(defn resource-file [resource]
  (str (mirror-dir resource)
       "/"
       (last (re-seq #"[^/]+" (:url resource)))))

(defn copy-resource [resource]
  (copy (:url resource)
       (resource-file resource)))

(defn copy-resources [] (map copy-resource (db :resources)))
(defn refina-csv [file]
  (println "Digitalizando: " file)
  (csv file (digitalize (csv file))))

(defn refina-zip [dir file]
  (println "Descomprimiendo: " file)
  (shs "unzip" file "-d" dir)
  (shs "rm" file))

(defn re-filter [re coll]
  (filter #(re-find re %) coll))

(defn filter-files [dir regex]
  (re-filter regex (ls& dir)))

(defn uncompress [dir]
  (if-let [zips (seq (filter-files dir #"zip"))]
    (do (doall (map #(refina-zip dir %)
                    zips))
        (uncompress dir))))

(defn refina-tsv [file]
  (csv (str file ".csv")
       (digitalize (tsv file)))
  (rm file))

(defn refina-tsvs [files]
  (let [victims (filter #(re-find #"tsv" %) files)]
    (doall (map refina-tsv
                victims))))

(defn refina-psv [file]
  (csv (str file ".csv")
       (digitalize (psv file)))
  (rm file))

(defn refina-psvs [files]
  (let [victims (filter #(and (or (re-find #"txt" %)
                                  (re-find #"psv" %))
                              (> 15 (count (re-find #"|" (slurp %))))) files)]
    (doall (map refina-psv
                victims))))

(defn refina [dir]
  (println "refining: " (str/join ", " (ls dir)))
  (uncompress dir)
  (let [files (ls& dir)]
    (doall (map refina-csv
                (re-filter #"csv" files)))
    (refina-tsvs files)
    (refina-psvs files)))

(defmacro in-buda [& body]
  `(binding [*db* (:db (mg/connect-via-uri "mongodb://localhost:27027/buda"))]
     ~@body))

(defn ls-buda []
  (binding [*db* (:db (mg/connect-via-uri "mongodb://localhost:27027/buda"))]
      (db)))

(defn update-buda [resource-name file]
  (let [batches (partition-all 100 (csv file))]
    (binding [*db* (:db (mg/connect-via-uri "mongodb://localhost:27027/buda"))]
      (db-delete resource-name)
      (emap #(db-insert resource-name %) batches))))

(defn apify
  [resource]
  (let [files (filter-files (mirror-dir resource) #"csv")]
    (map #(update-buda (resource-name resource) %)
         files)))

(defn mirror
  ([] (doall (map mirror (mirrored-resources))));later pmap
  ([resource]
   (let [dir (mirror-dir resource)]
     (try
       (println "mirroring: " (:name resource))
       (repo resource)
       (try
         (copy-resource resource)
         (catch Exception e (println "unable to download " (:name resource))
                (spit "log.log" (json {:name (:name resource) :e (str e) :en 1}))))
       (adda dir)
       (commit (str "Generado por la refinería en: " (t/now)) dir)
       (push dir "origin" "master")
       (checkout-B dir "refineria")
       (git-merge)
       (refina dir)
       (adda dir)
       (commit (str "Generado por la refinería en: " (t/now)) dir)
       (push-force dir "origin" "refineria")
       (apify resource)
       (catch Exception e (println e "\nCould not finish on: " (:name resource))
              (spit "log.log" (json {:name (:name resource) :e (str e) :en 2})))))))
