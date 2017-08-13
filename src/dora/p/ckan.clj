(ns dora.p.ckan
  "Wrapper for parts of CKAN API"
  (:require [clj-http.client :as http]
            [clj-http.util :refer :all]
            [clojure.data.json :as json]
            [clojure.string :as s]
            [digitalize.core :refer :all]
            [environ.core :refer [env]]
            [mongerr.core :refer :all]
            [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            monger.joda-time
            [nillib.formats :refer :all])
  (:import [com.mongodb MongoOptions ServerAddress]
           org.apache.commons.validator.UrlValidator))

(defn get-json
  "GET a JSON endpoint"
  [url]
  (println "fetching: " url)
  (-> url
      http/get
      :body
      (json/read-str :key-fn
                     (comp keyword #(clojure.string/replace % " " "_")))))

(def ^:dynamic *ckan-url* (or (env :ckan-url)
                              "https://datos.gob.mx/busca/api/3/"))

(defn api
  "make a call on CKAN endpoint"
  [& endpoint]
  (try (:result (get-json (str *ckan-url*
                               (s/join endpoint))))
       (catch Exception e (println "exception in endpoint: " (s/join endpoint)"\n" e))))

(defn package-list
  "get list of packages"
  []
  (api "action/package_list"))

(defn package-show
  "full representation of a package
  or all packages if called without args"
  ([] (pmap package-show (package-list)))
  ([package]
    (api "action/package_show?id=" package)))

(defn group-list
  "get list of groups"
  []
  (api "action/group_list"))

(defn group-show
  "full representation of group
  all groups if called without args"
  ([] (pmap group-show (group-list)))
  ([id]
   (api "action/group_show?id=" id)))

(defn tag-list
  "list of different tags"
  []
  (api "action/tag_list"))

(defn tag-show
  "get data on certain tag
  or on all tags with no args"
  ([] (map tag-show (tag-list)))
  ([id]
   (api "action/tag_show?id=" id)))

(defn package-search
  ([]
   (api "action/package_search?rows=300000"))
  ([q]
   (api "action/package_search?q=" q)))

(defn resource-search
  [q]
  (api "action/resource_search?query=" q))

(defn recently-changed
  []
  (api "action/recently_changed_packages_activity_list"))

(defn all-ckan
  "all data on all packages"
  []
  (digitalize (map package-show (package-list)) :clean-numbers false))

(defn email
  "Exctract email from a dataset"
  [dataset]
  (let [extras (:extras dataset)
        email (filter #(= "dcat_publisher_email" (:key %)) extras)]
    (if-not (empty? email)
      (re-seq #"[^,; \n]+"
              (:value (first email))))))

(defn emails
  "emails from a list of datasets"
  [datasets]
  (remove nil? (distinct (flatten (map email datasets)))))

(defn urls
  "list of urls from the list of datasets"
  [datasets]
  (flatten (map #(map :url (:resources %)) datasets)))

(defn ckan-organizations
  "list of organizations from a list of datasets"
  []
  (api "action/organization_list"))

(defn resources-from-dataset
  "list of resources, with organization name and dataset id"
  [dataset]
  (map #(assoc % :dataset_id (:id dataset)
                 :organization (:name (:organization dataset)))
      (:resources dataset)))

(defn update-all-ckan
  []
  (let [data (doall (all-ckan))]
    (when-not (empty? data)
      (db-delete :datasets)
      (doall (map #(db-insert :datasets %) data))
      (db-delete :resources)
      (db-insert :resources (mapcat resources-from-dataset data)))))

(defn valid-url? [url-str]
  (let [validator (UrlValidator.)]
    (.isValid validator url-str)))

(defn resource
  "Find a resource with a URL"
  ;;TODO if url is invalid but it was to be a url things break
  [o]
  (if (map? o)
    (db-findf :resources o)
    (if (valid-url? o)
      (db-findf :resources {:url o})
      (if-not (empty? o)
        (or (db-findf :resources {:name o})
            (db-findf :resources {:id o})
            (db-find :resources {:dataset_id o}))))))
