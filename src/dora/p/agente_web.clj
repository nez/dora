(ns dora.p.agente-web
  "Web agent for broken link identification"
  (:require [clj-http.client :as http]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.java.shell :as sh]
            [clojure.set :as set]
            [clojure.string :as s]
            [dora.p.zendesk :refer :all]
            [monger.operators :refer :all]
            monger.joda-time
            [mongerr.core :refer :all]
            [nillib.formats :refer :all]
            [postal.core :refer :all]
            [ring.util.codec :refer :all])
  (:gen-class))

(defn error
  "save to error-log.log"
  [& strs]
  (do (println "error: " (s/join strs))
      (spit "error-log.log" (str (s/join strs) "\n") :append true)))

(defn info
  "save to info-log.log"
  [& strs]
  (do (println "info: " (s/join strs))
      (spit "info-log.log" (str (s/join strs) "\n") :append true)))

(def user-agent
  "Mozilla/5.0 (X11; U; Linux x86_64; en-US) AppleWebKit/532.9 (KHTML, like Gecko) Chrome/5.0.307.11 Safari/532.9")

(defn status-with-usr-agent
  "Get the status code for a resource while faking the user agent"
  [url]
  (try
    (info "checking with fake agent: " url)
    (:status (http/get url {:headers {"user-agent"  user-agent}
                            :conn-timeout 5000}))
    (catch org.apache.http.conn.ConnectTimeoutException e
      (db-insert :status {:now (t/now) :url url :status :timeout}))
    (catch Exception e
      (let [request (db-insert :status {:now (t/now) :url url :status :error})]
        :error))))

(defn status
  "Está online el recurso web?."
  [url]
  (try (let [status (:status (http/head url {:conn-timeout 6000}))]
         (db-insert :status {:now (t/now) :url url :status status})
         status)
    (catch org.apache.http.conn.ConnectTimeoutException e :timeout)
    (catch Exception e (status-with-usr-agent url))))

(defn check-status
  []
  (doall (map #(-> % :url status-with-usr-agent) (db :resources))))

(defn get-status-1
  "Funcion para llamar desde jenkins para sacar el status de todos al dia"
  []
  (let [r (map :url (db-find "resources"))
        total-r (count r)
        a  (println "Checked " total-r " resources")
        statuses (pmap status r)]
       (println "validated " (count statuses) " statuses")))

(defn inegi-status []
  (let [url "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/imagen_cartografica/1_50_000/702825269258_s.zip"]
    (db-insert :inegi-status {:url url :status (status url)})))

(defn trim-or-empty
  "Trim string but if it is not a string, then return empty string"
  [s]
  (if (string? s)
      (s/trim s)
      ""))

(defn status-curl
  "Resource status using curl"
  [url]
  (trim-or-empty (first (re-seq #"[^\n]+"
                                (:out (sh/sh "curl" "-Is" url))))))

(defn ok-curl-status
  "Is this status ok?"
  [status]
  (if (string? status)
      (or (re-find #"200" status)
          (re-find #"301" status)
          (re-find #"302" status)
          (empty? status))
      false))

(defn check-broken-curl-status
  "Check the status of a list of urls"
  [urls]
  (remove #(ok-curl-status (:status %))
          (pmap #(hash-map :status (status-curl %)
                           :url %)
                urls)))


(defn check-urls
  "Check a list of urls, store in :status collection"
  [urls]
  (pmap #(try (assoc % :status (status (:url %)))
              (catch Exception e
                (db-insert :errors
                           {:now (t/now)
                            :here "f.p.agente-web/check-urls"
                            :exception (str e)})))
        urls))

(defn save-failures
  "Save failures on db"
  [failures]
  (db-insert :ligas-caidas
             {:time (t/now)
              :urls (map #(select-keys % [:organization :url])
                         failures)}))

(defn failed-urls []
  ;las urls de hoy y las del dia anterior
  ;agarra las que aparezcan 2 veces
  ;agrupa por organizacion()
  ;agarra los contactos de las organizaciones
  ;manda tickets a cada organizacion
  )

(defn time-format
  "Convert java objects to yyyy-mm-dd"
  [time]
  (s/join (take 10 (str time))))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))


(defn str-reporte-downtime [failures organizations]
  (str "Hay " (count failures) " ligas rotas, de " (count (distinct (map :organization failures))) " instituciones.

            El informe completo se encuentra en: http://crm.fractal-ware.com/images/ligas-rotas.csv

            NOTA: El código que genera esta alerta puede contener errores, es indispensable validar manualmente una liga antes de reportar a una dependencia.
            En caso de encontrar errores en este informe, favor de contestar sobre este ticket."))

(defn handle-failures
  [failures]
  (if-not (empty? failures)
    (let [without-false-negatives (remove #(= :timeout (:status %))
                                          (remove #(re-find #"coneval" (:url %))
                                                  failures))]
      (csv "resources/failed-urls.csv" (sort-by :organization failures))
      (when (seq without-false-negatives)
        (try (csv "resources/ligas-rotas.csv"
                  (sort-by :organization failures))
             (catch Exception e (error "Error 90" e)))
        (try (save-failures (sort-by :organization failures))
             (catch Exception e (error "Error 91" e)))
        (comment (try (ticket-downtime
                       (str-reporte-downtime
                        (count failures)
                        (count (distinct (map :organization failures)))))
                      (catch Exception e (error "Error 92" e))))
        ;(try (link-uptime-report) (catch Exception e (error "Error 96" e)))
        ))))

(defn check
  [urls]
  (let [checked (check-urls urls)
        all-failed (remove #(= 200 (:status %)) checked)]
    (csv "resources/checked-urls.csv" (sort-by :organization checked))
    (handle-failures all-failed)))

(defn check-file [f]
  (check (csv f)))

(defn unchecked-resources
  []
; (check (db-find "resources"))) ;;TODO meter a la query solo los de hoy
  (let [already-checked (set (map :url (db-find :status)))]
    (remove #(set/subset? (set [(:url %)]) already-checked)
            (db-find :resources))))

(defn check-resources [] (check (unchecked-resources)))

(defn analisis [rel]
  {:total (count rel)
   :up (count (filter #(= "true" (:status %)) rel))
   :down (map :url (remove #(= "true" (:status %)) rel))})

;(def urls-checadas (check-urls (db-find "resources")))

(defn update-status [resource]
  (db-upsert :resources
             (select-keys resource [:id])
             {:status (status (:url resource))
              :check-status (t/now)}))

(defn check-ckan-urls
  ([] (check-ckan-urls (db-find :resources)))
  ([resources]
   (pmap #(try (update-status %)
               (catch Exception e (error "check-ckan-urls " e)))
          resources)))

;(csv "error-list.csv" (db-find :resources {:status "error"}))
(defn url->org [url]
  (:organization (db-find :resources {:url url})))

;enlaces y admins
(defn email-admin [siglas-dependencia]
  ((keyword "Correo Electrónico Administrador")
   (db-findf :people {:Siglas (s/upper-case siglas-dependencia)})))

(defn e-mail [to subject body]
  (send-message {:host "smtp.gmail.com"
                 :user "escuadron.datos@gmail.com"
                 :pass "d4t0s.mx"
                 :ssl :yes!!!11}
                {:from "escuadron.datos@gmail.com"
                 :to to
                 :subject subject
                 :body body}))

(defn email-ligas-rotas [to url-list]
  (e-mail to
         "Reporte de urls rotas"
         (str "Las siguientes ligas estan rotas:\n" (s/join "\n" url-list))))


(defn errors-today []
  (distinct (map #(select-keys % [:url :status])
                 (filter #(t/before? (t/today-at-midnight) (:now %))
                         (db-find :status {:status :error })))))
                       ;:time {$gte (t/today-at-midnight)}


(defn save-broken-links []
  (db-insert :status-broken
             (map #(assoc % :now (t/now))
                  (check-broken-curl-status (map :url (errors-today))))))

(defn broken
  "Endpoint with clear broken urls"
  ([]
   (broken nil))
  ([query]
   (distinct (filter #(not (ok-curl-status (:status %)))
                     (db-find :status-broken query)))))

(defn last-day?
  "Is t more recent than 24h ago?"
  [t]
  (t/before? (t/minus (t/now)
                      (t/days 1))
             t))

(defn last-2days?
  "Is t more recent than 48h ago?"
  [t]
  (t/before? (t/minus (t/now)
                      (t/days 1))
             t))

(defn broken-today
  ([] (filter #(last-day? (:now %))
              (broken)))
  ([url]
   (if-not (nil? url)
     (first (filter #(last-day? (:now %))
                    (broken {:url url}))))))

(defn broken-today
  (distinct-by :url
               (db :status {:date-insert {:$gt (t/minus (t/now) (t/days 2))}
                            :status "error"})))

(defn sure-errors []
 (map :url (broken-today)))
