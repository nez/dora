(ns dora.p.zendesk
  "Zendesk API wrapper"
  (:require [org.httpkit.client :as client]
            [clojure.data.json :as json]
            [clojure.string :as s]
            [clj-pdf.core :refer :all]
            [clj-time.format :as f]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [mongerr.core :refer :all]
            [nillib.formats :refer :all]
            [nillib.text :refer :all]
            [clojure.string :as str]))

(def zen-auth {:basic-auth [(env :zendesk-email) (env :zendesk-password)]})

(def content-json {:headers {"Content-Type" "application/json"}})

(defn zen-headers [& maps]
  (apply merge zen-auth))

(defn handle-response
  "Parse the result and handle error responses"
  [{:keys [status headers body error]}]
  (if error
    (println "Failed, exception is " error)
    (json/read-str body :key-fn keyword)))

(defn request
  "Make a request to an url"
  [url]
  (client/get url
              zen-auth
              handle-response))

(defn zendesk-request-url
  "Url request as a string"
  [endpoint]
  (str "https://mxabierto.zendesk.com/api/v2/" (name endpoint) ".json"))

(defn zendesk-request
  "Make a request to zendesk"
  [endpoint]
  (request (zendesk-request-url endpoint)))

(defn json-body
  "Make a map {:body JSON}"
  [m]
  {:body (json/write-str m)})

(defn create-user
  "Create a new zendesk user"
  ([name email] (create-user name email {}))
  ([name email map-to-merge]
   (client/post (zendesk-request-url :users)
                (zen-headers content-json
                             (json-body {:user (merge {:name name :email email :verified true}
                                                                  map-to-merge)}))
      handle-response)))

(defn update-user
  [user-id new-data]
  (client/put (zendesk-request-url (str "users/" user-id))
              (zen-headers content-json
                           (json-body {:user new-data}))
      handle-response))


(defn read-tickets [data]
  (json/read-str (:body data) :key-fn keyword))

(defn articles
  []
  (:articles @(request "https://mxabierto.zendesk.com/api/v2/help_center/articles.json?page=1&per_page=100")))

(defn report-articles []
  (csv "articulos.csv" (map #(select-keys % [:html_url :name]) (articles))))

(defn get-all-pages
  ([endpoint]
   (get-all-pages endpoint endpoint))
  ([endpoint keyname]
   (loop [curr (zendesk-request endpoint)
          all []]
     (if (:next_page @curr)
       (recur (request (:next_page @curr))
              (concat all (keyname @curr)))
       (concat all (endpoint @curr))))))

(defn all-tickets []
  (get-all-pages :tickets))

(defn all-organizations []
  (get-all-pages :organizations))

(defn all-satisfaction []
  (get-all-pages :satisfaction_ratings))

(defn all-users []
  (get-all-pages :users))

(comment (defn all-articles []
           (get-all-pages "help_center/articles" :articles)))

(defn due-tickets [ticks]
  (filter #(or (= "open" (:status %))
               (= "pending" (:status %))) ticks))

(defn done-tickets [ticks]
  (filter #(or (= "closed" (:status %))
               (= "solved" (:status %))) ticks))

(def important-keys [:subject :description :tags :created_at :status :assignee_id  :id])

(defn tickets-inform []
  (let [t (due-tickets (all-tickets))
        data (map #(select-keys % important-keys) t)]
    (csv "Informe-Tickets.csv" data)))

(defn closed-tickets-inform []
  (let [t (done-tickets (all-tickets))
        ;data (map #(select-keys % important-keys) t)
        ]
    (csv "Informe-Tickets-Resueltos.csv" t)))

(defn gram-1 [tickets]
  (estudio-de-keywords (tickets-text tickets)))

(defn gram-2 []
  (ngram-study 2 (tickets-text (all-tickets))))

(defn attachment
  ([id] (zendesk-request (str "attachments/" id))))

(defn end-users []
  (filter #(= "end-user" (:role %)) (all-users)))

(defn user-id [email]
  (:id (first (filter #(= email (:email %)) (all-users)))))

(def usrs (all-users))
(defn user-data [id]
  (select-keys (first (filter #(= id (:id %)) usrs)) [:name :email]))

;; Reportes
(defn reporte-infotec [email month day]
  (let [id (user-id email)
        data (filter #(= id (:assignee_id %)) (done-tickets (all-tickets)))
        data (filter #(t/before? (t/date-time (t/year (t/now)) month day)
                                 (f/parse (:updated_at %)))
                     data)]
    data))

(defn format-reporte [data]
  (let [data (map #(assoc %1 :Req %2 :Resultado "Entregado" :Estado "T" :Avance "100%")
                  data
                  (map inc (range)))]
    (csv "Reporte.csv" data [:Req :type :subject :Resultado :Estado :Avance])))


(defn pdf-evidencia [data]
  (pdf [{}
        [:paragraph (str
"Comentarios que Sugieren Atención a un Recurso de Datos


Estimado Administrador de Datos,

En un intento por mejorar el servicio de Datos Abiertos, perfeccionar los Recursos de Datos que las

Dependencias de la Administración Pública publican y asegurar su accesibilidad y permanencia, esta

Dirección General ha realizado un ejercicio de prueba ­con la intención de hacerlo permanente­ para

comprobar el funcionamiento de la descarga de sus recursos de datos. Durante dicha prueba,

detectamos posibles problemas con los siguientes recursos de datos bajo su responsabilidad:

URL: " (:url data) "

Fecha de prueba: " (:now data)"

Los errores pueden ser los siguientes:

1.­ El servidor no está disponible.

2.­ El recurso requiere derechos de acceso.

3.­ El servidor toma demasiado tiempo en responder a una solicitud por el recurso.

Amablemente sugerimos atender dichas ligas y revisar todos los conjuntos de datos restantes que su

dependencia publica en el sitio. Sin más por el momento, me mantengo a su disposición para resolver

cualquier duda sobre el proceso de cumplimiento de la Política de Datos Abiertos en el correo

escuadron@datos.gob.mx o vía telefónica al 50935300 ext: 7054.

Saludos cordiales.

Coordinación de Estrategia Digital Nacional

Presidencia de la República

www.datos.gob.mx")]]
       (str "pdfs/reporte-" (rand) ".pdf")))

(defn pdfs-month-ligas-rotas []
  (let [data (filter #(t/before? (t/date-time (t/year (t/now)) (t/month (t/now)) 1)
                                 (:now %))
                     (db-find :status-broken))]
    data))

(defn genera-evidencia []
  (map pdf-evidencia (pdfs-month-ligas-rotas)))

(defn pdf-evidencia-pira [data num]
  (pdf [{}
        [:paragraph (str
"Comentarios que Sugieren Atención a un Recurso de Datos


Estimado Administrador de Datos,

En un intento por mejorar el servicio de Datos Abiertos, perfeccionar los Recursos de Datos que las

Dependencias de la Administración Pública publican y asegurar su accesibilidad y permanencia, esta

Dirección General ha realizado un ejercicio de prueba ­con la intención de hacerlo permanente­ para

comprobar el funcionamiento de la descarga de sus recursos de datos. Durante dicha prueba,

detectamos posibles problemas con los siguientes recursos de datos bajo su responsabilidad:

URLS:


" (str/join "\n\n" data) "


Fecha de prueba:  2017/03/" num"

Los errores pueden ser los siguientes:

1.­ El servidor no está disponible.

2.­ El recurso requiere derechos de acceso.

3.­ El servidor toma demasiado tiempo en responder a una solicitud por el recurso.

Amablemente sugerimos atender dichas ligas y revisar todos los conjuntos de datos restantes que su

dependencia publica en el sitio. Sin más por el momento, me mantengo a su disposición para resolver

cualquier duda sobre el proceso de cumplimiento de la Política de Datos Abiertos en el correo

escuadron@datos.gob.mx o vía telefónica al 50935300 ext: 7054.

Saludos cordiales.

Coordinación de Estrategia Digital Nacional

Presidencia de la República

www.datos.gob.mx")]]
       (str "pdfs/reporte-" num ".pdf")))

(defn genera-pdf-evidencia-pira
  [dias data]
  (let [partitions (int  (/ (count data) dias))]
    (map pdf-evidencia-pira
         (partition partitions (map :url data))
         (range 1 dias))))

;(genera-pdf-evidencia-pira NUM-DIAS (errors-today))
