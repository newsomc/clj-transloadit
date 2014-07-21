(ns clj-transloadit.core
  (:import java.text.SimpleDateFormat)
  (:import java.util.Calendar)
  (:require [pandect.core :refer :all]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            [clj-http.util :as http-util]))

(def service "api2.transloadit.com")
(def path "/assemblies")
(def auth-key "")
(def auth-secret "")
(def state {
   :auth-key auth-key
   :auth-secret auth-secret
   :service service
   :region "us-east-1"
   :protocol "http://"
   :streams {}
   :options {}
   :request-opts {
     :url "http://api2.url/assemblies"
     :method "post"
     :timeout (* 24 60 60 1000)
     :params {}
     :fields {}
   }})

(defn get-current-iso-date
  "Returns current ISO 8601 compliant date."
  [days]
  (let [cal (Calendar/getInstance)]
    (if-not (nil? days)
      (.add cal Calendar/DATE days))
    (.format (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ssZ") (.getTime cal))))

(defn add-option 
  "Conj an to options and return new state."
  [options]
  (if-not (and (empty? options) (map? options)) 
    (-> state update-in [:options] conj options)
    state))

(defn add-stream 
  "Conj on a new stream. Should be {:stream-name stream}"
  [stream]
  (if-not (and (not (empty? stream)) (map? stream))
        (-> state update-in [:streams] conj stream)
    state))

(defn reset-streams []
  (-> state assoc-in [:streams] {}))

(defn add-file [name path]
  (let [name (keyword name)
        stream (io/file path)]
    (add-stream {name stream})))

(declare get-bored-instance remote-json service-url prepare-params)

(defn create-assembly [func & opts]
  (get-bored-instance nil true 
    (fn [err url]
      (if (or err (not url))
        (func err)
        (remote-json (:request-opts state) 
          (fn [err result]
            (let [res (json/read-str result :key-fn keyword)]
              (if err
                (func err))          
              (if (and res (:ok res))
                (func nil res))
              (func "Error!"))))))))

(defn remove-assembly [assembly-id func]
  (let [opts {:url (str (service-url) "/assembly/" assembly-id)
              :timeout 16000}]
    (remote-json opts
      (fn [err result]
        (let [res (json/read-str result :key-fn keyword)]
          (if err 
            (func err)
            (do 
              (-> opts 
                (assoc-in [:url] (:assembly-id res))
                (assoc-in [:timeout] 5000)
                (client/delete ([:url] opts))))))))))

(defn replay-assembly [opts func]
  (let [assembly-id (:assembly-id opts)
        request-opts {:url (str (service-url) "/assembly/" assembly-id "/replay")
              :method "POST"}]
    (if (:notify-url opts)
      (-> request-opts (assoc :params {:notify-url opts}))
      request-opts)
    (remote-json request-opts func)))

(defn replay-assembly-notification [opts func]
  (let [assembly-id (:assembly-id opts)
        request-opts {:url (str (service-url) "/assembly_notifications/" assembly-id "/replay")
              :method "POST"}]
    (if (:notify-url opts)
      (-> request-opts (assoc :params {:notify-url opts}))
      request-opts)
    (remote-json request-opts func)))

(defn- get-bored-instance [])

(defn- remote-json [])

(defn list-assembly-notifications [params func]
  (let [request-opts {:url (str (service-url) "/assemblies")
                      :method "GET"
                      :params (or params {})}]
    (remote-json request-opts func)))

(defn list-assemblies [params func]
  (let [request-opts {:url (str (service-url) "/assemblies")
                      :method "get"
                      :params (or params {})}]
    (remote-json request-opts func)))

(defn get-assembly [assembly-id func]
  (let [opts {:url (str (service-url) "/assemblies/" assembly-id)
              :error "NOT OK"}]
    (remote-json opts 
      (fn [err result]
        (if err
          (func err)
          (let [res (json/read-str result :key-fn keyword)]
            (if (and (not (nil? res)) (:ok res))
              (func nil res)
              (func (:error opts)))))))))

(defn create-template [params func])

(defn edit-template [template-id params func])

(defn delete-template [template-id func]
  (let [request-opts {:url (str (service-url) "/templates/" template-id)
                      :method "DELETE"
                      :params {}}]
    (remote-json request-opts func)))

(defn get-template [template-id func]
  (let [request-opts {:url (str (service-url) "/templates/" template-id)
                      :method "GET"
                      :params {}}]
    (remote-json request-opts func)))

(defn list-templates [params func]
  (let [request-opts {:url (str (service-url) "/templates")
                      :method "GET"
                      :params (or params {})}]
    (remote-json request-opts func)))

(defn calc-signature [to-sign]
  (sha1-hmac to-sign auth-secret))

(defn- assoc-form [req params fields state]
  (let [r (json/read-str req :key-fn keyword)
        clj-params (prepare-params params)
        signature (calc-signature clj-params)
        fields (if (empty? fields) {} (json/read-str fields :key-fn keyword))
        form (merge (:form r) fields)]
    (merge (:streams state) 
      (-> form
        (assoc :params clj-params)
        (assoc :signature signature)))))

(defn- append-signature [path]
  (let [pos (.indexOf path "?")]
    (if (= pos 0) "?signature" "&signature")))

(defn- append-params-to-url [url params]
  (let [clj-params (prepare-params params) 
        signature (calc-signature clj-params)
        url (str (append-signature url) signature)
        encoded-params (http-util/url-encode (json/write-str clj-params))]
    (str url "&params=" encoded-params)))

(defn- get-expires-date []
  (get-current-iso-date 1))

(defn- prepare-params [params]
  (let [p (json/read-str params :key-fn keyword)]
    (cond
      (nil? p) {}
      (nil? (:auth p)) (assoc p :auth {})
      (nil? (:key (:auth p))) (assoc p :key auth-key))
    (-> p
      (assoc-in [:auth :expires] (get-expires-date)))))

(defn- service-url [] 
  (str (:protocol state) (:service state)))

(defn- find-bored-instance-url [func])

(defn- find-responsive-instance [instances index func]
  (let [err (if (nil? (get instances index)) (do (func error)))
        url (str protocol (get instances index))
        opts {:uri url :timeout 3000}]
    (remote-json opts 
      (fn []
        (let [result 
              instances])))
    )
  )

(defn- remote-json [opts func]
  (let [method  (or (:method opts) "GET")
        params  (or (:params opts) {})
        timeout (or (:timeout opts) 5000)
        url (if (and (= method "GET") (not (nil? params)))
              (append-params-to-url url params)
              nil)
        request-opts {:uri url :timeout timeout}
        req (client/method url
              {:query-params params
               :content-type :json
               :socket-timeout 1000  ;; in milliseconds
               :conn-timeout timeout    ;; in milliseconds
               :accept :json})]
    (if (or (= method "POST") (= method "PUT") (= method "DELETE"))
      (-> req (assoc-form (:params opts) (:fields opts)))
      req)))
