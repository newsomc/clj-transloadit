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
(defn init [opts]
  (def state {:auth-key (:auth-key opts)
              :auth-secret (:auth-secret opts)
              :service service
              :region "us-east-1"
              :protocol "http://"
              :streams {}
              :options {}
              :request-opts {:url "http://api2.url/assemblies"
                             :method "post"
                             :timeout (* 24 60 60 1000)
                             :params {}
                             :fields {}}})
  state)

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
  (if (and (not (empty? stream)) (map? stream))
    (-> state (update-in [:streams] conj stream))
    state))

(defn reset-streams []
  (-> state assoc-in [:streams] {}))

(defn add-file [name path]
  (let [name (keyword name)
        stream (io/file path)]
    (println name stream)
    (add-stream {name stream})))

(declare get-bored-instance remote-data service-url prepare-params)

(defn create-assembly [opts func]
  (get-bored-instance nil true 
    (fn [err url]
      (if (or err (not url))
        (func err)
        (remote-data (:request-opts state) 
          (fn [err result]
            (cond 
              err (func err)
              (and res (:ok result)) (func nil result)
              :else (func "Error!"))))))))

(defn remove-assembly [assembly-id func]
  (let [opts {:url (str (service-url) "/assembly/" assembly-id)
              :timeout 16000}]
    (remote-data opts
      (fn [err result]
        (if err 
          (func err)
          (do 
            (-> opts 
              (assoc-in [:url] (:assembly-id result))
              (assoc-in [:timeout] 5000)
              (client/delete ([:url] opts)))))))))

(defn replay-assembly [opts func]
  (let [assembly-id (:assembly-id opts)
        request-opts {:url (str (service-url) "/assembly/" assembly-id "/replay")
              :method "POST"}]
    (if (:notify-url opts)
      (-> request-opts (assoc :params {:notify-url opts}))
      request-opts)
    (remote-data request-opts func)))

(defn replay-assembly-notification [opts func]
  (let [assembly-id (:assembly-id opts)
        request-opts {:url (str (service-url) "/assembly_notifications/" assembly-id "/replay")
              :method "POST"}]
    (if (:notify-url opts)
      (-> request-opts (assoc :params {:notify-url opts}))
      request-opts)
    (remote-data request-opts func)))

(defn- get-bored-instance [url custom-bored-logic func]
  (let [url (if (nil? url) (str (service-url) "/instances/bored"))
        opts {:url url}]
    (remote-data opts 
      (fn [err instance]
        (if (nil? error)
          (if (:error instance)
            (func (:error instance)))
          (func nil (:api2_host instance)))
        (if-not (nil? custom-bored-logic)
          (find-bored-instance-url 
            (fn [err the-url]
              (if err
                (func {:error "BORED_INSTANCE_ERROR"
                        :message (str "Could not find a bored instance. " err)})
                (get-bored-instance 
                  (str (:protocol state) "api2." the-url "/instances/bored") false func))))
          (func {:error "CONNECTION_ERROR"
                  :message "There was a problem connecting to the upload server"
                  :reason err
                  :url url}))))))

(defn list-assembly-notifications [params func]
  (let [request-opts {:url (str (service-url) "/assemblies")
                      :method "GET"
                      :params (or params {})}]
    (remote-data request-opts func)))

(defn list-assemblies [params func]
  (let [request-opts {:url (str (service-url) "/assemblies")
                      :method "get"
                      :params (or params {})}]
    (remote-data request-opts func)))

(defn get-assembly [assembly-id func]
  (let [opts {:url (str (service-url) "/assemblies/" assembly-id)
              :error "NOT OK"}]
    (remote-data opts 
      (fn [err result]
        (if err
          (func err)
          (if (and (not (nil? result)) (:ok result))
            (func nil res)
            (func (:error opts))))))))

;; (defn create-template [params func])

;; (defn edit-template [template-id params func])

(defn delete-template [template-id func]
  (let [request-opts {:url (str (service-url) "/templates/" template-id)
                      :method "DELETE"
                      :params {}}]
    (remote-data request-opts func)))

(defn get-template [template-id func]
  (let [request-opts {:url (str (service-url) "/templates/" template-id)
                      :method "GET"
                      :params {}}]
    (remote-data request-opts func)))

(defn list-templates [params func]
  (let [request-opts {:url (str (service-url) "/templates")
                      :method "GET"
                      :params (or params {})}]
    (remote-data request-opts func)))

(defn calc-signature [to-sign]
  (sha1-hmac to-sign auth-secret))

(defn- assoc-form [req params fields state]
  (let [clj-params (prepare-params params)
        signature (calc-signature clj-params)
        fields (if (empty? fields) {} fields)
        form (merge (:form req) fields)]
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
  (cond
    (nil? params) {}
    (nil? (:auth params)) (assoc params :auth {})
    (nil? (:key (:auth params))) (assoc params :key auth-key))
  (-> params
    (assoc-in [:auth :expires] (get-expires-date))))

(defn- service-url [] 
  (str (:protocol state) (:service state)))

(defn- find-responsive-instance [instances index func]
  (let [err (if (nil? (get instances index)) 
              "Error: No responsive instances" 
              nil)
        url (str (:protocol state) (get instances index))
        opts {:uri url :timeout 3000}]
    (if err
      (func err)
      (remote-data opts 
        (fn [err result]
          (if err
            (find-responsive-instance instances (inc index) func)
            (func result)))))))

(defn- find-bored-instance-url [func]
  (let [base "http://infra-"
        path ".transloadit.com.s3.amazonaw.com/"
        instances "cached_instances.json"
        url (str base (:region state) path instances)
        opts {:url url :timeout 3000}]
    (remote-data opts 
      (fn [err result]
        (let [instances (shuffle (:uploders result))]
          (if err
            (func (str "Could not uery S3 for cached uploaders:" (:message err)))
            (find-responsive-instance instances 0 func)))))))

(defn remote-data [opts func]
  (let [method  (or (:method opts) "post")
        params  (or (:params opts) {})
        timeout (or (:timeout opts) 5000)
        url     (or (:url opts) nil)
        req     (if url
                  (client/post url
                    {:query-params params
                     :content-type :json
                     :socket-timeout 1000  ;; in milliseconds
                     :conn-timeout timeout    ;; in milliseconds
                     :throw-entire-message? true
                     :accept :json})
                  (println  "Error: No URL provided"))]
    (if (or (= method "post") (= method "put") (= method "del"))
      (-> req 
        (assoc-form (:params opts) (:fields opts)))
      req)))

(comment
    (if (and (= method "GET") (not (nil? params)))
      (append-params-to-url url params)
      nil))
