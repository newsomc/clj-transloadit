(ns clj-transloadit.core
  (:require [pandect.core :refer :all]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

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

(defn add-option 
  "Conj an to options and return new state."
  [options]
  (if-not (and (empty? options) (map? options)) 
    (-> state update-in [:options] conj options)
    state))

(defn add-stream 
  "Conj on a new stream. Should be {:stream-name stream}"
  [stream]
  (if-not (and (empty? stream) (map? stream))
        (-> state update-in [:streams] conj stream)
    state))

(defn reset-streams []
  (-> state assoc-in [:streams] {}))

(defn add-file [name path]
  (let [name (keyword name)
        stream (io/file path)]
    (add-stream {name stream})))

(declare get-bored-instance remote-json)

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

(defn- get-bored-instance [] )

(defn- remote-json [])

(defn remove-assembly [assembly-id func]
  (let [opts {:url (+ (service-url) (str "/assembly/" assembly-id))
              :timeout 16000}]
    (remote-json opts
      (fn [err result]
        (let [res (json/read-str result :key-fn keyword)]
          (if err 
            (func err)
            (do 
              (-> opts 
                (assoc-in [:url] (:assembly-id res))
                (assoc-in [:timeout] 5000)))))))))

(defn replay-assembly-notification [opts func])

(defn list-assembly-notifications [params func])

(defn list-assemblies [params func])

(defn get-assembly [assembly-id func])

(defn create-template [params func])

(defn edit-template [template-id params func])

(defn delete-template [template-id func])

(defn get-template [template-id func])

(defn list-templates [params func])

(defn calc-signature [to-sign])

(defn- append-form [req params fields])

(defn- append-params-to-url [url params])

(defn- find-bored-instance-url [func])

(defn- find-responsive-instance [instances index func])

(defn- prepare-params [params])

(defn- get-expires-date [])

(defn- service-url [] )

(defn- remote-json [opts func])
