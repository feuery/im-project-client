(ns mese-client.friends
  (:require [org.httpkit.client :as http]
            [clojure.pprint :refer :all]
            [mese-client.util :refer [in?]]                        
            [mese-client.settings :refer [get-setting]]))

(defn get-current-users [session-id]
  (let [{body :body :as result} @(http/get (str (get-setting :server-url) "list-friends/" session-id))]
    ;; (println "Results: ")
    ;; (pprint result)
    ;(println "@get-current-users: body: " body)
    (if-let [result (read-string body)]
      (if (map? result)
        (do
          (println "Result was a map: " result)
          false)
        (do
          (println "class of state: " (map (comp class :state) result))
          result)))))

(defn get-friend-requests [session-id]
  (let [{body :body :as result} @(http/get (str (get-setting :server-url) "friend-requests/" session-id))]
    (println "result@get-friend-requests: " result)
    (if-let [result (read-string body)]
      (if (:success result)
        (:requests result)
        false)
      false)))

(defn send-request [session-id friend-handle]
  (let [{body :body :as result} @(http/get (str (get-setting :server-url) "friend-request/" session-id "/" friend-handle))]
    (println "result@send-request: " result)
    (if-let [result (read-string body)]
      (if (and (map? result)
               (contains? result :success))
        (:success result)
        false)
      false)))
  

(defn accept-request [session-id requester-handle]
  (println "Accepting " requester-handle)
  (let [url (str (get-setting :server-url) "accept-request/" session-id "/" requester-handle)]
    (println "url: " url)
    (let [{body :body :as result} @(http/get url)]
      (println "result@accept-request: " body)
      (if-let [result (read-string body)]
        (if (and (map? result)
                 (contains? result :success))
          (:success result)
          (throw (Exception. "Received rubbish from the server: " body)))
        false))))

(defn update-myself [sessid userhandle property value]
  (let [url (str (get-setting :server-url) "update-myself/" sessid "/" userhandle "/")
        options {:form-params {:property property :new-value value}}]
    (println "Url: " url " | userhandle: " userhandle)
    (when-let [{body :body :as result} @(http/post url options)]
      (println "body: " body)
      (if-let [result (read-string body)]
        (if (map? result)
          (:success result)
          false)))))

(defn update-font [sessid userhandle
                   & {:keys [bold? italic? underline? color font-name]
                      :or {bold? false
                           italic? false
                           underline? false
                           color "#000000"
                           font-name "arial"}}]
  (println "Some fool is updating fonts")
  (update-myself sessid userhandle :font-preferences
                 (pr-str {:bold? bold?
                          :italic? italic?
                          :underline? underline?
                          :color color
                          :font-name font-name})))
                          

(def possible-states [:online :busy :away :returning :lunch :fake-offline :real-offline])

(defn state-to-color [state]
  {:pre [(in? possible-states state)]}
  ;; (println "state: " state)
  (cond
   (in? [:online] state) "#1AFF00"
   (in? [:busy] state) "#FF0000"
   (in? [:away :returning :lunch] state) "#FFA600"
   :t "#999999"))
