(ns mese-client.settings
  (:require [clojure.java.io :as io]))

;(def server-url "http://localhost:5000/")

(defn get-initial-settings []
  (loop [counter 0]
    (when (> counter 1)
      (throw (Exception. (str "Creating settings file failed. Can I write to " (str (System/getProperty "user.home") "/.yool-im") "?"))))
    (let [home (System/getProperty "user.home")
          demosettings {:server-url "http://localhost:5000/"
                        :font-preferences {:bold? false
                                           :italic? false
                                           :underline? false
                                           :color "#000000"
                                           :font-name "arial"}
                        :running-in-server? false
                        :main-width 640
                        :main-height 480
                        :font-size 13}
          settingsfile (io/file (str home "/.yool-im"))
          exists? (.exists settingsfile)]
      (if-not exists?
        (do
          (spit settingsfile (pr-str demosettings))
          (recur (inc counter)))
        
        (-> settingsfile
            slurp
            read-string
            eval)))))

(def settings (atom (let [settings (get-initial-settings)]
                      (if (map? settings)
                        settings
                        (throw (Exception. (str "Loading settings failed! Settings file's tail position must have a map; got: " settings)))))))

(add-watch settings :settings-serializer
           (fn [_ _ _ new-state]
             (println "Settings changed: " new-state)
             (let [home (System/getProperty "user.home")
                   settingsfile (io/file (str home "/.yool-im"))]
               (.delete settingsfile)
               (spit settingsfile (pr-str new-state)))))


(defn get-setting [key]
  (get @settings key :not-found))
