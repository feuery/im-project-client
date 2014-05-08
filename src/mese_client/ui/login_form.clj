(ns mese-client.ui.login-form
  (:require [seesaw.core :refer :all]
            [mese-client.settings :refer [settings]]
            [seesaw.bind :as b]
            [mese-client.util :refer [in?]]))

(defn sha-512 [data]
  (let [md (. java.security.MessageDigest getInstance "sha-512")]
    (. md update (.getBytes data))
    (let [bytes (. md digest)]
      (reduce #(str %1 (format "%02x" %2)) "" bytes))))

(def window-states [:open :canceled :ready])

(defn edit-settings [user-atom]
  (try
    (frame :size [320 :by 240]
           :title "Propertyeditor"
           :visible? true
           
           :content (grid-panel :columns 2 :items
                                (-> (map (fn [[key val]]
                                       [(str key)
                                        (text :text (str val)
                                              :listen [:document
                                                       (fn [e]
                                                         (println "Swapping " key " to " (text e) " on " user-atom)
                                                         (swap! user-atom assoc key
                                                                (cond
                                                                 (number? val) (Long/parseLong (text e))
                                                                 (keyword? val) val
                                                                 :t (text e))))])])
                                         (dissoc @user-atom :user-handle :state
                                                 :font-preferences :_rev :_id))
                                    flatten
                                    vec)))
    (catch Exception ex
      (println "Plöp")
      (println ex))))

(defn get-credentsials
  "When value of atom of key :window-state is :ready, other atoms contain the information asked from the user. While the value is :open, the form is open, and when it is :canceled, user has canceled the login and the app should kill itself"
  []
  (let [username (atom "")
        usrnamefield (text)      
        passwordfield (password)
        password (atom "")
        
        window-state (atom :open :validator #(in? window-states %))
        
        f (frame :title "YOOL-IM (working title)"
                 :width 300
                 :height 150
                 :visible? true

                 :menubar (menubar :items [(menu :text "Options"
                                                 :items [(action :handler (fn [_]
                                                                            (edit-settings settings))
                                                                 :name "System settings")])])
                 
                 :on-close :dispose
                 :listen [:window-closed (fn [_]
                                           (if (= @window-state :open)
                                             (reset! window-state :canceled)))])
        login-event-fn (fn [_]
                         (reset! window-state :ready)
                         (-> f hide! dispose!))
        login-btn (button :text "Log in" :listen [:action-performed login-event-fn])]
    (config! f :content (vertical-panel
                         :items [(horizontal-panel :items [(label :text "Username") usrnamefield])
                                 (horizontal-panel :items [(label :text "Password") passwordfield])
                                 (horizontal-panel :items
                                                   [login-btn
                                                    (button :text "Close" :listen [:action-performed
                                                                                   (fn [_]
                                                                                     (reset! window-state :canceled)
                                                                                     (-> f hide! dispose!))])])]))
    (listen passwordfield :key-released (fn [e]
                              (let [keychar (.getKeyChar e)]
                                (when (= keychar \newline)
                                  (login-event-fn nil)))))
    (b/bind usrnamefield username)
    (b/bind passwordfield
            (b/transform sha-512)
            password)

    (comment    (doseq [a [username password window-state]]
                  (add-watch a :jee (fn [_ _ _ new-val]
                                      (println "new-val is " new-val)))))
    
                                        ;    (-> f pack!)

    {:username username
     :password password
     :window-state window-state}))
