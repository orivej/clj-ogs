(ns ogs.notifier
  (:gen-class)
  (:require http.async.client
            [seesaw core timer]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.contrib.zip-filter :as zip-filter]
            [clojure.contrib.zip-filter.xml :as zip-filter.xml]
            [clojure.java.io :as java.io]
            [net.cgrand.enlive-html :as html])
  (:use clojure.pprint))

(defmacro preferences-node
  "Return the java.util.prefs.Preferences/userRoot for the current
namespace."
  ([] `(.node (java.util.prefs.Preferences/userRoot) ~(str (ns-name *ns*))))
  ([s] `(.node (java.util.prefs.Preferences/userRoot) ~s)))

(defn- get-login []
  (.get (preferences-node "ogs") "login" ""))
(defn- get-password []
  (.get (preferences-node "ogs") "password" ""))
(defn- put-login [s]
  (.put (preferences-node "ogs") "login" s))
(defn- put-password [s]
  (.put (preferences-node "ogs") "password" s))

(def *login-url* "http://www.online-go.com/login.php")
(def *login-form*
  {:userName ""
   :validLoginAttempt "1"
   :passWord ""
   :robot "1"})
(def *mygames-url* "http://www.online-go.com/games/mygames.php")

(def *myturn-selector* [[:a.main (html/attr= :href "/games/mygames.php")]])
(def *unread-selector* [:table.toptable [:tr (html/nth-child 2)] [:td.toptableright (html/nth-child 2)]])

(def *cookies* nil)

(defn- request [& args]
  (with-open [client (http.async.client/create-client)]
    (let [response (apply http.async.client/GET client (concat args (list :cookies *cookies*)))]
      (http.async.client/await response)
      (java.io/input-stream
       (.toByteArray
        (http.async.client/body response))))))

(defn- enlive-select [resource selector]
  (html/text (first (html/select resource selector))))

(defn- login []
  (with-open [client (http.async.client/create-client)]
    (let [response (http.async.client/await
                    (http.async.client/POST client *login-url*
                                            :body (assoc *login-form*
                                                    :userName (get-login)
                                                    :passWord (get-password))))
          headers (http.async.client/headers response)
          cookies (http.async.client/cookies response)]
      (if (= "0" (get headers :content-length)) ; login successful
        (def *cookies* cookies)
        (do 
          (seesaw.core/alert "Login failed")
          false)))))
  

(defn- dialog-text [myturn unread]
  (cl-format
   false "~{~@[~&~a~]~}"
   (list
    (when (pos? myturn)
      (cl-format false "C'est à vous de jouer dans ~a partie~:p." myturn))
    (when (pos? unread)
      (cl-format false "Vous aves ~a message~:p non lus." unread)))))

(defn- show! [obj]
  (seesaw.core/show!
   (seesaw.core/pack! obj)))

(defmacro defshow [name args & body]
  `(defn ~name ~args
     (show! (do ~@body))))


(defshow display-dialog [myturn unread]
  (seesaw.core/dialog :title "OGS"
                      :option-type :yes-no-cancel
                      :content (dialog-text myturn unread)))

(declare notification-timer)
(defn- notification-callback [previous-myturn+unread]
  (.stop notification-timer)
  (.setInitialDelay notification-timer 60000)
  (when (login)
    (let [resource (html/html-resource (request *mygames-url*))]
      (let [myturn (Integer. (enlive-select resource *myturn-selector*))
            unread (Integer.
                    (nth (.split (enlive-select resource *unread-selector*)
                                 "[ ()]")
                         2))]
        (if (> (+ myturn unread) previous-myturn+unread)
          (case (display-dialog myturn unread)
            :success (do (.start notification-timer) 0)
            :no (do (.start notification-timer) (+ myturn unread))
            false)
          (do (.start notification-timer) previous-myturn+unread))))))

(def notification-timer (seesaw.timer/timer notification-callback
                                            :initial-value 0
                                            :delay 60000 :start? false))

(defshow main-window []
  (->>
   (seesaw.core/grid-panel
    :columns 2
    :hgap 5 :vgap 10
    :border 5
    :items ["Login:" (seesaw.core/text :text (get-login) :id :login)
            "Password:" (seesaw.core/text :text (get-password) :id :password)
            (seesaw.core/action :name "Start"
                                :handler
                                (fn [e]
                                  (put-login
                                   (seesaw.core/text
                                    (seesaw.core/select
                                     (seesaw.core/to-root e) [:#login])))
                                  (put-password
                                   (seesaw.core/text
                                    (seesaw.core/select
                                     (seesaw.core/to-root e) [:#password])))
                                  (notification-callback 0)))
            (seesaw.core/action :name "Exit"
                                :handler
                                (fn [e]
                                  (seesaw.core/return-from-dialog e nil)))])
   (seesaw.core/custom-dialog :title "OGS client"
                              :content)))

(defn -main [& args]
  (seesaw.core/native!)
  (main-window))