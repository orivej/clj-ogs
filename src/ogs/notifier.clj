(ns ogs.notifier
  (:gen-class)
  (:require http.async.client
            [seesaw core timer mig]
            [clojure.java.io :as java.io])
  (:use clojure.pprint)
  (:import [org.htmlcleaner HtmlCleaner DomSerializer XPather]
           javax.xml.xpath.XPathFactory))

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
(defn- get-hide-mainwin []
  (case (.get (preferences-node) "hide-mainwin" "true")
    "true" true
    "false" false))
(defn- put-hide-mainwin [s]
  (.put (preferences-node) "hide-mainwin" (str s)))

(def *login-url* "http://www.online-go.com/login.php")
(def *login-form*
  {:userName ""
   :validLoginAttempt "1"
   :passWord ""
   :robot "1"})
(def *mygames-url* "http://www.online-go.com/games/mygames.php")

(def *myturn-xpath* "//a[@class='main'][@href='/games/mygames.php']")
(def *unread-xpath* "//a[@class='main'][@href='/messages/inbox.php']/../../td[2]")

(def *cookies* nil)

(defn- request [& args]
  (with-open [client (http.async.client/create-client)]
    (let [response (apply http.async.client/GET client (concat args (list :cookies *cookies*)))]
      (http.async.client/await response)
      (java.io/input-stream
       (.toByteArray
        (http.async.client/body response))))))

(defn- dom [req]
  (let [cleaner (HtmlCleaner.)
        props (.getProperties cleaner)
        node (.clean cleaner req)
        serializer (DomSerializer. props)]
    (.createDOM serializer node)))

(defn- get-xpath [doc xpath]
  (-> (XPathFactory/newInstance)
      .newXPath
      (.compile xpath)
      (.evaluate doc)))

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
  

(defn- get-myturn-unread []
  (when (login)
    (let [doc (dom (request *mygames-url*))]
      (let [myturn (Integer. (get-xpath doc *myturn-xpath*))
            unread (Integer.
                    (nth (.split (get-xpath doc *unread-xpath*)
                                 "[ ()]")
                         2))]
        [myturn unread]))))

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
                      :option-type :ok-cancel
                      :content (dialog-text myturn unread)))

(declare notification-timer main-window-node)
(defn- notification-callback [previous-myturn+unread]
  (.stop notification-timer)
  (.setInitialDelay notification-timer 60000)
  (when-let [[myturn unread] (get-myturn-unread)]
    (if (> (+ myturn unread) previous-myturn+unread)
      (case (display-dialog myturn unread)
        :success (do (.start notification-timer) (apply + (get-myturn-unread)))
        (if (.isVisible main-window-node)
          false
          (.setVisible main-window-node true)))
      (do (.start notification-timer) (if (pos? previous-myturn+unread)
                                        (+ myturn unread)
                                        0)))))

(def notification-timer (seesaw.timer/timer notification-callback
                                            :initial-value 0
                                            :delay 60000 :start? false))
(defn- get-text [widget node]
  (seesaw.core/text
   (seesaw.core/select
    (seesaw.core/to-root widget) [node])))

(defn- get-selected [widget node]
  (.isSelected
   (seesaw.core/select
    (seesaw.core/to-root widget) [node])))

(defn- save-preferences [widget]
  (put-login (get-text widget :#login))
  (put-password (get-text widget :#password))
  (put-hide-mainwin (get-selected widget :#hide)))

(defshow main-window []
  (def main-window-node
    (->>
     (seesaw.mig/mig-panel
      :constraints ["wrap 2"
                    "[shrink 0] 5 [70, grow, fill]"
                    "[shrink 0]"]
      :items [["Login:"]
              [(seesaw.core/text :text (get-login) :id :login)]
              ["Password:"]
              [(seesaw.core/password :text (get-password) :id :password)]
              [(seesaw.core/checkbox :text "Hide main window" :id :hide
                                     :selected? (get-hide-mainwin))
               "span 2"]
              [(seesaw.core/action :name "Start"
                                   :handler
                                   (fn [e]
                                     (save-preferences e)
                                     (if (get-selected e :#hide)
                                       (seesaw.core/hide!
                                        (seesaw.core/to-root e)))
                                     (notification-callback 0)))]
              [(seesaw.core/action :name "Exit"
                                   :handler
                                   (fn [e]
                                     (save-preferences e)
                                     (.stop notification-timer)
                                     (seesaw.core/return-from-dialog e nil)))]])
     (seesaw.core/custom-dialog :title "OGS client"
                                :on-close :dispose
                                :content)))
  main-window-node)

(defn -main [& args]
  (seesaw.core/native!)
  (main-window))