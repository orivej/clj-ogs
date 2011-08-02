(ns ogs.core
  (:require clojure.contrib.http.agent
            clojure.contrib.miglayout)
  (:use seesaw.core
        [clojure.contrib.pprint :only (cl-format)]))

(defn- dialog-text [myturn unread]
  (clojure.string/join
   \newline
   (when (pos? myturn)
     (cl-format false "C'est à vous de jouer dans ~a partie~:p." myturn))
   (when (pos? unread)
     (cl-format false "Vous aves ~a message~:p non lus." unread))))

(defn- display-dialog [myturn unread]
  (-> (dialog :title "OGS"
              :option-type :ok-cancel
              :content (dialog-text myturn unread))
      pack!
      show!))

(defn -main [& args]
  (display-dialog 0 0))