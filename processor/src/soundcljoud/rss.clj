(ns soundcljoud.rss
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [selmer.parser :as selmer])
  (:import (java.time ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(def dt-formatter
  (DateTimeFormatter/ofPattern "EEE, dd MMM yyyy HH:mm:ss xxxx"))

(defn now []
  (.format (ZonedDateTime/now java.time.ZoneOffset/UTC) dt-formatter))

(defn album-feed [opts album-info]
  (let [template (-> (io/resource "album-feed.rss") slurp)]
    (->> (update album-info :tracks (partial map #(update % :mp3-filename fs/file-name)))
         (merge opts {:date (now)})
         (selmer/render template))))
