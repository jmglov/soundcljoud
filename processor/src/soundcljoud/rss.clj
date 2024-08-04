(ns soundcljoud.rss
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [selmer.parser :as selmer]
            [soundcljoud.audio :as audio])
  (:import (java.time ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(def dt-formatter
  (DateTimeFormatter/ofPattern "EEE, dd MMM yyyy HH:mm:ss xxxx"))

(defn format-dt [dt]
  (.format dt dt-formatter))

(defn now []
  (format-dt (ZonedDateTime/now java.time.ZoneOffset/UTC)))

(defn album-feed [opts album-info]
  (let [template (-> (io/resource "album-feed.rss") slurp)]
    (->> (update album-info :tracks (partial map #(update % :mp3-filename fs/file-name)))
         (merge opts {:date (now)})
         (selmer/render template))))

(defn kebab->snake [k]
  (-> k name (str/replace "-" "-")))

(defn update-episode [{:keys [src-dir] :as opts}
                      {:keys [audio-file description path] :as episode}]
  (let [filename (format "%s%s/%s" src-dir path audio-file)]
    (assoc episode
           :audio-filesize (fs/size filename)
           :duration (audio/mp3-duration filename)
           :description (selmer/render description
                                       (assoc opts :episode episode)))))

(defn podcast-feed [{:keys [podcast-feed-template] :as opts}]
  (selmer/render (slurp podcast-feed-template)
                 (-> opts
                     (assoc :datetime-now (now))
                     (update :episodes (fn [episodes]
                                         (->> episodes
                                              (filter #(or (:include-previews opts)
                                                           (not (:preview? %))))
                                              (map (partial update-episode opts))))))))
