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

(defn html->single-line [html]
  (-> html
      (str/replace #"\n\s*<" "<")
      (str/replace #">\n\s*" ">")
      (str/replace #"\n\s+" " ")))

(defn update-episode [{:keys [out-dir description-epilogue] :as opts}
                      {:keys [audio-file description path] :as episode}]
  (let [filename (format "%s%s/%s" out-dir path audio-file)
        description (->> [description description-epilogue]
                         (map str/trim)
                         (str/join "\n")
                         html->single-line)]
    (assoc episode
           :audio-filesize (fs/size filename)
           :duration (audio/mp3-duration filename)
           :description (selmer/render description
                                       (assoc opts :episode episode)))))

(defn podcast-feed [{:keys [podcast] :as opts}]
  (let [template (-> (io/resource "podcast-feed.rss") slurp)
        sort-fn (fn [ep1 ep2]
                  (if (= (:type podcast) "Serial")
                    (compare (or (:number ep1) 0)
                             (or (:number ep2) 0))
                    (compare (or (:number ep2) 0)
                             (or (:number ep1) 0))))]
    (selmer/render template
                   (-> opts
                       (assoc :datetime-now (now))
                       (update :episodes
                               (fn [episodes]
                                 (->> episodes
                                      (sort sort-fn)
                                      (filter #(or (:include-previews opts)
                                                   (not (:preview? %))))
                                      (map (partial update-episode opts)))))))))
