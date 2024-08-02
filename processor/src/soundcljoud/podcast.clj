(ns soundcljoud.podcast
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [selmer.parser :as selmer]
            [soundcljoud.rss :as rss]))

(defn update-soundcljoud-opts [opts]
  (-> (update-in opts [:soundcljoud :opts]
                 #(-> (json/generate-string %)
                      (selmer/render opts)))))

(defn write-episode-page! [{:keys [episode-template] :as opts}
                                   episode]
  (let [template (slurp episode-template)
        episode-file (selmer/render (:episode-file opts)
                                    (assoc opts :episode episode))
        episode (update episode :description
                        selmer/render (assoc opts :episode episode))]
    (fs/create-dirs (fs/parent episode-file))
    (->> (selmer/render template (-> opts
                                     (assoc :episode episode)
                                     update-soundcljoud-opts))
         (spit episode-file))))

(defn write-episode-pages!
  [{:keys [episodes base-dir src-dir] :as opts}]
  (doseq [{:keys [audio-file transcript-file path] :as episode}
          (->> episodes
               (filter #(or (:include-previews? opts)
                            (not (:preview? %)))))]
    (doseq [filename [audio-file transcript-file]
            :let [src (fs/path base-dir (fs/file-name path) filename)
                  dst (format "%s%s/%s" src-dir path filename)]]
      (fs/create-dirs (fs/parent dst))
      (fs/copy src dst {:replace-existing true}))
    (write-episode-page! opts episode)))

(defn write-rss-feed! [{:keys [out-dir feed-file] :as opts}]
  (let [dst (fs/file (format "%s%s" out-dir feed-file))]
    (fs/create-dirs (fs/parent dst))
    (->> (rss/podcast-feed opts)
         (spit dst))))
