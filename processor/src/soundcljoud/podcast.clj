(ns soundcljoud.podcast
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [selmer.parser :as selmer]
            [soundcljoud.rss :as rss]))

(defn render-tree [data opts]
  (cond
    (string? data) (selmer/render data opts)
    (vector? data) (->> data
                        (map #(render-tree % opts))
                        vec)
    (sequential? data) (->> data
                            (map #(render-tree % opts)))
    (set? data) (->> data
                     (map #(render-tree % opts))
                     set)
    (map? data) (->> data
                     (map (fn [[k v]] [k (render-tree v opts)]))
                     (into {}))
    :else data))

(defn update-soundcljoud-opts [opts]
  (-> (update-in opts [:soundcljoud :opts]
                 #(->> (render-tree % opts)
                       json/generate-string))))

(defn write-episode-page! [{:keys [episode-template] :as opts}
                                   episode]
  (let [template (slurp episode-template)
        episode-file (selmer/render (:episode-file opts)
                                    (assoc opts :episode episode))
        episode (update episode :description
                        selmer/render (assoc opts :episode episode))]
    (fs/create-dirs (fs/parent episode-file))
    (println (format "Writing episode page %s" episode-file))
    (->> (selmer/render template (-> opts
                                     (assoc :episode episode)
                                     update-soundcljoud-opts))
         (spit episode-file))))

(defn write-episode-pages!
  [{:keys [episodes base-dir out-dir] :as opts}]
  (doseq [{:keys [audio-file transcript-file path] :as episode}
          (->> episodes
               (filter #(or (:include-previews opts)
                            (not (:preview? %)))))]
    (doseq [filename [audio-file transcript-file]
            :let [src (fs/path base-dir (fs/file-name path) filename)
                  dst (format "%s%s/%s" (fs/file base-dir out-dir) path filename)]]
      (fs/create-dirs (fs/parent dst))
      (when (or (not (fs/exists? dst))
                (not= (fs/size src) (fs/size dst)))
        (println (format "Copying %s => %s" (str src) (str dst)))
        (fs/copy src dst {:replace-existing true})))
    (write-episode-page! opts episode)))

(defn write-rss-feed! [{:keys [out-dir feed-file] :as opts}]
  (let [dst (fs/file (format "%s%s" out-dir feed-file))]
    (fs/create-dirs (fs/parent dst))
    (println (format "Writing RSS feed %s" dst))
    (->> (rss/podcast-feed opts)
         (spit dst))))
