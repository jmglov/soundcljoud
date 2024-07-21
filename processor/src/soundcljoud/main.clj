(ns soundcljoud.main
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [soundcljoud.audio :as audio]
            [soundcljoud.discogs :as discogs]
            [soundcljoud.rss :as rss]))

(defn normalise-title [title]
  (-> title
      str/lower-case
      (str/replace #"[^a-z]" "")))

(defn album-info [{:keys [token]} dir]
  (let [tracks (->> (fs/glob dir "*.ogg")
                    (map (comp audio/track-info fs/file)))
        track-filename (->> tracks
                            (map (fn [{:keys [filename title]}]
                                   [(normalise-title title) filename]))
                            (into {}))
        token (or token (discogs/load-token))
        album-info (discogs/album-info token (first tracks))]
    (update album-info :tracks
            (fn [ts]
              (->> ts
                   (map (fn [{:keys [number title] :as track}]
                          (assoc track
                                 :number (Integer/parseInt number)
                                 :filename (track-filename (normalise-title title)))))
                   (sort-by :number))))))

(defn process-track [track tmpdir]
  (-> track
      (audio/ogg->wav tmpdir)
      (audio/wav->mp3 tmpdir)))

(defn process-album [opts dir]
  (let [info (album-info opts dir)
        tmpdir (fs/create-temp-dir {:prefix "soundcljoud."})
        info (update info :tracks (partial map #(process-track % tmpdir)))]
    (spit (fs/file tmpdir "album.rss") (rss/album-feed opts info))
    (assoc info :out-dir tmpdir)))
