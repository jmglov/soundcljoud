(ns soundcljoud.main
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [soundcljoud.audio :as audio]
            [soundcljoud.discogs :as discogs]))

(defn normalise-title [title]
  (-> title
      str/lower-case
      (str/replace #"[^a-z]" "")))

(defn assoc-track-number [{:keys [tracks] :as album}
                          {:keys [title] :as track}]
  (if-let [found-track (some #(and (= (normalise-title title)
                                      (normalise-title (:title %)))
                                   %)
                               tracks)]
    (assoc track :number (Integer/parseInt (:number found-track)))
    track))

(defn album-info [dir]
  (let [tracks (->> (fs/glob dir "*.ogg")
                    (map (comp audio/track-info fs/file)))
        album-info (discogs/album-info token (first tracks))]
    (assoc album-info
           :tracks (->> tracks
                        (map #(assoc-track-number album-info %))
                        (sort-by :number)))))

(defn process-track [track tmpdir]
  (-> track
      (audio/ogg->wav tmpdir)
      (audio/wav->mp3 tmpdir)))

(defn process-album [dir]
  (let [info (album-info dir)
        tmpdir (fs/create-temp-dir {:prefix "soundcljoud."})]
    (update info :tracks (partial map #(process-track % tmpdir)))))
