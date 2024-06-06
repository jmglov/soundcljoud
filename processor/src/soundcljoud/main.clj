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
    (assoc track :track (Integer/parseInt (:track found-track)))
    track))

(defn process-album [dir]
  (let [tracks (->> (fs/glob dir "*.ogg")
                    (map (comp audio/track-info fs/file)))
        album-info (discogs/album-info token (first tracks))
        album-info (assoc album-info
                          :tracks (->> tracks
                                       (map #(assoc-track-number album-info %))
                                       (sort-by :track)))
        tmpdir (fs/create-temp-dir)]
    (doseq [track (:tracks album-info)]
      (audio/ogg->wav track tmpdir)
      (audio/wav->mp3 track tmpdir))))
