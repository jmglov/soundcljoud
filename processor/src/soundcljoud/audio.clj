(ns soundcljoud.audio
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(defn ogg->wav [{:keys [filename] :as track} tmpdir]
  (let [out-filename (fs/file tmpdir (str/replace (fs/file-name filename)
                                                  ".ogg" ".wav"))]
    (println (format "Converting %s -> %s" filename out-filename))
    (p/shell "oggdec" "-o" out-filename filename)))

(defn wav->mp3 [{:keys [filename artist album title year track]} tmpdir]
  (let [wav-file (fs/file tmpdir
                          (-> (fs/file-name filename)
                              (str/replace #"[.][^.]+$" ".wav")))
        mp3-file (str/replace wav-file ".wav" ".mp3")
        ffmpeg-args ["ffmpeg" "-i" wav-file "-vn" "-q:a" "2" mp3-file]
        id3v2-args ["id3v2" "-a" artist "-A" album "-t" title "-y" year "-T" track
                    mp3-file]]
    (println (format "Converting %s -> %s" wav-file mp3-file))
    (apply println (map str ffmpeg-args))
    (apply p/shell ffmpeg-args)
    (println "Writing ID3 tag")
    (apply println id3v2-args)
    (apply p/shell (map str id3v2-args))))

(defn track-info [filename]
  (->> (p/shell {:out :string} "vorbiscomment" filename)
       :out
       str/split-lines
       (map #(let [[k v] (str/split % #"=")] [(keyword k) v]))
       (into {})
       (merge {:filename filename})))
