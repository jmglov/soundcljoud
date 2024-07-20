(ns soundcljoud.discogs
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def base-url "https://api.discogs.com")
(def user-agent "SoundCljoud/0.1 +https://jmglov.net")

(defn load-token
  ([]
   (load-token nil))
  ([filename]
   (when-let [f (or filename (io/resource "discogs-token.txt"))]
     (-> (slurp f) str/trim-newline))))

(defn api-get
  ([token path]
   (api-get token path {}))
  ([token path opts]
   (let [url (if (str/starts-with? path base-url)
               path
               (str base-url path))]
     (-> (http/get url
                   (merge {:headers {:User-Agent user-agent}}
                          opts))
         :body
         (json/parse-string keyword)))))

(defn search-album [token {:keys [artist album]}]
  (api-get token "/database/search"
           {:query-params {:artist artist
                           :release_title album
                           :token token}}))

(defn album-info [token {:keys [artist album] :as metadata}]
  (let [{:keys [cover_image master_url year]}
        (->> (search-album token metadata)
             :results
             first)
        {:keys [tracklist]} (api-get token master_url)]
    (->  metadata
         (merge {:link master_url
                 :image cover_image
                 :year year
                 :tracks (map (fn [{:keys [title position]}]
                                {:title title
                                 :artist artist
                                 :album album
                                 :number position
                                 :year year})
                              tracklist)})
         (dissoc :title :filename))))
