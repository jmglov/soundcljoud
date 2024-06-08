(ns soundcljoud
  (:require [promesa.core :as p]))

(defn log [msg obj]
  (js/console.log msg obj))

(defn fetch-xml [path]
  (log "Fetching path:" path)
  (let [parse #(.parseFromString (js/window.DOMParser.) % "text/xml")]
    (p/-> (js/fetch (js/Request. path))
          (.text)
          parse)))

(comment

  (def base-path "/albums/Garth+Brooks/Fresh+Horses/")
  ;; => #'soundcljoud/base-path

  (def audio (js/document.querySelector "audio"))

  (set! (.-src audio) (str base-path "Garth+Brooks+-+Ireland.mp3"))

  (.-src audio)
  ;; => "/albums/Garth+Brooks/Fresh+Horses/Garth+Brooks+-+Ireland.mp3"

  (p/-> (str base-path "album.rss")
        fetch-xml
        log)

  (log "foo")

  )
