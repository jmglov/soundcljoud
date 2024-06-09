(ns soundcljoud
  (:require [promesa.core :as p]))

(defn log [msg obj]
  (js/console.log msg obj))

(defn get-el [selector]
  (if (instance? js/HTMLElement selector)
    selector  ; already an element; just return it
    (js/document.querySelector selector)))

(defn set-styles! [el styles]
  (set! (.-style el) styles))

(defn append-children! [id children]
  (let [children (if (sequential? children) children [children])  ; handle single child
        el (get-el id)]
    (doseq [child children]
      (.appendChild el child))
    el))

(defn remove-children! [id]
  (let [el (get-el id)]
    (while (.-firstChild el)
      (.removeChild el (.-lastChild el)))
    el))

(defn set-children! [id children]
  (-> (remove-children! id)
      (append-children! children)))

(defn fetch-xml [path]
  (log "Fetching path:" path)
  (let [parse #(.parseFromString (js/window.DOMParser.) % "text/xml")]
    (p/-> (js/fetch (js/Request. path))
          (.text)
          parse)))

(defn xml-get [el k]
  (-> el
      (.querySelector k)
      (.-innerHTML)))

(defn xml-get-attr [el k attr]
  (-> el
      (.querySelector k)
      (.getAttribute attr)))

(defn ->track [item-el]
  {:artist (xml-get item-el "author")
   :title (xml-get item-el "title")
   :number (-> (xml-get item-el "episode") js/parseInt)
   :src (xml-get-attr item-el "enclosure" "url")})

(defn ->album [xml]
  {:title (xml-get xml "title")
   :image (xml-get-attr xml "image" "href")
   :tracks (->> (.querySelectorAll xml "item")
                (map ->track)
                (sort-by :number))})

(defn load-album [path]
  (p/-> (fetch-xml path) ->album))

(defn play-track! [{:keys [number src] :as track}]
  (log "Playing track" (clj->js track))
  (let [track-spans (seq (.-children (get-el "#tracks")))]
    (doseq [span track-spans]
      (set-styles! span "font-weight: normal;"))
    (-> track-spans
        (nth (dec number))
        (set-styles! "font-weight: bold;")))
  (set! (.-src (get-el "audio")) src))

(defn track->span [track]
  (let [span (js/document.createElement "span")
        {:keys [number title]} track]
    (set! (.-innerHTML span) (str number ". " title))
    (.addEventListener span "click" (partial play-track! track))
    span))

(defn display-tracks! [tracks]
  (->> tracks
       (map track->span)
       (set-children! "#tracks")))

(defn display-album! [{:keys [image title tracks] :as album}]
  (let [img (get-el "img")]
    (set! (.-src img) image)
    (set! (.-alt img) title))
  (let [display-title (str "Soundcljoud: " title)]
    (set! (.-innerHTML (get-el "h1")) display-title)
    (set! (.-title js/document) display-title))
  (set-styles! (get-el "#player") "display: flex; gap: 3%;")
  (display-tracks! tracks)
  album)

(defn load-ui! [base-path]
  (p/let [album (load-album (str base-path "album.rss"))]
    (display-album! album)
    (play-track! (-> album :tracks first))))

(load-ui! "/albums/Garth+Brooks/Fresh+Horses/")
