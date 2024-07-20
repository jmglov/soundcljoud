(ns soundcljoud
  (:require [promesa.core :as p]))

(def state (atom nil))

(defn log
  ([msg]
   (log msg nil))
  ([msg obj]
   (if obj
     (js/console.log msg obj)
     (js/console.log msg))
   obj))

(defn get-el [selector]
  (if (instance? js/HTMLElement selector)
    selector  ; already an element; just return it
    (js/document.querySelector selector)))

(defn set-children! [el children]
  (.replaceChildren el)
  (doseq [child children]
    (.appendChild el child))
  el)

(defn set-styles! [el styles]
  (set! (.-style el) styles))

(defn parse-xml [xml-str]
  (.parseFromString (js/window.DOMParser.) xml-str "text/xml"))

(defn fetch-xml [path]
  (p/->> (js/fetch (js/Request. path))
         (.text)
         parse-xml
         (log "Fetched XML:")))

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
                (sort-by :number))
   :paused? true})

(defn load-album [path]
  (p/-> (fetch-xml path) ->album))

(defn activate-track! [{:keys [number src] :as track}]
  (log "Activating track:" (clj->js track))
  (let [track-spans (seq (.-children (get-el "#tracks")))
        audio-el (get-el "audio")
        {:keys [paused?]} @state]
    (doseq [span track-spans]
      (set-styles! span "font-weight: normal;"))
    (-> track-spans
        (nth (dec number))
        (set-styles! "font-weight: bold;"))
    (set! (.-src audio-el) src)
    (when-not paused?
      (.play audio-el)))
  (swap! state assoc :active-track number)
  track)

(defn track->span [{:keys [number title] :as track}]
  (let [span (js/document.createElement "span")]
    (set! (.-innerHTML span) (str number ". " title))
    (.addEventListener span "click" (partial activate-track! track))
    span))

(defn display-album! [{:keys [title image tracks] :as album}]
  (let [header (get-el "h1")
        cover (get-el ".cover-image > img")
        wrapper (get-el "#wrapper")]
    (set! (.-innerHTML header) title)
    (set! (.-src cover) image)
    (->> tracks
         (map track->span)
         (set-children! (get-el "#tracks")))
    (set-styles! wrapper "display: flex;")
    album))

(defn advance-track! []
  (let [{:keys [active-track album]} @state
        {:keys [tracks]} album
        last-track? (= active-track (count tracks))]
    (when-not last-track?
      (activate-track! (nth tracks active-track)))))

(defn load-ui! [dir]
  (p/let [album (load-album (str dir "/album.rss"))]
    (display-album! album)
    (reset! state {:paused? true, :album album})
    (->> album
         :tracks
         first
         activate-track!)
    (.addEventListener (get-el "audio") "play"
                       #(swap! state assoc :paused? false))
    (.addEventListener (get-el "audio") "ended"
                       advance-track!)))
