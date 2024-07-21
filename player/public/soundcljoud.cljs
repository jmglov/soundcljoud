(ns soundcljoud
  (:require [clojure.string :as str]
            [promesa.core :as p]))

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
  (set! (.-style (get-el el)) styles))

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

(defn display-timeline! []
  (let [audio (get-el "audio")
        canvas (get-el "#timeline")
        ctx (.getContext canvas "2d")]
    (set! (.-fillStyle ctx) "lightgray")
    (.fillRect ctx 0 0 (.-width canvas) (.-height canvas))
    (set! (.-fillStyle ctx) "green")
    (set! (.-strokeStyle ctx) "white")
    (doseq [i (range (.-length audio.buffered))
            :let [inc (/ (.-width canvas) (.-duration audio))
                  start-x (* (.start audio.buffered i) inc)
                  end-x (* (.end audio.buffered i) inc)
                  width (- end-x start-x)]]
      (.fillRect ctx start-x 0 width (.-height canvas))
      (.rect ctx start-x 0 width (.-height canvas))
      (.stroke ctx)
      [inc start-x end-x width])))

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
  (display-timeline!)
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

(defn get-track-duration []
  (.-duration (get-el "audio")))

(defn get-playback-position []
  (.-currentTime (get-el "audio")))

(defn set-playback-position! [ss]
  (set! (.-currentTime (get-el "audio")) ss))

(defn format-pos [pos]
  (let [hh (-> pos (/ 3600) int)
        hh-str (if (zero? hh) "" (str hh ":"))
        mm (-> pos (/ 60) (mod 60) int)
        mm-str (-> (str mm) (str/replace #"^(\d)$" "0$1") (str ":"))
        ss (mod pos 60)
        ss-str (-> (str ss)
                   (str/replace #"^(\d+)$" "$1.00")
                   (str/replace #"^(\d)[.]" "0$1.")
                   (str/replace #"[.](\d{1,2})\d*" ".$1"))]
    (str hh-str mm-str ss-str)))

(defn format-playback
  ([]
   (format-playback nil (get-playback-position)))
  ([num pos]
   (format-playback (or num (:active-track @state))
                    pos
                    (get-track-duration)))
  ([num pos dur]
   (str "track " num
        " [" (format-pos pos) " / " (format-pos dur) "]")))

(defn get-buffers []
  (let [buffered (.-buffered (get-el "audio"))]
    (->> (range (.-length buffered))
         (map (fn [i]
                [(.start buffered i)
                 (.end buffered i)])))))

(defn toggle-button! [id src tgt]
  (let [button (get-el id)]
    (doseq [cls ["drop-shadow" "bg" "shine"]
            p (.querySelectorAll button (str "." cls src))]
      (-> p (.-classList) (.add (str cls tgt)))
      (-> p (.-classList) (.remove (str cls src))))))

(defn turn-off-button! [id]
  (toggle-button! id "" "-off"))

(defn turn-on-button! [id]
  (toggle-button! id "-off" ""))

(defn advance-track! []
  (let [{:keys [active-track album]} @state
        {:keys [tracks]} album
        last-track? (= active-track (count tracks))]
    (when-not last-track?
      (log (str "Advancing to " (format-playback (inc active-track) 0)))
      (activate-track! (nth tracks active-track)))))

(defn back-track! []
  (let [{:keys [active-track album]} @state
        {:keys [tracks]} album
        first-track? (= active-track 1)
        at-start-of-track? (<= (get-playback-position) 1.0)]
    (if (and at-start-of-track? (not first-track?))
      (do
        (log (str "Moving back to " (format-playback (dec active-track) 0)))
        (activate-track! (nth tracks (- active-track 2))))
      (do
        (log (str "Moving back to " (format-playback active-track 0)))
        (set-playback-position! 0.0)))))

(defn rewind-track! []
  (let [cur-pos (get-playback-position)
        new-pos (- cur-pos 15.0)
        buffers (get-buffers)
        [start _] (some (fn [[start end :as buffer]]
                          (and (>= cur-pos start) (<= cur-pos end)
                               buffer))
                        buffers)
        new-pos (max 0 start new-pos)]
    (log (str "Rewinding to " (format-playback nil new-pos)))
    (set-playback-position! new-pos)))

(defn fast-forward-track! []
  (let [cur-pos (get-playback-position)
        new-pos (+ cur-pos 15.0)
        buffers (get-buffers)
        [_ end] (some (fn [[start end :as buffer]]
                        (and (>= cur-pos start) (<= cur-pos end)
                             buffer))
                      buffers)
        new-pos (min (get-track-duration) end new-pos)]
    (log (str "Fast forwarding to " (format-playback nil new-pos)))
    (set-playback-position! new-pos)))

(defn play-track! []
  (log (str "Playing from " (format-playback)))
  (.play (get-el "audio"))
  (swap! state assoc :paused? false)
  (set-styles! "#play-button" "display: none")
  (set-styles! "#pause-button" "display: inline"))

(defn pause-track! []
  (log (str "Pausing at " (format-playback)))
  (.pause (get-el "audio"))
  (swap! state assoc :paused? true)
  (set-styles! "#play-button" "display: inline")
  (set-styles! "#pause-button" "display: none"))

(defn stop-track! []
  (log (str "Stopping at " (format-playback)))
  (let [audio (get-el "audio")]
    (.pause audio)
    (set! (.-currentTime audio) 0.0))
  (swap! state assoc :paused? true)
  (set-styles! "#play-button" "display: inline")
  (set-styles! "#pause-button" "display: none"))

(defn audio-event-handler
  ([]
   (audio-event-handler nil))
  ([handler]
   (fn [ev]
     (let [pos (get-playback-position)
           buffers (get-buffers)]
       (log (str (format-playback)
                 "; buffers:")
            (pr-str buffers))
       (when handler (handler ev))
       true))))

(defn add-click-handler! [button-name f]
  (let [button-id (str "#" button-name "-button")
        button (get-el button-id)]
    (log (str "Installing click handler for " button-name)
         button)
    (.addEventListener button "click"
                       (fn [_ev]
                         (log (str button-name " clicked; "
                                   (format-playback)))
                         (f)))))

(defn load-ui! [feed]
  (p/let [album (load-album feed)
          audio (get-el "audio")]
    (display-album! album)
    (turn-off-button! "#shuffle-button")
    (turn-off-button! "#repeat-button")
    (set-styles! "#pause-button" "display: none")
    (set-styles! "#repeat-one-button" "display: none")
    (reset! state {:paused? true, :album album})
    (->> album
         :tracks
         first
         activate-track!)
    (add-click-handler! "back" back-track!)
    (add-click-handler! "rewind" rewind-track!)
    (add-click-handler! "play" play-track!)
    (add-click-handler! "pause" pause-track!)
    (add-click-handler! "stop" stop-track!)
    (add-click-handler! "fast-forward" fast-forward-track!)
    (add-click-handler! "next" advance-track!)
    (.addEventListener audio "ended" (audio-event-handler advance-track!))))

#_(load-ui! "http://localhost:1341/Garth+Brooks/Fresh+Horses/album.rss")

