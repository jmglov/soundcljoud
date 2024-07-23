(ns soundcljoud
  (:require [clojure.string :as str]
            [promesa.core :as p]))

(defonce state (atom nil))

(def colour-played "#ff9800")
(def colour-buffered "#ffbd52")

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

(defn add-class! [el cls]
  (-> el (.-classList) (.add cls)))

(defn remove-class! [el cls]
  (-> el (.-classList) (.remove cls)))

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

(defn init-state [{:keys [tracks] :as album}]
  (let [track-numbers (range 1 (inc (count tracks)))]
    {:album album
     :paused? true
     :shuffling? false
     :repeating? false
     :repeating-all? false
     :active-track (first track-numbers)
     :prev-tracks (list)
     :next-tracks (rest track-numbers)}))

(defn toggle-repeat [{:keys [repeating? repeating-all?] :as state}]
  (cond
    repeating-all?
    (assoc state
           :repeating? true
           :repeating-all? false)

    repeating?
    (assoc state :repeating? false)


    :else
    (assoc state :repeating-all? true)))

(defn toggle-shuffle [{:keys [album active-track shuffling?] :as state}]
  (let [num-tracks (count (:tracks album))]
    (if shuffling?
      (assoc state
             :shuffling? false
             :next-tracks (range (inc active-track) (inc num-tracks))
             :prev-tracks (range 1 active-track))
      (assoc state
             :shuffling? true
             :next-tracks (->> (range 1 (inc num-tracks))
                               (remove #(= active-track %))
                               shuffle)))))

(defn advance-track [{:keys [active-track next-tracks prev-tracks] :as state}]
  (let [next-track (first next-tracks)]
    (assoc state
           :active-track (or next-track active-track)
           :prev-tracks (cons active-track prev-tracks)
           :next-tracks (rest next-tracks))))

(defn auto-advance-track [{:keys [active-track next-tracks prev-tracks
                                  repeating? repeating-all?]
                           :as state}]
  (let [next-track (first next-tracks)]
    (cond
      repeating?
      state

      (and repeating-all? (not next-track))
      (let [prev-tracks (cons active-track prev-tracks)
            next-tracks (rest (reverse prev-tracks))
            active-track (first (reverse prev-tracks))]
        (assoc state
               :active-track active-track
               :next-tracks next-tracks
               :prev-tracks prev-tracks))

      :else
      (advance-track state))))

(defn back-track [{:keys [active-track next-tracks prev-tracks] :as state}]
  (if-let [prev-track (first prev-tracks)]
    (assoc state
           :active-track prev-track
           :prev-tracks (rest prev-tracks)
           :next-tracks (cons active-track next-tracks))
    state))

(defn move-to-track [{:keys [album active-track next-tracks prev-tracks shuffling?] :as state} n]
  (let [num-tracks (count (:tracks album))
        next-tracks (cond
                      (some #(= n %) next-tracks)
                      (->> next-tracks (drop-while #(not= n %)) rest)

                      shuffling?
                      next-tracks

                      :else
                      (range (inc n) (inc num-tracks)))]
    (assoc state
           :active-track n
           :prev-tracks (if shuffling?
                          (cons active-track prev-tracks)
                          (range 1 n))
           :next-tracks next-tracks)))

(defn ->track [item-el]
  {:artist (xml-get item-el "author")
   :title (xml-get item-el "title")
   :number (-> (xml-get item-el "episode") js/parseInt)
   :src (xml-get-attr item-el "enclosure" "url")})

(defn ->album [xml]
  {:artist (xml-get xml "author")
   :title (xml-get xml "title")
   :image (xml-get-attr xml "image" "href")
   :tracks (->> (.querySelectorAll xml "item")
                (map ->track)
                (sort-by :number))})

(defn load-album [path]
  (p/-> (fetch-xml path) ->album))

(defn get-track-duration []
  (.-duration (get-el "audio")))

(defn get-playback-position []
  (.-currentTime (get-el "audio")))

(defn set-playback-position! [ss]
  (set! (.-currentTime (get-el "audio")) ss))

(defn format-pos
  ([pos]
   (format-pos pos false))
  ([pos drop-decimal?]
   (let [hh (-> pos (/ 3600) int)
         hh-str (if (zero? hh) "" (str hh ":"))
         mm (-> pos (/ 60) (mod 60) int)
         mm-str (-> (str mm) (str/replace #"^(\d)$" "0$1") (str ":"))
         ss (mod pos 60)
         ss-str (-> (str ss)
                    (str/replace #"^(\d+)$" "$1.00")
                    (str/replace #"^(\d)[.]" "0$1.")
                    (str/replace #"[.](\d{1,2})\d*" ".$1"))
         ss-str (if drop-decimal?
                  (str/replace ss-str #"[.]\d+$" "")
                  ss-str)]
     (str hh-str mm-str ss-str))))

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

(defn format-playlist [state]
  (-> state
      (select-keys [:prev-tracks :active-track :next-tracks])
      pr-str))

(defn get-buffers []
  (let [buffered (.-buffered (get-el "audio"))]
    (->> (range (.-length buffered))
         (map (fn [i]
                [(.start buffered i)
                 (.end buffered i)])))))

(defn draw-rect! [ctx x y w h colour]
  (set! (.-fillStyle ctx) colour)
  (.fillRect ctx x y w h))

(defn display-timeline! []
  (let [canvas (get-el "canvas.timeline")
        canvas-width (.-width canvas)
        canvas-height (.-height canvas)
        ctx (.getContext canvas "2d")
        buffers (get-buffers)
        pos (get-playback-position)
        duration (get-track-duration)
        sec-width (/ canvas-width duration)
        cur-pos-x (* pos sec-width)
        cur-pos-y (/ canvas-height 2)
        cur-pos-r (/ canvas-height 2)]
    (set! (.-innerHTML (get-el "#position"))
          (str (format-pos pos true) " / " (format-pos duration true)))
    (draw-rect! ctx 0 0 canvas-width canvas-height "lightgray")
    (doseq [[start end] buffers
            :let [start-x (* start sec-width)
                  end-x (* end sec-width)
                  width (- end-x start-x)]]
      (draw-rect! ctx start-x 0 width canvas-height colour-buffered))
    (draw-rect! ctx 0 0 cur-pos-x canvas-height colour-played)
    (.beginPath ctx)
    (.arc ctx cur-pos-x cur-pos-y cur-pos-r
          0 (* js/Math.PI 2) false)
    (set! (.-fillStyle ctx) "black")
    (.fill ctx)))

(defn set-metadata! [album track]
  (set! (.-metadata js/navigator.mediaSession)
        (js/MediaMetadata. (clj->js {:title (:title track)
                                     :artist (:artist track)
                                     :album (:title album)
                                     :artwork [{:src (:image album)}]}))))

(defn activate-track! []
  (let [{:keys [album active-track paused?]} @state
        {:keys [artist title src] :as track} (-> album :tracks (nth (dec active-track)))]
    (log "Activating track:" (clj->js track))
    (let [track-spans (seq (.-children (get-el "#tracks")))
          audio-el (get-el "audio")]
      (set! (.-title audio-el) (str artist " - " title))
      (doseq [span track-spans]
        (set-styles! span "font-weight: normal;"))
      (-> track-spans
          (nth (dec active-track))
          (set-styles! "font-weight: bold;"))
      (set! (.-src audio-el) src)
      (when-not paused?
        (.play audio-el)))
    (display-timeline!)
    (set-metadata! album track)
    track))

(defn toggle-button! [id src tgt]
  (let [button (get-el id)]
    (doseq [cls ["drop-shadow" "bg" "shine"]
            p (.querySelectorAll button (str "." cls src))]
      (add-class! p (str cls tgt))
      (remove-class! p (str cls src)))))

(defn turn-off-button! [id]
  (toggle-button! id "" "-off"))

(defn turn-on-button! [id]
  (toggle-button! id "-off" ""))

(defn toggle-repeat! []
  (let [{:keys [repeating? repeating-all?]} (swap! state toggle-repeat)]
    (cond
      repeating-all?
      (do
        (log "Repeating all")
        (set-styles! "#repeat-button" "display: inline")
        (set-styles! "#repeat-one-button" "display: none")
        (turn-on-button! "#repeat-button"))

      repeating?
      (do
        (log "Repeating one")
        (set-styles! "#repeat-button" "display: none")
        (set-styles! "#repeat-one-button" "display: inline")
        (turn-off-button! "#repeat-button"))

      :else
      (do
        (log "Repeat off")
        (set-styles! "#repeat-one-button" "display: none")
        (set-styles! "#repeat-button" "display: inline")))))

(defn toggle-shuffle! []
  (let [{:keys [shuffling?]} @state]
    (if shuffling?
      (turn-off-button! "#shuffle-button")
      (turn-on-button! "#shuffle-button"))
    (swap! state toggle-shuffle))
  (let [{:keys [shuffling? next-tracks]} @state]
    (log (str "Shuffle " (if shuffling? "on" "off")
              "; playlist:")
         (format-playlist @state))))

(defn advance-track! []
  (let [{:keys [next-tracks]} @state
        next-track (first next-tracks)]
    (when next-track
      (log (str "Advancing to " (format-playback next-track 0)))
      (swap! state advance-track)
      (activate-track!)
      (log "Playlist:" (format-playlist @state)))))

(defn auto-advance-track! []
  (log (str "Ended " (format-playback)))
  (let [cur-track (:active-track @state)
        {:keys [active-track]} (swap! state auto-advance-track)]
    (if (= cur-track active-track)
      (log "Playlist:" (format-playlist @state))
      (do
        (activate-track!)
        (log (str "Advanced to track " active-track "; playlist:")
             (format-playlist @state))))))

(defn back-track! []
  (let [{:keys [active-track prev-tracks]} @state
        prev-track (first prev-tracks)
        at-start-of-track? (<= (get-playback-position) 1.0)]
    (if (and at-start-of-track? prev-track)
      (do
        (log (str "Moving back to " (format-playback prev-track 0)))
        (swap! state back-track)
        (activate-track!)
        (log "Playlist:" (format-playlist @state)))
      (do
        (log (str "Moving back to " (format-playback active-track 0)
                  "; playlist:")
             (format-playlist @state))
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

(defn fast-forward-track!
  ([]
   (let [cur-pos (get-playback-position)
         new-pos (+ cur-pos 15.0)]
     (fast-forward-track! cur-pos new-pos)))
  ([cur-pos new-pos]
   (let [buffers (get-buffers)
         [_ end] (some (fn [[start end :as buffer]]
                         (and (>= cur-pos start) (<= cur-pos end)
                              buffer))
                       buffers)
         end (or end (-> (last buffers) second))
         new-pos (min (get-track-duration) end new-pos)]
     (log (str "Fast forwarding to " (format-playback nil new-pos)))
     (set-playback-position! new-pos))))

(defn play-track! []
  (log (str "Playing from " (format-playback)))
  (.play (get-el "audio"))
  (swap! state assoc :paused? false)
  (set-styles! "#play-button" "display: none")
  (set-styles! "#pause-button" "display: inline")
  (turn-on-button! "#stop-button")
  (.focus (get-el "#pause-button"))
  (log "Playlist:" (format-playlist @state)))

(defn pause-track! []
  (log (str "Pausing at " (format-playback)))
  (.pause (get-el "audio"))
  (swap! state assoc :paused? true)
  (set-styles! "#play-button" "display: inline")
  (set-styles! "#pause-button" "display: none")
  (.focus (get-el "#play-button")))

(defn stop-track! []
  (log (str "Stopping at " (format-playback)))
  (let [audio (get-el "audio")]
    (.pause audio)
    (set! (.-currentTime audio) 0.0))
  (swap! state assoc :paused? true)
  (set-styles! "#play-button" "display: inline")
  (set-styles! "#pause-button" "display: none")
  (turn-off-button! "#stop-button")
  (.focus (get-el "#play-button")))

(defn move-to-track! [n]
  (log (str "Moving to track " n " from " (format-playback)))
  (if (= (:active-track @state) n)
    (do
      (stop-track!)
      (play-track!))
    (do
      (swap! state move-to-track n)
      (activate-track!)))
  (log "Playlist:" (format-playlist @state)))

(defn audio-event-handler
  ([]
   (audio-event-handler nil))
  ([handler]
   (fn [ev]
     (let [pos (get-playback-position)
           buffers (get-buffers)]
       (log (str (.-type ev) " at "
                 (format-playback)
                 "; buffers:")
            (pr-str buffers))
       (when handler (handler ev))
       true))))

(defn seek-handler [ev]
  (when (or (= "mouseup" (.-type ev))
            (and (= "mouseleave" (.-type ev))
                 (pos? (.-buttons ev))))
    (let [canvas (get-el "canvas.timeline")
          canvas-width (.-width canvas)
          canvas-height (.-height canvas)
          duration (get-track-duration)
          sec-width (/ canvas-width duration)
          x (.-offsetX ev)
          pos (/ x sec-width)]
      (log (str "Requesting seek to " (format-playback nil pos)))
      (fast-forward-track! pos pos))))

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

(defn init-buttons! []
  (turn-off-button! "#shuffle-button")
  (turn-off-button! "#repeat-button")
  (turn-off-button! "#stop-button")
  (set-styles! "#pause-button" "display: none")
  (set-styles! "#repeat-one-button" "display: none")
  (.focus (get-el "#play-button"))
  (add-click-handler! "shuffle" toggle-shuffle!)
  (add-click-handler! "back" back-track!)
  (add-click-handler! "rewind" rewind-track!)
  (add-click-handler! "play" play-track!)
  (add-click-handler! "pause" pause-track!)
  (add-click-handler! "stop" stop-track!)
  (add-click-handler! "fast-forward" fast-forward-track!)
  (add-click-handler! "next" advance-track!)
  (add-click-handler! "repeat" toggle-repeat!)
  (add-click-handler! "repeat-one" toggle-repeat!))

(defn init-audio! []
  (let [audio (get-el "audio")
        canvas (get-el "canvas.timeline")]
    (.addEventListener audio "ended"
                       (audio-event-handler auto-advance-track!))
    (.addEventListener audio "durationchange"
                       (audio-event-handler display-timeline!))
    (.addEventListener audio "timeupdate"
                       (audio-event-handler display-timeline!))
    (.addEventListener canvas "mouseup" seek-handler)
    (.addEventListener canvas "mouseleave" seek-handler)))

(defn track->span [{:keys [number title] :as track}]
  (let [span (js/document.createElement "span")]
    (set! (.-innerHTML span) (str number ". " title))
    (add-class! span "clickable")
    (.addEventListener span "click" (partial move-to-track! number))
    span))

(defn display-album! [{:keys [artist title image tracks] :as album}]
  (let [cover (get-el "#cover-image")
        wrapper (get-el "#wrapper")]
    (set! (.-innerHTML (get-el "#title")) (str artist " - " title))
    (set! (.-src cover) image)
    (->> tracks
         (map track->span)
         (set-children! (get-el "#tracks")))
    (set-styles! wrapper "display: flex;")
    album))

(defn load-ui! [feed]
  (p/let [album (load-album feed)]
    (reset! state (init-state album))
    (init-audio!)
    (display-album! album)
    (init-buttons!)
    (activate-track!)))

(set! (.-loadUI js/window) load-ui!)
