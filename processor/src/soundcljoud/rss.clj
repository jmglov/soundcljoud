(ns soundcljoud.rss
  (:require [selmer.parser :as selmer]))

(defn- rfc-3339-now []
  #_(let [fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssxxx")
        now (java.time.ZonedDateTime/now java.time.ZoneOffset/UTC)]
    (.format now fmt)))

(defn rfc-3339 [yyyy-MM-dd]
  #_(let [in-fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd")
        local-date (java.time.LocalDate/parse yyyy-MM-dd in-fmt)
        fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssxxx")
        now (java.time.ZonedDateTime/of (.atTime local-date 23 59 59) java.time.ZoneOffset/UTC)]
    (.format now fmt)))

(defn safe-link [base-uri title])

(comment

  (-> (xml/sexp-as-element
       [:rss
        {:version "2.0"}
        [:channel
         [::atom/link
          {:href "https://jmglov.net/soundcljoud/albums/garth-brooks/fresh-horses.rss"
           :rel "self"
           :type "application/rss+xml"}]
         [:title "Fresh Horses"]
         [::itunes/author "Garth Brooks"]]])
      xml/indent-str)
  ;; => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<rss version=\"2.0\">\n  <channel>\n    <b:link xmlns:b=\"http://www.w3.org/2005/Atom\" href=\"https://jmglov.net/soundcljoud/albums/garth-brooks/fresh-horses.rss\" rel=\"self\" type=\"application/rss+xml\"/>\n    <title>Fresh Horses</title>\n    <f:author xmlns:f=\"http://www.itunes.com/dtds/podcast-1.0.dtd\">Garth Brooks</f:author>\n  </channel>\n</rss>\n"
  ;; => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<rss/>\n"


  )
