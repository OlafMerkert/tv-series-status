(defsystem tv-series-status
  :serial t
  :depends-on ("ol-utils"
               "ol-data-utils"
               "drakma"
               "cxml"
               "closure-html"
               "css-selectors"
               "cl-csv"
               "local-time"
               "hunchentoot"
               "cl-who"
               "cl-gtk2-gtk")
  :components ((:file "tv-series-episodes-finder")
               (:file "tv-series-filter")
               (:file "tv-series-display-gtk")
               (:file "tv-series-display-web")))
