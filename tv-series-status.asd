(defsystem tv-series-status
  :serial t
  :depends-on ("ol-utils"
               "ol-data-utils"
               "drakma"
               "cxml-dom"
               "css-selectors"
               "local-time"
               "hunchentoot"
               "cl-who")
  :components ((:file "tv-series-episodes-finder")
               (:file "tv-series-display-gtk")
               (:file "tv-series-display-web")))
