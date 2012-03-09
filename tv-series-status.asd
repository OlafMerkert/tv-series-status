(defsystem tv-series-status
  :serial t
  :depends-on ("ol-utils"
               "ol-data-utils"
               "drakma"
               "cxml-dom"
               "css-selectors"
               "local-time")
  :components ((:file "tv-series-episodes-finder")
               (:file "tv-series-display-gtk")))
