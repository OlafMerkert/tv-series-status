(defsystem tv-series-status
  :serial t
  :depends-on ("ol-utils"
               "ol-data-utils"
               "iterate"
               "drakma"
               "cxml"
               "closure-html"
               "css-selectors"
               "cl-csv"
               "split-sequence"
               "local-time"
               "hunchentoot"
               "cl-who"
               ;;"cl-gtk2-gtk"
               ;;"cl-gtk-utils"
               "mcclim")
  :components ((:file "tv-series-episodes-finder")
               (:file "tv-series-filter")
               ;; (:file "tv-series-display-gtk")
               (:file "tv-series-display-web")
               (:file "tv-series-display-clim")))
