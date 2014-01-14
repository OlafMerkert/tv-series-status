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
               "clon")
  :components ((:file "tv-series-episodes-finder")
               (:file "tv-series-filter")))
