(defsystem tv-series-status
  :serial t
  :depends-on ("ol-utils"
               "cl-prevalence"
               "drakma"
               "cxml-dom"
               "css-selectors"
               "local-time")
  :components ((:file "tv-series-episodes-finder")))