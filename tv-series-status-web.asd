(defsystem tv-series-status-web
  :depends-on ("tv-series-status"
               "hunchentoot"
               "cl-who"
               "web-utils")
  :serial t
  :components ((:file "tv-series-display-web")))
