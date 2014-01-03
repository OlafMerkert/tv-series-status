(defsystem tv-series-status-clim
  :depends-on ("tv-series-status"
               "mcclim")
  :serial t
  :components ((:file "tv-series-display-clim")))
