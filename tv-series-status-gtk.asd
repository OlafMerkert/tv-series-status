#|(defsystem tv-series-status-gtk
  :depends-on ("tv-series-status"
               "cl-gtk2-gtk"
               "cl-gtk-utils")
  :serial t
  :components ((:file "tv-series-display-gtk")))|#
