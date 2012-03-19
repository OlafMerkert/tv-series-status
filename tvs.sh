#!/bin/sh

sbcl --core tvs.core <<EOF
(tvs-gtk::tv-series-display)
(gtk:join-gtk-main)
EOF