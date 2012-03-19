#!/bin/sh

sbcl <<EOF
(ql:quickload 'tv-series-status)
(sb-ext:save-lisp-and-die "tvs.core")
EOF
