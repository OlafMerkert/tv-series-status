#!/bin/sh

if [ $1 = general ]; then
    echo "General core"
    sbcl <<EOF
(ql:quickload '(drakma cxml local-time hunchentoot cl-who mcclim))
(sb-ext:save-lisp-and-die "general.core")
EOF
elif [ $1 = tvs ]; then
    echo "Tvs core based on general core"
    sbcl --core general.core <<EOF
(ql:quickload '(tv-series-status))
(sb-ext:save-lisp-and-die "tvs.core")
EOF
else
    echo "Tvs core"
    sbcl <<EOF
(ql:quickload '(tv-series-status))
(sb-ext:save-lisp-and-die "tvs.core")
EOF
fi
