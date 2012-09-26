#!/bin/sh

sbcl --core tvs.core <<EOF
(tvs-clim:tv-series-display)
EOF
