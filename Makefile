empty :

all : general.core tvs.core

general.core : /usr/bin/sbcl
	./build-core.sh general

tvs.core : general.core tv-series-episodes-finder.lisp tv-series-filter.lisp tv-series-display-gtk.lisp tv-series-display-web.lisp
	./build-core.sh tvs

update :
	git pull

run : tvs.core
	./tvs.sh

# should not actually be needed
clean :
	rm general.core tvs.core
