(defpackage :tv-series-display-clim
  (:nicknames :tvs-clim)
  (:use :ol :clim :clim-lisp)
  (:export))

(in-package :tv-series-display-clim)

(define-presentation-type date-range ())

(define-presentation-method presentation-typep (object (type date-range))
  (typep object '(member :alles :future :past :week)))

(define-presentation-method present
    (object (type date-range) stream view &key)
  (princ
   (ecase object
     (:alles "Alles")
     (:future "Zukünftige")
     (:past "Vergangene")
     (:week "Diese Woche"))
   stream))

(defparameter date-ranges
  (mapcar (lambda (x) (make-instance 'time-range :descriptor x))
          '(:alles :past :week :future)))


(defmethod equals ((a date-range) (b date-range))
  (eq (descriptor a) (descriptor b)))


(define-application-frame tvs-display ()
  ()
  (:panes
   (tv-series-selector (make-pane 'option-pane
                                  :items tv-series
                                  :value (first tv-series)
                                  :mode :one-of
                                  :test 'equals))
   (date-range-selector (make-pane 'option-pane
                                   :items date-ranges
                                   :value (first date-ranges)
                                   :mode :one-of
                                   :test 'equals)))
  (:layouts (default
                (vertically ()
                  (horizontally ()
                    tv-series-selector
                    date-range-selector)))))

(define-presentation-method present
    (object (type tv-series) stream (view textual-view) &key)
  (format stream "Series ~A" (title object)))

(define-presentation-method present
    (object (type date-range) stream (view textual-view) &key)
  (format stream "~:(~A~)" (descriptor object)))
