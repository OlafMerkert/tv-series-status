(defpackage :tv-series-display-clim
  (:nicknames :tvs-clim)
  (:use :ol :clim :clim-lisp)
  (:export))

(in-package :tv-series-display-clim)

(defclass tv-series ()
  ((identifier :initarg :identifier
               :accessor identifier)
   (title      :initarg :title
               :initform ""
               :accessor title))
  (:documentation "TODO"))

(defmethod equals (a b)
  nil)

(defmethod equals ((a tv-series) (b tv-series))
  (eq (identifier a) (identifier b)))

(defparameter tv-series
  (list*
   (make-instance 'tv-series
                  :identifier 'all
                  :title "Alle")
   (mapcar (lambda (x)
             (make-instance 'tv-series
                            :identifier (first x)
                            :title (second x)))
           tvs-find:tv-series-epguides)))

(defclass date-range ()
  ((descriptor :initarg :descriptor
               :initform :alles
               :accessor descriptor))
  (:documentation "TODO"))


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
