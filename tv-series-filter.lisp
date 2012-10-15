(in-package :cl-user)

(defpackage :tv-series-filter
  (:shadow :filter :sort)
  (:nicknames :tvs-filter)
  (:use :cl :ol
        :ol-date-utils
        :tvs-find)
  (:export
   :alle
   :filter-epi-array
   :date-filter-name
   :date-filter-names))

(in-package :tvs-filter)

(defclass sort-filter ()
  ((sort? :initarg :sort?
          :initform nil
          :accessor sort?))
  (:documentation "abstract class for filter objects that encourage sorting."))

(defgeneric test (filter object)
  (:documentation "test whether the OBJECT falls through the FILTER,
  return t if it is to be kept."))

(defgeneric trivial-filter-p (filter)
  (:documentation "return t when FILTER would not remove any objects."))

(defgeneric sort (filter object-seq)
  (:documentation "sort the object-seq according to the sort rule of the filter."))

(defmethod sort (filter object-seq)
  object-seq)

;;; filtering for shows
(defclass show-filter (sort-filter)
  ((show :initarg :show
         :accessor show))
  (:documentation "only display episodes from one show"))

(defmethod trivial-filter-p ((filter show-filter))
  (eq (show filter) 'alle))

(defmethod test ((filter show-filter) (episode episode))
  (or (eq (show filter) 'alle)
      (eq (show filter) (identifier episode))
      ;; TODO support for tv-series instance instead of identifier symbols
      ))

(defun make-show-filter (show)
  (make-instance 'show-filter :show show))


;;; filtering for dates
(defclass date-filter (sort-filter)
  ((begin          :initarg :begin
                   :initform nil
                   :accessor begin)
   (end            :initarg :end
                   :initform nil
                   :accessor end)
   (keep-unspec :initarg :keep-unspec
                  :initform nil
                  :accessor keep-unspec))
  (:documentation "only display episodes within a given time range."))

(defmethod trivial-filter-p ((filter date-filter))
  (not (or (begin filter)
           (end filter)
           (keep-unspec filter))))

(defmethod test ((filter date-filter) (episode episode))
  (with-slots (begin end keep-unspec) filter
    (with-slots (air-date) episode
      (cond ((not air-date) keep-unspec)
            ((and begin end) (local-time:timestamp<= begin air-date end))
            (end (local-time:timestamp<= air-date end))
            (begin (local-time:timestamp<= begin air-date))
            (t t)))))

(defun time-compare (ts1 ts2)
  "Compare two timestamps stably, where NIL is considered the infinite future."
  (cond ((and (null ts1)
              (null ts2)) nil)
        ((null ts1) nil)
        ((null ts2) t)
        (t (local-time:timestamp<= ts1 ts2))))

(defmethod sort ((filter date-filter) object-seq)
  (if (sort? filter)
      (stable-sort object-seq #'time-compare :key #'air-date)
      object-seq))

(defparameter time-distance 6
  "How many days in the past and in the future belong to the current
  week.")

(defparameter date-filter-names
  (list (list :alles "Alles" )
        (list :future "Zukünftige"
              :begin (from-today 0 nil)
              :keep-unspec t
              :sort? t)
        (list :past "Vergangene"
              :end (from-today 0 t))
        (list :week "Diese Woche"
              :begin (from-today (- time-distance))
              :end   (from-today time-distance t)
              :sort? t)
        (list :yesterday "Gestern"
              :begin (from-today -1)
              :end (from-today 0)
              :keep-unspec t)))

(defun date-filter-name (keyword)
  (second (assoc keyword date-filter-names)))

(defun make-date-filter (keyword)
  (apply #'make-instance 'date-filter
         (cddr (assoc keyword date-filter-names))))

;;; filtering for seasons
(defclass season-filter (sort-filter)
  ((season-nr :initarg :season-nr
              :initform 0
              :accessor season-nr))
  (:documentation "only display episodes from the selected season"))

(defmethod trivial-filter-p ((filter season-filter))
  "0 means all seasons."
  (zerop (season-nr filter)))

(defmethod test ((filter season-filter) (episode episode))
  (or (zerop (season-nr filter))
      (= (season-nr filter)
         (season-nr episode))))

(defun make-season-filter (season-nr)
  (make-instance 'season-filter :season-nr season-nr))

;;; actual filtering work
(defun filter-epi-array (time-filter show-filter season-filter episodes)
  (let ((filters (remove-if #'trivial-filter-p
                            (list (make-date-filter   time-filter)
                                  (make-show-filter   show-filter)
                                  (make-season-filter season-filter)))))
    (setf episodes
          (remove-if-not
           (lambda (x)
             (every (lambda (f) (test f x)) filters))
           episodes))
    (dolist (f filters)
      (setf episodes (sort f episodes)))
    episodes))
