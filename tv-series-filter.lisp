(in-package :cl-user)

(defpackage :tv-series-filter
  (:shadow :filter :sort)
  (:nicknames :tvs-filter)
  (:use :cl :ol
        :tvs-find)
  (:export
   :alle
   :filter-epi-array))

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
   (remove-unspec :initarg :remove-unspec
                  :initform nil
                  :accessor remove-unspec))
  (:documentation "only display episodes within a given time range."))

(defmethod trivial-filter-p ((filter date-filter))
  (not (or (begin filter)
           (end filter)
           (remove-unspec filter))))

(defmethod test ((filter date-filter) (episode episode))
  (with-slots (begin end remove-unspec) filter
    (with-slots (air-date) episode
      (cond ((not air-date) remove-unspec)
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

(defun make-date-filter (keyword)
  (ecase keyword
    (:alles (make-instance 'date-filter))
    (:future (make-instance 'date-filter
                            :begin (local-time:today) ; TODO early
                            :remove-unspec t
                            :sort? t))
    (:past (make-instance 'date-filter
                          :end (local-time:today) ; TODO late
                          ))
    (:week (make-instance 'date-filter
                          :begin (local-time:timestamp- (local-time:today) time-distance :day)
                          :end   (local-time:timestamp+ (local-time:today) time-distance :day)
                          :sort? t))))

;;; filtering for seasons
(defclass season-filter (sort-filter)
  ((season-nr :initarg :season-nr
              :initform nil
              :accessor season-nr))
  (:documentation "only display episodes from the selected season"))

(defmethod trivial-filter-p ((filter season-filter))
  (not (season-nr filter)))

(defmethod test ((filter season-filter) (episode episode))
  (or (not (season-nr filter))
      (= (season-nr filter)
         (season-nr episode))))

(defun make-season-filter (season-nr)
  (make-instance 'season-filter :season-nr season-nr))

;;; actual filtering work
(defun filter-epi-array (time-filter show-filter season-filter episodes)
  (let ((filters (remove-if #'trivial-filter-p
                            (list (make-date-filter time-filter)
                                  (make-show-filter show-filter)
                                  (make-season-filter season-filter)))))
    (setf episodes
          (remove-if-not
           (lambda (x)
             (every (lambda (f) (test f x)) filters))
           episodes))
    (dolist (f filters)
      (setf episodes (sort f episodes)))
    episodes))

