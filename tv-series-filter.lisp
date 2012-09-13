(in-package :cl-user)

(defpackage :tv-series-filter
  (:nicknames :tvs-filter)
  (:use :cl :ol
        :tvs-find)
  (:export
   :alle
   :filter-epi-array))

(in-package :tvs-filter)


;; filtering the array
(defgeneric filter-epi-array (time-filter show-filter episodes)
  (:documentation "Filter a list of episodes by given time ranges or
  show identifier (a symbol)."))

(defmethod filter-epi-array ((time-filter (eql :alles)) (show-filter (eql 'alle)) episodes)
  episodes)

(defparameter time-distance 6
  "How many days in the past and in the future belong to the current
  week.")

(defun time-filter (time-filter)
  "TIME-FILTER must be one of :alles, :future, :past or :week.  This
function then returns a function that tests whether the AIR-DATE of
its argument is in the proper time range.  An unspecified AIR-DATE is
considered to be future."
  (let* ((now (local-time:today))
         (week-start (local-time:timestamp- now time-distance :day))
         (week-end   (local-time:timestamp+ now time-distance :day)))
    (ecase time-filter
      (:alles (values (constantly t) t))
      (:future (lambda (x) (aif (air-date x)
                                (local-time:timestamp<= now it)
                                t)))
      (:past (lambda (x) (aif (air-date x)
                                (local-time:timestamp<= it now)
                                nil)))
      (:week (lambda (x) (aif (air-date x)
                              (local-time:timestamp<= week-start it week-end)
                              nil))))))

(defun show-filter (show)
  "Return a function that compares the SERIES-ID of its argument with
SHOW.  'alle acts as a wildcard."
  (if (eq 'alle show)
      (values (constantly t) t)
      (lambda (x) (eq (identifier x) show))))

(defmethod filter-epi-array (time-filter (show-filter (eql 'alle)) episodes)
  (remove-if-not (time-filter time-filter) episodes))

(defmethod filter-epi-array (time-filter show-filter episodes)
  (let ((tf (time-filter  time-filter))
        (sf (show-filter show-filter)))
    (remove-if-not (lambda (x) (and (funcall sf x)
                                    (funcall tf x)))
                   episodes)))

;; additionally sort the episodes by date, for week and future lists
(defun time-compare (ts1 ts2)
  "Compare two timestamps stably, where NIL is considered the infinite future."
  (cond ((and (null ts1)
              (null ts2)) nil)
        ((null ts1) nil)
        ((null ts2) t)
        (t (local-time:timestamp<= ts1 ts2))))

(defmethod filter-epi-array :around ((time-filter (eql :week)) show-filter episodes)
  (stable-sort (call-next-method)
        #'time-compare :key #'air-date))

(defmethod filter-epi-array :around ((time-filter (eql :future)) show-filter episodes)
  (stable-sort (call-next-method)
        #'time-compare :key #'air-date))
