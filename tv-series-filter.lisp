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
(defgeneric filter-epi-array (time-filter show-filter episodes))

(defmethod filter-epi-array ((time-filter (eql :alles)) (show-filter (eql 'alle)) episodes)
  episodes)

(defparameter time-distance 6)

(defun time-filter (time-filter)
  (let* ((now (local-time:today))
         (week-start (local-time:timestamp- now time-distance :day))
         (week-end   (local-time:timestamp+ now time-distance :day)))
    (ecase time-filter
      (:alles (constantly t))
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
  (if (eq 'alle show)
      (constantly t)
      (lambda (x) (eq (series-id x) show))))

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
