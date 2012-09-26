(defpackage :tv-series-display-clim
  (:nicknames :tvs-clim)
  (:use :ol :clim :clim-lisp
        :tvs-find)
  (:export))

(in-package :tv-series-display-clim)

(define-presentation-type date-range ())

(define-presentation-type season-number ())

(defparameter date-ranges 
  '(:alles :past :week :future))

(define-presentation-method present
    (object (type date-range) stream view &key)
  (princ
   (ecase object
     (:alles "Alles")
     (:future "Zukünftige")
     (:past "Vergangene")
     (:week "Diese Woche"))
   stream))

(define-application-frame tvs-display ()
  ((selected-date-range :initform :alles
                        :accessor selected-date-range)
   (selected-series :initform 'alle
                    :accessor selected-series)
   (selected-season :initform 0         ; zero means all seasons
                    :accessor selected-season))
  (:panes
   (selection-pane clim-stream-pane :width 900 :height 50
                   :display-function 'display-selection-filters)
   (app-pane :application
             :width 900 :heigth 600
             :display-function 'display-episode-list)
   (int :interactor :width 900 :height 200))
  (:layouts (default (vertically ()
                       selection-pane
                       app-pane
                       int))))

(defun print-date (date &optional stream)
  (format stream
          "~2,'0D.~2,'0D.~4,'0D"
          (local-time:timestamp-day   date)
          (local-time:timestamp-month date)
          (local-time:timestamp-year  date)))

(defun display-episode-list (frame pane)
  (declare (ignore pane))
  (let ((stream (frame-standard-output frame)))
    (formatting-table (stream :x-spacing 25)
      ;; TODO provide a header
      ;; TODO also apply filters
      (loop for episode across
           (tvs-filter:filter-epi-array (selected-date-range *application-frame*)
                                        (selected-series *application-frame*)
                                        (selected-season *application-frame*)
                                        tse-data) do
           (formatting-row (stream)
             (with-slots (series-title
                          season-nr episode-nr
                          episode-title
                          air-date) episode
               ;; TODO enrich with presentations
               (formatting-cell (stream)
                 (with-output-as-presentation (stream episode 'tv-series)
                   (princ series-title stream)))
               (formatting-cell (stream)
                 (with-output-as-presentation (stream season-nr 'season-number)
                   (princ season-nr stream)))
               (formatting-cell (stream)
                 (princ episode-nr stream))
               (formatting-cell (stream)
                 (princ episode-title stream))
               (formatting-cell (stream)
                 (print-date air-date stream))))))))

(defun display-selection-filters (frame pane)
  (declare (ignore frame))
  (formatting-table (pane :x-spacing 30)
    (formatting-row (pane)
      ;; give choices for date ranges
      (dolist (range date-ranges)
        (formatting-cell (pane)
          (present range 'date-range :stream pane))))
    (formatting-row (pane)
      ;; give choices for tv-series
      (dolist (series (list* all-series tv-series-epguides))
        (formatting-cell (pane)
          (with-output-as-presentation (pane series 'tv-series)
            (princ (series-title series) pane)))))
    (formatting-row (pane)
      ;; give choices for season-number
      (formatting-cell (pane)
        (with-output-as-presentation (pane 0 'season-number)
          (princ "Alle" pane)))
      (dotimes+ (i 1 11) ()
          (formatting-cell (pane)
            (with-output-as-presentation (pane i 'season-number)
              (format pane "~D" i)))))))


(defun tv-series-display ()
  (run-frame-top-level (make-instance 'tvs-display)))

(define-tvs-display-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-tvs-display-command (com-series-filter :name "Nach Serie filtern"
                                               :menu t)
    ((tv-series 'tv-series))
  (setf (selected-series *application-frame*)
        (identifier tv-series)))

(define-tvs-display-command (com-series-season :name "Nach Season filtern"
                                               :menu t)
    ((season 'season-number))
  (setf (selected-season *application-frame*)
        season))

(define-tvs-display-command (com-series-date :name "Nach Datum filtern"
                                             :menu t)
    ((date-range 'date-range))
  (setf (selected-date-range *application-frame*)
        date-range))

(define-presentation-method present (series (type tv-series) stream view &key)
  (declare (ignore view))
  (princ (series-title series) stream))

;;; translate clicks to commands
(define-presentation-to-command-translator filter-series
    (tv-series com-series-filter tvs-display)
    (object) (list object))

(define-presentation-to-command-translator filter-season
    (season-number com-season-filter tvs-display)
    (object) (list object))

(define-presentation-to-command-translator filter-date
    (date-range com-series-date tvs-display)
    (object) (list object))