(defpackage :tv-series-display-clim
  (:nicknames :tvs-clim)
  (:use :ol :clim :clim-lisp
        :tvs-find)
  (:export
   :tv-series-display))

(in-package :tv-series-display-clim)

(define-presentation-type date-range ())

(define-presentation-type season-number ()
  :inherit-from 'integer)

(define-presentation-method present (number (type season-number) stream view &key)
  (declare (ignore view))
  (princ (if (zerop number) "Alle" number) stream))

(define-presentation-method present (series (type tv-series) stream view &key)
  (declare (ignore view))
  (princ (series-title series) stream))

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
   (selected-series     :initform 'alle
                        :accessor selected-series)
   (selected-season     :initform 0         ; zero means all seasons
                        :accessor selected-season))
  (:panes
   (selection-pane  clim-stream-pane :width 900 :height 50
                    :display-function 'display-selection-filters)
   (series-pane     clim-stream-pane :width 320 :heigth 700
                    :display-function 'display-series-list)
   (app-pane        :application
                    :width 900 :heigth 700
                    :display-function 'display-episode-list)
   (int             :interactor :width 900 :height 100))
  (:layouts (default (vertically ()
                       (50 selection-pane)
                       (+fill+ (horizontally ()
                                 (200 series-pane)
                                 (+fill+ app-pane)))
                       (100 int)))))

(defun print-date (date &optional stream)
  (format stream
          "~2,'0D.~2,'0D.~4,'0D"
          (local-time:timestamp-day   date)
          (local-time:timestamp-month date)
          (local-time:timestamp-year  date)))

(defun display-episode-list (frame pane)
  
  (declare (ignore frame))
  (formatting-table (pane :x-spacing 25)
    ;; provide a header
    ;; TODO keep header from scrolling away
    (with-text-face (pane :bold)
     (formatting-row (pane)
       (formatting-cell (pane) (princ "Serie" pane))
       (formatting-cell (pane) (princ "Se" pane))
       (formatting-cell (pane) (princ "Ep" pane))
       (formatting-cell (pane) (princ "Titel" pane))
       (formatting-cell (pane) (princ "Erstausstrahlung" pane))))
    (loop for episode across
         (tvs-filter:filter-epi-array (selected-date-range *application-frame*)
                                      (selected-series *application-frame*)
                                      (selected-season *application-frame*)
                                      tse-data) do
         (formatting-row (pane)
           (with-slots (series-title
                        season-nr episode-nr
                        episode-title
                        air-date) episode
             (formatting-cell (pane)
               (present episode 'tv-series :stream pane))
             (formatting-cell (pane)
               (present season-nr 'season-number :stream pane))
             (formatting-cell (pane)
               (princ episode-nr pane))
             (formatting-cell (pane)
               (princ episode-title pane))
             (formatting-cell (pane)
               (print-date air-date pane)))))))

(defun display-selection-filters (frame pane)
  (declare (ignore frame))
  (formatting-table (pane :x-spacing 40)
    ;; give choices for date ranges
    (formatting-row (pane)
      (with-text-face (pane :bold)
        (formatting-cell (pane)
          (princ "Zeiträume:" pane)))
      (formatting-cell (pane)
        (dolist (range date-ranges)
          (present range 'date-range :stream pane)
          (princ "  " pane)))
      )
    ;; give choices for season-number
    (formatting-row (pane)
      (with-text-face (pane :bold)
        (formatting-cell (pane)
          (princ "Season:" pane)))
      (formatting-cell (pane)
        (dotimes (i 11) 
          (present i 'season-number :stream pane)
          (princ "  " pane))))))

(defun display-series-list (frame pane)
  (declare (ignore frame))
  ;; give choices for tv-series
  (formatting-table (pane)
    (formatting-row (pane)
      (with-text-face (pane :bold)
        (formatting-cell (pane)
          (princ "Serien:" pane))))
    (dolist (series (list* all-series tv-series-epguides))
      (formatting-row (pane)
        (formatting-cell (pane)
          (present series 'tv-series :stream pane))))))


(define-tvs-display-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-tvs-display-command (com-filter-series :name "Nach Serie filtern"
                                               :menu t)
    ((tv-series 'tv-series))
  (setf (selected-series *application-frame*)
        (identifier tv-series)))

(define-tvs-display-command (com-filter-season :name "Nach Season filtern"
                                               :menu t)
    ((season 'season-number))
  (setf (selected-season *application-frame*)
        season))

(define-tvs-display-command (com-filter-date :name "Nach Datum filtern"
                                             :menu t)
    ((date-range 'date-range))
  (setf (selected-date-range *application-frame*)
        date-range))

(define-tvs-display-command (com-download-info :name "Lade aktuelle Sendezeiten"
                                               :menu t) ()
  (sb-thread:make-thread #'download-all-episodes))

;;; translate clicks to commands
(define-presentation-to-command-translator filter-series
    (tv-series com-filter-series tvs-display)
    (object) (list object))

(define-presentation-to-command-translator filter-season
    (season-number com-filter-season tvs-display)
    (object) (list object))

(define-presentation-to-command-translator filter-date
    (date-range com-filter-date tvs-display)
    (object) (list object))

;;; the function to run the application
(defun tv-series-display ()
  (run-frame-top-level (make-instance 'tvs-display)))
