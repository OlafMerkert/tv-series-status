(in-package :cl-user)

(defpackage :tv-series-display-gtk
  (:nicknames :tvs-gtk)
  (:shadowing-import-from :gtk :range)
  (:use :cl :ol
        :tvs-find
        :gtk :gdk :gobject))

(in-package :tvs-gtk)

(defun make-series-store ()
  (let ((store (make-instance 'array-list-store)))
    (store-add-column store "gchararray" #'cdr)
    (setf (slot-value store 'gtk::items)
          (map 'vector
               (lambda (x)
                 (cons (first x)
                       (fourth x)))
               (cons '(alle nil nil "Alle") tv-series-wp)))
    store))

(defun make-episode-store (&optional (array tse-data))
  (let ((store (make-instance 'array-list-store)))
    (bind-multi ((field series-name season-nr episode-nr title)
                 (type #1="gchararray" #2="gint" #2# #1#))
      (store-add-column store type #'field))
    (store-add-column store #1# (compose #'date->string #'air-date))
    (setf (slot-value store 'gtk::items)
          array)
    store))

;; todo put into ol-utils sometime
(defmacro on-clicked (button &body body)
  `(connect-signal ,button "clicked"
                   (ilambda (b) ,@body)))

(defun add-tree-view-column (view title col-index)
  (let ((column   (make-instance 'tree-view-column :title title))
        (renderer (make-instance 'cell-renderer-text)))
    (tree-view-column-pack-start     column renderer)
    (tree-view-column-add-attribute  column renderer "text" col-index)
    (tree-view-append-column view    column)))

(defun add-cell-layout-column (view col-index)
  (let ((renderer (make-instance 'cell-renderer-text)))
    (cell-layout-pack-start      view renderer)
    (cell-layout-add-attribute  view renderer "text" col-index)))

(defun tv-series-display ()
  (within-main-loop
    (let-ui (gtk-window
             :type :toplevel
             :title "TV Serien Status Monitor"
             :default-width 800
             :default-height 600
             :var window
             (v-box
              (h-button-box
               ;; Auswahl der Show
               (combo-box
                :var show-selector)
               :fill nil
               ;; Zeitraumauswahl
               ;; TODO Ausrichtung
               (radio-button :label "Alles"       :var select-alles  :toggled t)   :expand nil
               (radio-button :label "Vergangene"  :var select-past   :toggled nil) :expand nil
               (radio-button :label "Diese Woche" :var select-week   :toggled nil) :expand nil
               (radio-button :label "Zukünftige"  :var select-future :toggled nil) :expand nil)
              :expand nil
              (h-button-box :x-align 0.1 ; TODO links ausgerichtete knöpfe
                            (button :label "Herunterladen" :var download-button) :expand nil
                            (button :label "Aktualisieren" :var refresh-button)  :expand nil)
              :expand nil
              (scrolled-window
               :hscrollbar-policy :automatic
               :vscrollbar-policy :automatic
               (tree-view :var view))))
      ;; setup radio-buttons
      (setf (radio-button-group select-past)   (radio-button-group select-alles)
            (radio-button-group select-week)   (radio-button-group select-past)
            (radio-button-group select-future) (radio-button-group select-week))
      ;; setup models and their views
      (setf (combo-box-model show-selector) (make-series-store))
      (setf (tree-view-model view) (make-episode-store tse-data))
      (bind-multi ((col-title "Serie" "Se" "Ep" "Titel" "Erstausstrahlung")
                   (col-nr 0 1 2 3 4))
        (add-tree-view-column view col-title col-nr))
      (add-cell-layout-column show-selector 0)
      ;; select the first entry
      (setf (combo-box-active show-selector) 0)
      ;; button action
      (labels ((apply-filters ()
                 (let* ((selected-show-index (combo-box-active show-selector))
                        (selected-show (if (zerop selected-show-index)
                                           'alle
                                           (first (nth (max 0 (- selected-show-index 1)) tv-series-wp))))
                        (selected-range (cond ((toggle-button-active select-alles)
                                               :alles)
                                              ((toggle-button-active select-past)
                                               :past)
                                              ((toggle-button-active select-future)
                                               :future)
                                              ((toggle-button-active select-week)
                                               :week))))

                   (setf
                    (slot-value (tree-view-model view) 'gtk::items)                     
                    (filter-epi-array selected-range selected-show tse-data)))))
        (on-clicked download-button
          (download-all-episodes)
          (apply-filters))
        (on-clicked refresh-button
          (apply-filters)))
      ;; on closing the window, move the edits back to the lektion.
      (connect-signal
       window "destroy"
       (ilambda (w)
         (leave-gtk-main)))
      (widget-show window))))

;; filtering the array
(defgeneric filter-epi-array (time-filter show-filter episodes))

(defmethod filter-epi-array ((time-filter (eql :alles)) (show-filter (eql 'alle)) episodes)
  episodes)

(defun time-filter (time-filter)
  (let* ((now (local-time:today))
         (week-start (local-time:timestamp- now 4 :day))
         (week-end   (local-time:timestamp+ now 4 :day)))
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
