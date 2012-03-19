(in-package :cl-user)

(defpackage :tv-series-display-gtk
  (:nicknames :tvs-gtk)
  (:shadowing-import-from :gtk :range)
  (:use :cl :ol
        :tvs-find
        :tvs-filter
        :gtk :gdk :gobject)
  (:export))

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

(defun make-episode-store (array)
  (let ((store (make-instance 'array-list-store)))
    (bind-multi ((field series-name season-nr episode-nr title)
                 (type #1="gchararray" #2="gint" #2# #1#))
      (store-add-column store type #'field))
    (store-add-column store #1# (compose #'date->string #'air-date))
    (setf (slot-value store 'gtk::items)
          array)
    store))

(defun store-replace-all-items (store new-item-array)
  (let ((l-old (store-items-count store))
        (l-new (length new-item-array)))
    ;; signal deletion of all the rows
    (loop for i from (- l-old 1) downto 0 do
         (let ((path (make-instance 'tree-path)))
           (setf (tree-path-indices path) (list i))
          (emit-signal store "row-deleted" path)))
    ;; replace the array
    (setf (slot-value store 'gtk::items) new-item-array)
    ;; signal creation of all the new rows
    (loop for i from 0 below l-new do
         (let ((path (make-instance 'tree-path))
               (iter (make-instance 'tree-iter)))
           (setf (tree-path-indices path) (list i))
           (setf (tree-iter-stamp iter) 0
                 (tree-iter-user-data iter) i)
          (emit-signal store "row-inserted"
                       path iter)))
    l-new))

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
             :default-width 600
             :default-height 800
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
               (radio-button :label "Zukünftige"  :var select-future :toggled nil) :expand nil
               (button :label "Herunterladen" :var download-button) :expand nil)
              :expand nil
              (scrolled-window
               :hscrollbar-policy :automatic
               :vscrollbar-policy :automatic
               (tree-view :var view))))
      ;; setup radio-buttons
      (setf (radio-button-group select-past)   (radio-button-group select-alles)
            (radio-button-group select-week)   (radio-button-group select-past)
            (radio-button-group select-future) (radio-button-group select-week))
      ;; load data from persistence if nothing is there
      (unless tse-data (load-tse-data))
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

                   (store-replace-all-items
                    (tree-view-model view)                     
                    (filter-epi-array selected-range selected-show tse-data)))))
        (on-clicked download-button
          (download-all-episodes)
          (apply-filters))
        (bind-multi ((button select-alles select-past select-future select-week))
          (connect-signal button "toggled"
                          (ilambda (b)
                            (when (toggle-button-active button)
                              (apply-filters)))))
        (connect-signal show-selector "changed"
                        (ilambda (c)
                          (apply-filters))))
      ;; on closing the window, move the edits back to the lektion.
      (connect-signal
       window "destroy"
       (ilambda (w)
         (gtk:leave-gtk-main)))
      (widget-show window))))
