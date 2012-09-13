(in-package :cl-user)

(defpackage :tv-series-display-gtk
  (:nicknames :tvs-gtk)
  (:shadowing-import-from :gtk :range)
  (:use :cl :ol
        :tvs-find
        :tvs-filter
        :gtk :gdk :gobject)
  (:export
   :tv-series-display))

(in-package :tvs-gtk)

(defun make-series-store ()
  "Create an ARRAY-LIST-STORE for gtk, filled with the names of the
tv-series, read from a special variable."
  (let ((store (make-instance 'array-list-store)))
    (store-add-column store "gchararray" #'series-title)
    (setf (slot-value store 'gtk::items)
          (list->array
           (list* (make-instance 'tv-series :identifier 'alle :series-title "Alle")
                  tv-series-epguides)))
    store))

(defun make-episode-store (array)
  "Create an ARRAY-LIST-STORE for gtk, with columns for the crucial
information of a particular episode."
  (let ((store (make-instance 'array-list-store)))
    (bind-multi ((field series-title season-nr episode-nr episode-title)
                 (type #1="gchararray" #2="gint" #2# #1#))
      (store-add-column store type #'field))
    (store-add-column store #1# (compose #'date->string #'air-date))
    (setf (slot-value store 'gtk::items)
          array)
    store))

(defun store-replace-all-items (store new-item-array)
  "Replace the backing array of an ARRAY-LIST-STORE with
NEW-ITEM-ARRAY and send signals for the deletion of all previous
entries, and signals for the insertion of all the new entries."
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
(defmacro define-signals (&rest signal-names)
  `(progn
     ,@(mapcar
        (lambda (signal-name)
          `(defmacro ,(symb 'on- signal-name) (object &body body)
             `(connect-signal ,object ,',(format nil "~(~A~)" signal-name)
                              (ilambda (event) ,@body))))
        signal-names)))

(define-signals clicked toggled changed destroy)


(defun add-tree-view-column (view title col-index)
  "Properly add a text column to the tree-view"
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
  "Display a graphical interface for downloading and filtering
episodes information.  It is possible to filter for series name and
date range, only listing past, future or episodes from this week."
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
                                           (identifier (elt tv-series-epguides
                                                            (max 0 (- selected-show-index 1))))))
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
          (sb-thread:make-thread #'download-all-episodes)
          (apply-filters))
        (bind-multi ((button select-alles select-past select-future select-week))
          (on-toggled button
            (when (toggle-button-active button)
              (apply-filters))))
        (on-changed show-selector (apply-filters)))
      ;; on closing the window, move the edits back to the lektion.
      (on-destroy window (gtk:leave-gtk-main))
      (widget-show window))))
