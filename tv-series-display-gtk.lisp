(in-package :cl-user)

(defpackage :tv-series-display-gtk
  (:nicknames :tvs-gtk)
  (:shadowing-import-from :gtk :range)
  (:use :cl :ol
        :iterate
        :tvs-find
        :tvs-filter
        :gtk :gdk :gobject
        :gtk-utils)
  (:export
   :tv-series-display))

(in-package :tvs-gtk)

;; Create an ARRAY-LIST-STORE for gtk, filled with the names of the
;; tv-series, read from a special variable.
(define-custom-store series
    ((series-title :type string))
  :initial-contents (list* (make-instance 'tv-series
                                          :identifier 'alle
                                          :series-title "Alle")
                           tv-series-epguides))


;; Create an ARRAY-LIST-STORE for gtk, with columns for the crucial
;; information of a particular episode.
(define-custom-store episodes
    ((series-title    :type string  :label "Serie")
     (season-nr       :type integer :label "Se")
     (episode-nr      :type integer :label "Ep")
     (episode-title   :type string  :label "Titel")
     (string-air-date :type string  :label "Erstausstrahlung")))

(defun string-air-date (obj)
  (date->string (air-date obj)))

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
              (h-box
               ;; Auswahl der Show
               (combo-box
                :var show-selector)
               :fill nil
               ;; Zeitraumauswahl
               ;; TODO Ausrichtung
               (radio-button :label "Alles"         :var select-alles     :toggled t)   :expand nil
               (radio-button :label "Vergangene"    :var select-past      :toggled nil) :expand nil
               (radio-button :label "Zukünftige"    :var select-future    :toggled nil) :expand nil
               (radio-button :label "Diese Woche"   :var select-week      :toggled nil) :expand nil
               (radio-button :label "Gestern"       :var select-yesterday :toggled nil) :expand nil
               (radio-button :label "Heute"         :var select-today    :toggled nil) :expand nil
               (button       :label "Herunterladen" :var download-button) :expand nil)
              :expand nil
              (scrolled-window
               :hscrollbar-policy :automatic
               :vscrollbar-policy :automatic
               (tree-view :var view))))
      ;; setup radio-buttons
      (group-radio-buttons select-alles select-past select-week select-future select-yesterday select-today)
      ;; load data from persistence if nothing is there
      (unless tse-data (load-tse-data))
      ;; setup models and their views
      (setup-combo-box 'series show-selector)
      (setup-tree-view 'episodes (make-store 'episodes tse-data) view)
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
                                               :week)
                                              ((toggle-button-active select-yesterday)
                                               :yesterday)
                                              ((toggle-button-active select-today)
                                               :today))))

                   (store-replace-all-items
                    (tree-view-model view)                     
                    (filter-epi-array selected-range selected-show 0 tse-data)))))
        (on-clicked download-button
          (funcall+thread #'download-all-episodes)
          (apply-filters))
        (bind-multi ((button select-alles select-past select-future select-week select-yesterday select-today))
          (on-toggled button
            (when (toggle-button-active button)
              (apply-filters))))
        (on-changed show-selector (apply-filters)))
      ;; on closing the window, move the edits back to the lektion.
      (default-destroy window)
      (widget-show window))))
