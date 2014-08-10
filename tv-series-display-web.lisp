(defpackage :tv-series-display-web
  (:nicknames :tvs-web)
  (:use :cl :ol
        :web-utils
        :bootstrap
        :iterate
        :tvs-find
        :tvs-filter
        :cl-who)
  (:export
   :start-server
   :stop-server
   :start-server-and-open
   :toggle-downloading))

(in-package :tvs-web)

(eval-when (:load-toplevel :execute)
(web-utils:register-web-application "TV Serien Status Monitor" "/tv-series"))

(defun start-server-and-open ()
  (start-server)
  (run-program "/usr/bin/xdg-open" "http://localhost:8080/tv-series"))

(defmacro time-range-symbol-helper ()
  `(cond ,@(mapcar #`((string-equal time-range ,(mkstr (first a1))) ,(first a1))
                   date-filter-names)
         (t :week)))

(hunchentoot:define-easy-handler (tv-series-display :uri "/tv-series")
    (series season time-range)
  (let* ((series-symbol (aif (find series tv-series-epguides :key (lambda (x) (mkstr (identifier x))) :test #'string-equal)
                             (identifier it)
                             'alle))
         (season-nr (if (and (length>0 season) (every #'digit-char-p season))
                        (parse-integer season) 0))
         (time-range-symbol (time-range-symbol-helper))
         (episodes (filter-epi-array time-range-symbol series-symbol
                                     season-nr tse-data) ))
    (html/document (:title #1="TV Serien Status Monitor"
                      :style "/~olaf/style.css"
                      :library :jquery
                      :library :bootstrap)
      ;; todo move adjustments to bootstrap to web-utils
      (:script :type "text/javascript"
         (str (ps:ps
                (defun select-show (identifier)
                  (setf (@ document forms 0 series value) identifier)
                  (@@ document forms 0 (submit)))
                (defun select-season (nr)
                  (setf (@ document forms 0 season value) nr)
                  (@@ document forms 0 (submit)))
                ;; after having seen the user-help messages 5 times,
                ;; hide them
                (symbol-macrolet ((cookie (@ document cookie)))
                  (bind-event document ready ()
                    (let ((current-view-count (if cookie
                                                  (parse-int cookie)
                                                  0)))
                      (incf current-view-count)
                      (setf cookie (@@ current-view-count (to-string)))
                      (when (> current-view-count 5)
                        (@@ ($ ".user-help") (hide)))))))))
      (bootstrap:navbar #1#)
      (bs-body
        (:h1 #1#)
        (:p :class "text-primary text-right"
           "Last update: " (str (aif (tvs-find:last-download-time)
                                     (ol-date-utils:print-date-and-time it)
                                     "none")))
        (range-select-form series-symbol season-nr time-range-symbol)
        (:h2 "Liste der Episoden")
        (if (length=0 episodes)
            (htm (:p :class "bg-warning user-info text-warning" "Keine Episoden gefunden .."))
            (htm
             (:p :class "bg-info text-info user-help"
                "Klicke auf einen Seriennamen, um nach der Serie zu
                 filtern. Klicke auf die Titelzelle &quot;Serie&quot;,
                 um die Filterung aufzuheben.")
             (:p :class "bg-info text-info user-help"
                "Klicke auf eine Seasonnummer, um nach der Season zu
                 filtern. Klicke auf die Titelzelle &quot;Se&quot;, um
                 die Filterung aufzuheben.")
             (:table
                :class "table table-striped table-condensed"
                (:thead
                   (:tr
                      (:th :onclick (ps:ps-inline (select-show "ALLE"))
                         "Serie")
                      (:th :onclick (ps:ps-inline (select-season ""))
                         "Se")
                      (:th "Ep")
                      (:th "Titel")
                      (:th "Erstausstrahlung")))
                (:tbody
                   (map nil #'episode-html-row episodes)))))))))

(defvar download-thread-active t
  "Set this variable to nil in order to allow downloading current
  series data.")

(defun toggle-downloading ()
  (if (notf download-thread-active)
      (princ "Downloading disabled")
      (princ "Downloading enabled"))
  (not download-thread-active))

(hunchentoot:define-easy-handler (tv-series-download :uri "/tv-series/download")
    ()
  (funcall+thread (lambda ()
                    (unless download-thread-active
                      (setf download-thread-active t)
                      (download-all-episodes)
                      (format *standard-output* "~&Updated TV series database~%")
                      (setf download-thread-active nil))))
  (hunchentoot:redirect "/tv-series"))


(defun range-select-form (current-series current-season time-range)
  (flet ((build-url (&key (series current-series) (season current-season) (range time-range))
           (web-utils:uri "" :series series :season season :time-range range)))
    (html/node
      (:form :class "inline-form"
         :method "get"
         :action "/tv-series"
         (:div :class "form-group"
            (:input :type "hidden"  :name "series" :value current-series)
            (:div :class "btn-group"
               (:button :type "button" :class "btn btn-default dropdown-toggle" :data-toggle "dropdown"
                  (esc (if (eq current-series 'alle)
                           "Alle"
                           (series-title (get-series-by-id current-series))))
                  (str " ")
                  (:span :class "caret"))
               (:ul :class "dropdown-menu" :role "menu"
                  (:li (:a :href (build-url :series "ALLE") "Alle"))
                  (dolist (series tv-series-epguides)
                    (htm (:li (:a :href (build-url :series (identifier series))
                                 (esc (series-title series)))))))))
         (:div :class "form-group"
            (:div :class "input-group"
               (:label :class "input-group-addon" :for "season" "Season:")
               (:input :type "number" :name "season"
                  :class "form-control"
                  :style "width: 4em"
                  :size 2
                  :value (if (zerop current-season) "" (mkstr current-season)))))
         (:div :class "form-group"
            (:input :type "hidden" :name "time-range" :value (str time-range))
            (:div :class "btn-group"
               (dolist (range-spec tvs-filter:date-filter-names)
                 (htm (:button :type "button"
                         :class (if (eq time-range (first range-spec))
                                    "btn btn-default active"
                                    "btn btn-default")
                         :onclick (ps:ps-inline* `(setf (ps:@ location href)
                                                        ,(build-url :range (first range-spec))))
                         (str (second range-spec))))))))
      (unless download-thread-active
        (htm (:form
                :method "get"
                :action "/tv-series/download"
                (:input :type "submit" :value "Herunterladen")))))))

(defun episode-html-row (episode)
  (html/node
    (:tr
       (:td :class "title"
          :onclick (ps:ps-inline* `(select-show ,(mkstr (identifier episode))))
          (esc (series-title episode)))
       (:td :class "number"
          :onclick (ps:ps-inline* `(select-season ,(mkstr (season-nr episode))))
          (fmt "~D" (season-nr episode)))
       (:td :class "number"
          (fmt "~2,'0D" (episode-nr episode)))
       (:td :class "title"
          (esc (episode-title episode)))
       (:td :class "date"
          (esc (date->string (air-date episode)))))))

;;; todo export the list of episodes to org-mode (with dates)
