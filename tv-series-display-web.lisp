(in-package :cl-user)

(defpackage :tv-series-display-web
  (:nicknames :tvs-web)
  (:use :cl :ol
        :web-utils
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

(setup-static-content
   "/tv-series/style.css"
   #P"/home/olaf/Projekte/tv-series-status/style.css")

(defun start-server-and-open ()
  (start-server)
  (run-program "/usr/bin/xdg-open" "http://localhost:8080/tv-series"))

(defparameter *even-row* nil)

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
                           :style "/tv-series/style.css")
      (:script :type "text/javascript"
               (str (ps:ps
                      (defun select-show (identifier)
                        (setf (ps:@ document forms 0 series value) identifier)
                        (@@ documents forms 0 (submit)))
                      (defun select-season (nr)
                        (setf (ps:@ document forms 0 season value) nr)
                        (@@ document forms 0 (submit))))))
      (:h1 #1#)
      (range-select-form series-symbol season-nr time-range-symbol)
      (:p :class "last-update"
          "Last update: " (str (aif (tvs-find:last-download-time)
                                    (ol-date-utils:print-date-and-time it)
                                    "none")))
      (:h2 "Liste der Episoden")
      (if (length=0 episodes)
          (htm (:p :class "notfound" "Keine Episoden gefunden .."))
          (htm
           (:p :class "help"
               "Klicke auf einen Seriennamen, um nach der Serie zu
                 filtern. Klicke auf die Titelzelle &quot;Serie&quot;,
                 um die Filterung aufzuheben.")
           (:p :class "help"
               "Klicke auf eine Seasonnummer, um nach der Season zu
                 filtern. Klicke auf die Titelzelle &quot;Se&quot;, um
                 die Filterung aufzuheben.")
           (:table :class "episodes" :style "clear: both;"
                   (:thead
                    (:tr :class "even"
                         (:th :onclick (ps:ps-inline (select-show "ALLE"))
                              "Serie")
                         (:th :onclick (ps:ps-inline (select-season ""))
                              "Se")
                         (:th "Ep")
                         (:th "Titel")
                         (:th "Erstausstrahlung")))
                   (:tbody
                    (setf *even-row* nil)
                    (map nil #'episode-html-row episodes))))))))

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
  (html/node
    (:form
     :method "get"
     :action "/tv-series"
     (:select
      :name "series"
      :onchange #2=(ps:ps-inline (@@ this form (submit)))
      :size 3
      (:option :value "alle" "Alle")
      (dolist (series tv-series-epguides)
        (htm (:option :value (identifier series)
                      :selected (if (eq (identifier series) current-series)
                                    "selected")
                      (esc (series-title series))))))
     (:label :for "season" "Season:")
     (:input :type "text" :name "season"
             :size 2
             :value (if (zerop current-season) "" (mkstr current-season)))
     (:table
      :class "form"
      (dolist (row (splitn tvs-filter:date-filter-names 3))
        (htm (:tr
              (dolist (range-spec row)
                (let ((input-id (mkstr 'time- (first range-spec))))
                  (htm (:td
                        (:input :type "radio" :name "time-range"
                                :value (mkstr (first range-spec))
                                :id input-id
                                :checked (if (eq time-range (first range-spec)) "checked")
                                :onchange #2#))
                       (:td
                        (:label :for input-id (str (second range-spec)))))))))))
     (:input :type "submit" :value "Aktualisieren"))
    (unless download-thread-active
      (htm (:form
            :method "get"
            :action "/tv-series/download"
            (:input :type "submit" :value "Herunterladen"))))))

(defun episode-html-row (episode)
  (html/node
    (:tr :class (if *even-row* "even" "odd")
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
          (esc (date->string (air-date episode))))
     (notf *even-row*))))

;;; todo export the list of episodes to org-mode (with dates)
