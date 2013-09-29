(in-package :cl-user)

(defpackage :tv-series-display-web
  (:nicknames :tvs-web)
  (:use :cl :ol
        :iterate
        :tvs-find
        :tvs-filter
        :cl-who)
  (:import-from :web-utils :stop-server)
  (:export
   :start-server
   :stop-server
   :start-server-and-open))

(in-package :tvs-web)

(defun start-server ()
  (web-utils:setup-static-content
   "/tv-series/style.css"
   #P"/home/olaf/Projekte/tv-series-status/style.css")
  (web-utils:start-server))

(defun start-server-and-open ()
  (start-server)
  (run-program "/usr/bin/xdg-open" "http://localhost:8080/tv-series"))

(defparameter *html-output* nil)
(setf cl-who::*indent* 4)

(defmacro with-html (&body body)
  (if (eq (first body) :top)
      `(with-html-output-to-string (*html-output* nil :prologue t :indent cl-who::*indent*)
         ,@(rest body))
      `(with-html-output (*html-output*)
         ,@body)))

(defparameter *even-row* nil)

(defmacro time-range-symbol-helper ()
  `(cond ,@(mapcar #`((string-equal time-range ,(mkstr (first a1))) ,(first a1))
                   date-filter-names)
         (t :alles)))


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
    (with-html
      :top
      (:html
       (:head
        (:title #1=(esc "TV Serien Status Monitor"))
        (:link :rel "stylesheet" :type "text/css" :href "/tv-series/style.css")
        (:script :type "text/javascript"
                 "
function selectShow(identifier) {
  document.forms[0].series.value = identifier;
  document.forms[0].submit();
}
function selectSeason(nr) {
  document.forms[0].season.value = nr;
  document.forms[0].submit();
}
"))
      
       (:body
        (:h1 #1#)
        (range-select-form series-symbol season-nr time-range-symbol)
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
             (:table :style "clear: both;"
              (:thead
               (:tr :class "even"
                    (:th :onclick "selectShow(\"ALLE\");"
                         "Serie")
                    (:th :onclick "selectSeason(\"\");"
                         "Se")
                    (:th "Ep")
                    (:th "Titel")
                    (:th "Erstausstrahlung")))
              (:tbody
               (setf *even-row* nil)
               (map nil #'episode-html-row episodes))))))))))

(defvar download-thread-active nil)

(hunchentoot:define-easy-handler (tv-series-download :uri "/tv-series/download")
    ()
  (funcall+thread (lambda ()
                    (unless download-thread-active
                      (setf download-thread-active t)
                      (download-all-episodes)
                      (setf download-thread-active nil))))
  (hunchentoot:redirect "/tv-series"))

;; TODO filter by season (using clicks)

(defun range-select-form (current-series current-season time-range)
  (with-html
    (:form
     :method "get"
     :action "/tv-series"
     (:select
      :name "series"
      :onchange "this.form.submit()"
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
     (dolist (range-spec tvs-filter:date-filter-names)
       (let ((input-id (mkstr 'time- (first range-spec))))
         (htm (:input :type "radio" :name "time-range" :value (mkstr (first range-spec))
                      :id input-id
                      :checked (if (eq time-range (first range-spec)) "checked")
                      :onchange "this.form.submit()")
              (:label :for input-id (str (second range-spec))))))
     (:input :type "submit" :value "Aktualisieren"))
    (:form
     :method "get"
     :action "/tv-series/download"
     (:input :type "submit" :value "Herunterladen"))))

(defun episode-html-row (episode)
  (with-html
    (:tr :class (if *even-row* "even" "odd")
         (:td :class "title"
              :onclick (conc "selectShow(\"" (mkstr (identifier episode)) "\");")
          (esc (series-title episode)))
         (:td :class "number"
              :onclick (conc "selectSeason(" (mkstr (season-nr episode)) ");")
          (fmt "~D" (season-nr episode)))
     (:td :class "number"
          (fmt "~2,'0D" (episode-nr episode)))
     (:td :class "title"
          (esc (episode-title episode)))
     (:td :class "date"
          (esc (date->string (air-date episode))))
     (notf *even-row*))))


(defun length>0 (sequence)
  (> (length sequence) 0))
