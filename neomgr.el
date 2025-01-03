;; -*- coding: utf-8; lexical-binding: t; -*-
;; Neocities manager
(defvar neomgr-auth '()) ;; "username:password"

(defun neomgr-info ()
  "Pull information about site from Neocities and display in temp buffer"
  (interactive)
  (let ((url-request-method "GET"))
    (url-retrieve (concat "https://" neomgr-auth "@neocities.org/api/info")
		  (lambda (status)
		    (let ((errp (plist-get status :error)))
		      (if errp
			  (message "Neomgr info request error: %s" errp)
			(neomgr-display-info (current-buffer))))))))

(defun neomgr-display-info (info)
  "Helper function to actually display Neocities site information"
  (switch-to-buffer info)
  (forward-paragraph) ;; We start at the beginning of the buffer
  (let ((json (cdr (assoc 'info (json-read)))))
    (with-output-to-temp-buffer "*neomgr info*"
      (princ (concat "Sitename: " (cdr (assoc 'sitename json)) "\n"))
      (princ (concat "Views: " (prin1-to-string (cdr (assoc 'views json))) "\n"))
      (princ (concat "Hits: " (prin1-to-string (cdr (assoc 'hits json))) "\n"))
      (princ (concat "Created: " (cdr (assoc 'created_at json)) "\n"))
      (princ (concat "Last updated: " (cdr (assoc 'last_updated json)) "\n"))
      (princ (concat "Tags: " (prin1-to-string (cdr (assoc 'tags json))) "\n")))) ;; Iffy but I'll leave it for now
  (kill-buffer)) ;; Remove JSON buffer

