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

(defun neomgr-list ()
  "List files sitting on Neocities site"
  (interactive)
  (let ((url-request-method "GET"))
    (url-retrieve (concat "https://" neomgr-auth "@neocities.org/api/list")
		  (lambda (status)
		    (let ((errp (plist-get status :error)))
		      (if errp
			  (message "Neomgr listing request error: %s" errp)
			(neomgr-display-list (current-buffer))))))))

(defun neomgr-display-list (list)
  "Helper function to display remote files"
  (switch-to-buffer list)
  (forward-paragraph)
  (let ((filevec (cdr (assoc 'files (json-read)))))
    (with-output-to-temp-buffer "*neomgr file listing*"
      (mapcar (lambda (x) (princ (concat x "\n"))) (mapcar (lambda (x) (neomgr-file-details x)) filevec))))
  (kill-buffer))

(defun neomgr-file-details (f)
  "Get file details from alist f"
  (cdr (assoc 'path x)))

(defun neomgr-upload ()
  "Upload file to Neocities (interactive)"
  (interactive)
  (let* ((f (read-file-name "File: ")))
    (neomgr-file-upload f)))

(defun neomgr-file-upload (f)
  "Upload file f to Neocities (non-interactive)"
  (let* ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "multipart/form-data; boundary=----Boundary")))
	(url-request-data
	 (encode-coding-string
	  (with-temp-buffer
	    (insert "------Boundary\r\n")
	    (insert (format "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n" (file-name-nondirectory f) f))
	    (insert "Content-Type: application/octet-stream\r\n\r\n")
	    (let ((coding-system-for-read 'no-conversion))
	      (insert-file-contents-literally f))
	    (end-of-buffer)
	    (insert "\r\n------Boundary--\r\n")
	    (buffer-string)) 'us-ascii)))
    (url-retrieve (concat "https://" neomgr-auth "@neocities.org/api/upload")
		  (lambda (status)
		    (let ((errp (plist-get status :error)))
		      (if errp
			  (message "Could not upload file: %s" errp)
			(message "Successfully uploaded file")))))))

(defun neomgr-collect-links ()
  "Grab all the links contained in an org file and display in a buffer"
  (interactive)
  (with-output-to-temp-buffer "*neomgr file links*"
    (mapcar (lambda (x) (princ x) (princ "\n")) (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(org-element-property :path link))))))
