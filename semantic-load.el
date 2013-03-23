; Functions to bulk-build semanticdb of a large project.

;
; From "Words in a defun" example in Lisp info.
;
(defun files-in-below-directory (directory)
  "List the .[ch] files in DIRECTORY and in its sub-directories."
  (let (c-files-list
	(current-directory-list
	 (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.[ch]'
       ;; and if so, append its name to a list.
       ((string-match-p "\\.[ch]$" 
			(substring (car (car current-directory-list)) -3))
	(setq c-files-list
	      (cons (car (car current-directory-list)) c-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
	;; decide whether to skip or recurse
	(if (equal "." (substring (car (car current-directory-list)) -1))
	    ;; then do nothing since filename is that of
	    ;;   current directory or parent, "." or ".."
	    ()
	  ;; else descend into the directory and repeat the process
	  (setq c-files-list (append (files-in-below-directory
				      (car (car current-directory-list)))
				     c-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    c-files-list))
;
; Based on
; https://github.com/bobbysmith007/cedet-semantic/blob/master/semanticdb-mk.el
;
(defun semantic-load (file)
  "Build semanticdb tables for a given file."
  ;; Turn on semanticdb
  (global-semanticdb-minor-mode 1)
  (save-window-excursion
    (let* ((buffer (find-file-noselect file t)) (tags nil))
      (set-buffer buffer)
      (princ (format "%s.\n" file))
      (setq tags (semantic-fetch-tags))
;      (princ (format "%s %s.\n" file (length tags)))
      (kill-buffer buffer))))

(defun semantic-load-list (files)
  "Build semanticdb tables for a list of files."
  (mapc (lambda (file) (semantic-load file)) files)
  ;; Save the databases.
  (semanticdb-save-all-db))

(defun semantic-load-all (directory)
  "Recursively build semanticdb for all C files under the directory."
  (interactive "DDirectory: ")
  (semantic-load-list (files-in-below-directory directory)))

(if noninteractive
; For a large project, semantic-load-all can take
; impractically long time. Instead a batch mode can be used as
; following:
;
;     emacs -batch -l semantic-load.el $(find . -name '*.[ch]')"
;
    (progn
      (semantic-mode)
      (require 'cl)                 ; for cdddr
      ; suppress "Follow symlink?" question
      (setq vc-follow-symlinks t)
      ; cdddr to skip "emacs -batch -l semantic-load.el" from arguments
      (semantic-load-list (cddddr command-line-args))))

;; Done
