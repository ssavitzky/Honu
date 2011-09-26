;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following is a set of hacks for use with gnus,
;;;	brought to you by Steve Savitzky (steve@advansoft.com)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1990 Stephen Savitzky
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gnus)
(provide 'ss-gnus-hacks)

(defun ss-gnus-hacks ()
  "Called from startup hook to autoload setup file"
  (interactive nil)
  ;; doesn't have to do anything
  )

;;;
;;; Hooks
;;;
(setq gnus-Group-mode-hook
      '(lambda ()
	 (setq browse-hook 'mouse-browse-group)
	 (setq gnus-default-article-saver 'gnus-Subject-save-in-mail)
	 (setq gnus-mail-save-name 'gnus-numeric-save-name)))

(setq gnus-Subject-mode-hook
      '(lambda ()
	 (setq browse-hook 'mouse-browse-subject)
	 (setq browse-exit-hook 'gnus-Subject-browse-exit)
	 (setq gnus-subject-lines-height 12)
	 (local-set-key "Z" 'gnus-Subject-print-article)
	 (local-set-key "F" 'gnus-Subject-forward)
	 (fset 'kill-subject-permanently
	       "kb*Subject*k")
	 (local-set-key "\M-\C-k" 'kill-subject-permanently )))

(setq gnus-Select-group-hook
      (function
       (lambda ()
	 ;; Sort by subject ignoring `Re:'.
	 ;; (Except in alt.callahans, where order is important)
	 (if (not
	      (or (string-equal gnus-newsgroup-name "alt.callahans")
		  (string-equal gnus-newsgroup-name "alt.cyberpunk.chatsubo")
		  ))
	     (gnus-sort-headers
	      (function
	       (lambda (a b)
		 (string-lessp 
		  (gnus-simplify-subject
		   (gnus-header-subject a) 're-only)
		  (gnus-simplify-subject
		   (gnus-header-subject b) 're-only)
		  ))))
	   (gnus-sort-headers
	    (function
	     (lambda (a b)
	       (gnus-date-lessp (gnus-header-date a)
				(gnus-header-date b)))))
	   )
	 ;; Don't select first article in groups prone to bigness
	 ;;   (In most cases this is better done in the KILL.el file)
	 (if (or (string-match "\.sources" gnus-newsgroup-name)
		 (string-match "\.binaries\." gnus-newsgroup-name)
		 (string-match "\.pictures" gnus-newsgroup-name)
		 (string-match "\.answers" gnus-newsgroup-name)
		 (string-match "\.callahans" gnus-newsgroup-name)
		 )
	     (setq gnus-auto-select-first nil)
	   (setq gnus-auto-select-first t))
	 ;; If a save directory exists, change to it.
	 ;;
	 (let ((dir (expand-file-name
		     (gnus-newsgroup-directory-form gnus-newsgroup-name)
		     (or gnus-article-save-directory "~/News"))))
	   (if (file-exists-p dir)
	       (cd dir)))
	 ;; save .newsrc every once in a while
	 (if (> gnus-read-articles gnus-newsrc-save-frequency)
	     (progn
	       (gnus-save-newsrc-file)
	       (setq gnus-read-articles 0))))))

(setq gnus-Article-prepare-hook
      '(lambda ()
	 ;; If a save directory exists, change to it.
	 ;;
	 (let ((dir (expand-file-name
		     (gnus-newsgroup-directory-form gnus-newsgroup-name)
		     (or gnus-article-save-directory "~/News"))))
	   (if (file-exists-p dir)
	       (cd dir)))
	 ;; Bump article count
	 (setq gnus-read-articles (1+ gnus-read-articles))))

;;;From: jad@hpcndnm.cnd.hp.com (John Dilley)
(setq news-inews-hook 'gnus-inews-insert-lines-header)
(defun gnus-inews-insert-lines-header ()
  "Called as value of news-inews-hook to count the number of lines in the
current posting and insert the header line Lines into the message."
  (interactive)
  (if (search-forward (concat "\n" mail-header-separator "\n"))
      (let ((lines (count-lines (point) (point-max))))
	(forward-line -2)
	(insert-string "Lines: " lines "\n"))))

;;;
;;; Additional Commands
;;;

(defun mouse-browse-group ()
  (gnus-Group-read-group nil nil))

(defun mouse-browse-subject ()
  (gnus-Subject-next-page nil))

(defun gnus-Subject-browse-exit ()
  "Quietly catch up and exit"
  (interactive nil)
  (gnus-Subject-catch-up-and-exit nil t))

(defun gnus-Subject-print-article ()
  "print the article using mp"
  (interactive nil)
  (gnus-eval-in-buffer-window gnus-Article-buffer (article-print))
  )

(defun article-print ()
  "use mp to pretty-print the buffer"
  (interactive nil)
  (my-print-buffer)
  )

(defun gnus-Subject-forward ()
  "Forward the current message to another user.
Stolen almost blindly from rmail.el for use in GNUS."
  (interactive)
  (switch-to-buffer gnus-Article-buffer)
  (let ((forward-buffer gnus-Article-buffer) ; used to be (current-buffer)
        (subject (concat "["
                         (mail-strip-quoted-names (mail-fetch-field "From"))
                         ": " (or (mail-fetch-field "Subject") "") "]")))
    (mail nil nil subject)
    (save-excursion
      (goto-char (point-max))
      (forward-line 1)
      (insert-buffer forward-buffer))))
; Replace the typical included-followup function with this forwarder.

;;; Effectively swap article and subject modelines.
;;; IMO these are closer to what you want: newsgroup on Subject modeline
;;;     and article subject on Article modeline.

(defun gnus-Subject-set-mode-line ()
  "Set Article mode line string."
  (let ((unmarked
	 (- (length gnus-newsgroup-unreads)
	    (length (gnus-intersection
		     gnus-newsgroup-unreads gnus-newsgroup-marked))))
	(unselected
	 (- (length gnus-newsgroup-unselected)
	    (length (gnus-intersection
		     gnus-newsgroup-unselected gnus-newsgroup-marked)))))
    (setq mode-line-buffer-identification
	  (list 17
		(format "GNUS: %s{%d} %s"
			gnus-newsgroup-name
			gnus-current-article
			;; This is proposed by tale@pawl.rpi.edu.
			(cond ((and (zerop unmarked)
				    (zerop unselected))
			       "      ")
			      ((zerop unselected)
			       (format "%d more" unmarked))
			      (t
			       (format "%d(+%d) more"
				       unmarked unselected)))
			))))
  (set-buffer-modified-p t))

(defun gnus-Article-set-mode-line ()
  "Set Subject mode line string."
  ;; The value must be a string to escape %-constructs.
  (let ((subject
	 (if gnus-current-headers
	     (nntp-header-subject gnus-current-headers)
	   gnus-newsgroup-name)))
    (setq mode-line-buffer-identification
	  (concat "GNUS: "
		  subject
		  ;; Enough spaces to pad subject to 17 positions.
		  (make-string (max 0 (- 17 (length subject))) ? ))))
  (set-buffer-modified-p t))

