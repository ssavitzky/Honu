;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following is a set of hacks for use with gnus,
;;;	brought to you by Steve Savitzky (steve@crc.ricoh.com)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1993 Stephen Savitzky
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
(add-hook 'gnus-group-mode-hook
      (function
       (lambda ()
	 (setq browse-hook 'mouse-browse-group)
	 (setq gnus-default-article-saver 'gnus-summary-save-in-mail)
	 (setq gnus-mail-save-name 'gnus-numeric-save-name))))

(add-hook 'gnus-summary-mode-hook
      (function
       (lambda ()
	 (setq browse-hook 'mouse-browse-summary)
	 (setq browse-exit-hook 'gnus-summary-browse-exit)
	 (setq gnus-summary-lines-height 12)
	 (local-set-key "Z" 'gnus-summary-print-article)
	 (local-set-key "F" 'gnus-summary-forward)
	 (fset 'kill-subject-permanently
	       "kb*Subject*k")
	 (local-set-key "\M-\C-k" 'kill-subject-permanently ))))

(add-hook 'gnus-select-group-hook
      (function
       (lambda ()
	 ;; Don't select first article in groups prone to bigness
	 ;;   (In most cases this is better done in the KILL.el file)
	 (if (or (string-match "\.sources" gnus-newsgroup-name)
		 (string-match "\.binaries\." gnus-newsgroup-name)
		 (string-match "\.pictures" gnus-newsgroup-name)
		 (string-match "\.answers" gnus-newsgroup-name)
		 (string-match "\.callahans" gnus-newsgroup-name)
		 (string-match "\.coffeehouse" gnus-newsgroup-name)
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

(add-hook 'gnus-article-prepare-hook
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
  (gnus-group-read-group nil nil))

(defun mouse-browse-summary ()
  (gnus-summary-next-page nil))

(defun gnus-summary-browse-exit ()
  "Quietly catch up and exit"
  (interactive nil)
  (gnus-summary-catch-up-and-exit nil t))

(defun gnus-summary-print-article ()
  "print the article using mp"
  (interactive nil)
  (gnus-eval-in-buffer-window gnus-article-buffer (article-print))
  )

(defun article-print ()
  "use mp to pretty-print the buffer"
  (interactive nil)
  (my-print-buffer)
  )

;;; Should keep original author and set Resent-By (?)
(defun gnus-summary-forward ()
  "Forward the current message to another user.
Stolen almost blindly from rmail.el for use in GNUS."
  (interactive)
  (switch-to-buffer gnus-article-buffer)
  (let ((forward-buffer gnus-article-buffer) ; used to be (current-buffer)
        (subject (concat "["
                         (mail-strip-quoted-names (mail-fetch-field "From"))
                         " in " gnus-newsgroup-name ": "
			 (or (mail-fetch-field "Subject") "") "]")))
    (mail nil nil subject)
    (save-excursion
      (goto-char (point-max))
      (forward-line 1)
      (insert-buffer forward-buffer))))
; Replace the typical included-followup function with this forwarder.

;;; Effectively swap article and subject modelines.
;;; IMO these are closer to what you want: newsgroup on Subject modeline
;;;     and article subject on Article modeline.

(if nil (progn
	  ;; eliminated for now ...

(defun gnus-subject-set-mode-line ()
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

(defun gnus-article-set-mode-line ()
  "Set subject mode line string."
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

))
