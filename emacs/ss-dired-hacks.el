;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following is a set of hacks for use with dired,
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

(provide 'ss-dired-hacks)

(defun ss-dired-hacks ()
  "Add goodies to dired-mode"
  (define-key dired-mode-map " " 'dired-browse-file)
  (define-key dired-mode-map "," 'dired-find-parent)
  ;; (define-key dired-mode-map "B" 'dired-byte-compile)
  (define-key dired-mode-map "L" 'dired-load-file)
  ;; (define-key dired-mode-map "\C-m" 'dired-find-mail)
  ;; RETURN in dired buffer should NOT do anything drastic!
  (define-key dired-mode-map "q" 'bury-buffer)
  (define-key dired-mode-map "Q" 'kill-buffer)
  (define-key dired-mode-map "k" 'kill-buffer)
  (define-key dired-mode-map "s" 'dired-sort-by-size)
  (define-key dired-mode-map "t" 'dired-sort-by-time)
  )

;(defun dired-byte-compile ()
;  "Byte compile file, or recompile directory"
;  (interactive nil)
;  (let ((fn (dired-get-filename)))
;    (if (file-directory-p fn)
;	(byte-recompile-directory fn)
;      (dired-byte-recompile))))

(defun dired-find-mail ()
  "Find the selected file as mail"
  (interactive nil)
  (vm-visit-folder (dired-get-filename)))

(defun dired-find-parent ()
  "Find the parent of the current directory"
  (interactive nil)
  (find-file ".."))

(defun dired-load-file ()
  "Load the current file"
  (interactive nil)
  (load (dired-get-filename) nil nil t))

(defun dired-quit ()
  "Find the currently-selected file and make the resulting window the only one"
  (interactive nil)
  (dired-find-file)
  (delete-other-windows))

(defun dired-sort-by-size (arg)			; s
  "Reread directory and sort by size. Reverse sort with prefix."
  (interactive "P")
  (revert-buffer)
  (let ((buffer-read-only nil))		; I forgot this line
    (shell-command-on-region
     (point-min) (point-max)
     (concat "sort +3n" (and arg " -r")) t)))

(defun dired-sort-by-time (arg)			; t
  "Reread directory and sort by time. Reverse sort with prefix."
  (interactive "P")
  (let ((dired-listing-switches
	 (concat dired-listing-switches "t" (and arg "r"))))
    (revert-buffer)))

;; The Browser interface (space/doubleclick) for directories

(defun dired-browse-file (&optional fn)
  "If file is already in a buffer, switch to it; else visit it read-only."
  (interactive)
  (or fn (setq fn (dired-get-filename)))
  (let (fb)
    (cond
     ( (file-directory-p fn)
       (if (setq fb (dired-existing-buffer fn))
	   (switch-to-buffer fb)
	 (dired fn)))
     ( (setq fb (get-file-buffer fn)) (switch-to-buffer fb))
     ( t (find-file-read-only fn)))))

;; The following stuff enables us to find a dired buffer
;; without having to re-read the directory.

(defun dired-find-existing-buffer (dirname)
  (let ((blist (buffer-list))
	found)
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (and (eq major-mode 'dired-mode)
		 (equal dired-directory dirname))
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    found ))

(defun dired-existing-buffer (dirname)
  "switch to an existing buffer containing directory dirname"
  (or dirname (setq dirname default-directory))
  (setq dirname (expand-file-name (directory-file-name dirname)))
  (if (file-directory-p dirname)
      (setq dirname (file-name-as-directory dirname)))
  (dired-find-existing-buffer dirname))

