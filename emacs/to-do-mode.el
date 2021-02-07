;;; to-do-mode -- major mode for editing in to-do format
;;;

(provide 'to-do-mode)


(defvar to-do-mode-hook nil
  "Hook run when to-do-mode is loaded."
  )

(define-derived-mode to-do-mode text-mode "To-Do"
  "Major mode for to-do files.
\\{to-do-mode-map}
Turning on To-Do mode runs the normal hook `to-do-mode-hook'."
  
  (setq-local indent-tabs-mode nil)
  (local-set-key "\C-C*" 'to-do-mark-item-done)
  )

;;; to-do-mark-item-done 
(defun to-do-mark-item-done ()
  "Change an 'o' flag character to an asterisk.
Move to next line if no open items remain on the line." ; ought to go backward with C-U
  (interactive nil)
  (move-beginning-of-line 1)
  (cond ((looking-at ".*  o ")		; this doesn't handle partial items.  Should search.
	 (re-search-forward "  o ")
	 (delete-backward-char 2)
	 (insert "* "))
	((looking-at "[[:space:]]*$")
	 (insert "  *"))	; probably ought to handle indent.  tricky.
	)
  (or (looking-at ".*  o ") (next-line))
  )

