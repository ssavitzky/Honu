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
  )
