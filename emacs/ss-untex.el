;;; untex.el -- steve Savitzky
;;;
;;;   A quick-and-dirty hack to remove LaTeX commands from a buffer.
;;;   Handles local hacks \nl and \[Ff]oil{...}
;;;   There is a lot of quick-and-dirty stuff here.
;;;   Doesn't handle the prelude -- you have to do that by hand.
;;;
;;;   This would probably be simpler in Perl.

(provide 'untex)

(defun fix-in-buffer (from to)
  "fix a string pair in the whole buffer"
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp from to)
    ))

(defun untex ()
  "unlatexify buffer"
  (interactive nil)
  (let ((case-fold-search nil))
    ;; \Foil{...} is a local hack
    (fix-in-buffer "^\\\\[fF]oil{\\([^}]*\\)}" "\\1")
    (fix-in-buffer "^\\\\section{\\([^}]*\\)}" "\\1")
    (fix-in-buffer "^\\\\subsection{\\([^}]*\\)}" "\\1")
    (fix-in-buffer "^\\\\subsubsection{\\([^}]*\\)}" "\\1")
    (fix-in-buffer "\\\\item\\[\\([^\]]*\\)]" "*  \\1")
    (fix-in-buffer "\\\\item" "*  ")
    (fix-in-buffer "\\\\begin.*$" "")
    (fix-in-buffer "\\\\end.*$" "")
    (fix-in-buffer "\\\\vskip.*$" "")
    (fix-in-buffer "\\\\nl" "")		; local hack -- like \\
    (fix-in-buffer "\\\\hline" "")
    (fix-in-buffer "\\\\hskip *[^ ]* " "")
    (fix-in-buffer "\\\\ldots" "...")
    (fix-in-buffer "\\\\hfill" " ")
    ;;
    ;; Now try to handle stuff in {...}, usually font changes
    ;;   Any string of macros after {\...<space> gets eliminated.
    ;;   Block must end on the same line as it starts; no nesting.
    ;;
    (fix-in-buffer "{\\\\[^ ]* \\([^}]*\\)}" "\\1")
    ;;
    ;; Now the font-change stuff that didn't get caught
    ;;
    (fix-in-buffer "\\\\huge" "")
    (fix-in-buffer "\\\\Huge" "")
    (fix-in-buffer "\\\\large" "")
    (fix-in-buffer "\\\\Large" "")
    (fix-in-buffer "\\\\bf" "")
    (fix-in-buffer "\\\\it" "")
    (fix-in-buffer "\\\\rm" "")
    (fix-in-buffer "\\\\tt" "")
    (fix-in-buffer "\\\\em" "")
    ;;
    ;; Now handle stuff that shouldn't be done earlier
    ;;
    (fix-in-buffer "\\\\{" "{")
    (fix-in-buffer "\\\\}" "}")
    (fix-in-buffer "\\\\\\\\" "")
    ))
