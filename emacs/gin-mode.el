;; gin-mode.el -- Set up minor mode with guess-indent stuff.
;; Copyright (C) Martin Neitzel, May 1989

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; SUMMARY
;;
;; Gnu Emacs supports filling of paragraphs and wrapping of lines with a
;; settable string to be used for the left margin, the variable
;; ``fill-prefix''.  Setting this variable by hand is fine, but can
;; become mildly annoying if it has to be changed often in a document.
;; However, the appropriate value for fill-prefix can be derived from
;; the layout of the current line in almost all cases.
;;
;; This is a minor mode that ``guesses'' the indentation to be used
;; for (auto-) filling.  It has proven to be very handy in all text-mode
;; variants.  It uses a simple but effective heuristic to guess a
;; fill-prefix based on the current line and two (configurable) regular
;; expressions.  I almost never have to use "^X." explicitly anymore.
;; 
;; The two regexps control
;; 
;; 	* what line beginnings have to be taken as "fill-prefix" (my
;; 	  standard setup recognizes initial white space and typical
;; 	  mail-prefixes like ">" or "name> ").
;; 
;; 	* what line beginnings are really hanging indents.  The
;; 	  standard setup recognizes the stars used right here,
;; 	  enumerations, and some more...

;; The guessing stuff

(provide 'gin-mode)

(defvar gin-left-hang-indent-re
  "\\s *\\([+o*?!@#$\"([{]\\|[=-]+>*\\|([0-9.]+)\\|[0-9]+.\\)\\s +"
  "*Regexp that defines a hanging indent of a paragraph.
If it is seen by gin-guess-prefix, the next lines are indented with
white space beyond the hanging indent.  Setting this variable makes
it buffer-local.")

(defvar gin-retain-indent-re
  "[a-zA-Z]*>+\\s *\\|\\s +"
  "*Regexp that defines how a fill-prefix can look like.
If such a string is seen by gin-guess-prefix in the current line,
the next line will be indented with it, too.  Setting this variable
makes it buffer-local.")

(defun gin-guess-prefix ()
  "Try to figure out the prefix for the next line."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at gin-left-hang-indent-re)
	   (let ((beg (point))
		 indent-size
		 (indent-prefix ""))
	     (re-search-forward gin-left-hang-indent-re)
	     (setq indent-size (current-column))
	     ;; First gather tabs as needed ...
	     (if indent-tabs-mode
		 (while (>= indent-size tab-width)
		   (setq indent-prefix (concat indent-prefix "\t"))
		   (setq indent-size (- indent-size tab-width))))
	     ;; ... then append the rest as spaces:
	     (while (> indent-size 0)
	       (setq indent-prefix (concat indent-prefix " "))
	       (setq indent-size (1- indent-size)))
	     indent-prefix))

	  ((looking-at gin-retain-indent-re)
	   (buffer-substring (match-beginning 0) (match-end 0)))

	  (t ""))))



;; Replacements for old functions dealing with the fill-prefix.
;; Their function values are stuffed into the original symbols.

(defun gin-fill-paragraph (arg)
  "fill-paragraph in Gin mode, tries to guess the appropriate fill-prefix.
With arg, also justify."
  (interactive "P")
  (if gin-mode
      (let ((fill-prefix (gin-guess-prefix)))
	(funcall 'gin-old-fill-paragraph arg))
    (funcall 'gin-old-fill-paragraph arg)))

(defun gin-do-auto-fill()
  (if gin-mode
      (let ((fill-prefix (gin-guess-prefix)))
	(funcall 'gin-old-do-auto-fill))
    (funcall 'gin-old-do-auto-fill)))



;; When loaded for the first time, install our minor mode indicator

(defconst gin-old-fill-paragraph nil
  "Keeps the true fill-paragraph function during Gin mode.")

(defconst gin-old-do-auto-fill nil
  "Keeps the true do-auto-fill function during Gin mode.")

(defun gin-overlay-functions()
  "Undermine emacs with Gin stuff."
  (fset 'fill-paragraph (symbol-function 'gin-fill-paragraph))
  (fset 'do-auto-fill (symbol-function 'gin-do-auto-fill)))

(defun gin-restore-originals ()
  "Throw gin-mode functions out everywhere."
  (fset 'fill-paragraph (symbol-function 'gin-old-fill-paragraph))
  (fset 'do-auto-fill (symbol-function 'gin-old-do-auto-fill)))

(if (boundp 'gin-mode)
    nil
  (setq minor-mode-alist (cons '(gin-mode " Gin") 
			       minor-mode-alist))
  (make-variable-buffer-local 'gin-mode)
  (set-default 'gin-mode nil)
  (make-variable-buffer-local 'gin-left-hang-indent-re)
  (make-variable-buffer-local 'gin-retain-indent-re)
  (fset 'gin-old-fill-paragraph (symbol-function 'fill-paragraph))
  (fset 'gin-old-do-auto-fill (symbol-function 'do-auto-fill))
  (gin-overlay-functions))

  

(defun gin-mode (arg) 
  "Minor mode to guess indentations.
Toggle gin-mode, or turn it on iff optional ARG is positiv.

Gin mode adds the capability to \"guess\" a suitable indent for
filling based on the current line.  The line is matched against the
two regexps 'gin-left-hang-indent-re' and 'gin-retain-indent-re', see
their documentation.

When Gin mode is active, auto-filling and fill-paragraph will both use
a \"guessed\" value as fill-prefix."

  (interactive "P")
  (setq gin-mode
	(if (null arg) (not gin-mode)
	  (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p (buffer-modified-p)))

(defun gin-mode-on () "Turn Gin mode on" (gin-mode 1))

