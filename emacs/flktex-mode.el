;;; flktex-mode -- major mode for editing song files in flktex format
;;;

(provide 'flktex-mode)


(defvar flktex-mode-hook nil
  "Hook run when flktex-mode is loaded."
  )

;; we probably don't want to derive from latex; we're not using any of the job stuff --
;; it's all driven by make if we want to compile.  Want to steal the syntax tables and
;; some of the environment-related stuff.  Maybe the quote expansion too.
;;
;; electric "[" (electric-pair-mode)
;; C-h S (info-lookup-symbol) could be useful for looking up chords
;; see "autotyping" in the emacs manual.

(define-derived-mode flktex-mode text-mode "FlkTex"
  "Major mode for editing song lyrics marked up in LaTeX, with chords
in brackets.
\\{flktex-mode-map}
Turning on FlkTex mode runs the normal hook `flktex-mode-hook'."

  )


(defun flk-define-shortcut-keys ()
  "This defines mode-specific keys of the form C-C<LETTER>, 
which are set aside for the user to customize.  They are, however,
massively convenient, so most users will put this in their hook."
  (interactive)
  (local-set-key "\C-Ca" 'flk-insert-a)
  (local-set-key "\C-Cb" 'flk-insert-b)
  (local-set-key "\C-Cc" 'flk-insert-c)
  (local-set-key "\C-Cd" 'flk-insert-d)
  (local-set-key "\C-Ce" 'flk-insert-e)
  (local-set-key "\C-Cf" 'flk-insert-f)
  (local-set-key "\C-Cg" 'flk-insert-g)
  (local-set-key "\C-Cm" 'flk-insert-min)
  (local-set-key "\C-CM" 'flk-insert-maj)
  (local-set-key "\C-Cs" 'flk-insert-sus)
  (local-set-key "\C-C#" 'flk-insert-sharp)
  (local-set-key "\C-C-" 'flk-insert-flat)
  (local-set-key "\C-Co" 'flk-insert-dim)
  (local-set-key "\C-C+" 'flk-insert-aug)
  (local-set-key "\C-C\C-C"  'inline-chords-at-point)
)

(defun flk-insert-a (arg)
  (interactive "p")
  (flk-insert-chord "A" arg)
)
(defun flk-insert-b (arg)
  (interactive "p")
  (flk-insert-chord "B" arg)
)
(defun flk-insert-c (arg)
  (interactive "p")
  (flk-insert-chord "C" arg)
)
(defun flk-insert-d (arg)
  (interactive "p")
  (flk-insert-chord "D" arg)
)
(defun flk-insert-e (arg)
  (interactive "p")
  (flk-insert-chord "E" arg)
)
(defun flk-insert-f (arg)
  (interactive "p")
  (flk-insert-chord "F" arg)
)
(defun flk-insert-g (arg)
  (interactive "p")
  (flk-insert-chord "G" arg)
)

(defun flk-insert-chord (chord &optional arg)
  "Insert a chord in square brackets.  If given a numerical argument, (i.e. with ^U),
append \\min for a minor chord.  With ^U^U, append \\maj."
  (insert (concat "[" chord "]"))
  (backward-char 1)
  (cond ((> arg 4) (insert "\\maj"))
	((eq arg 4) (insert "\\min"))
	)
)

;;; Insert a chord modifier
(defun flk-insert-sharp ()
  (interactive)
  (insert "\\sharp")
)
(defun flk-insert-flat ()
  (interactive)
  (insert "\\flat")
)
(defun flk-insert-min ()
  (interactive)
  (insert "\\min")
)
(defun flk-insert-maj ()
  (interactive)
  (insert "\\maj")
)
(defun flk-insert-sus ()
  (interactive)
  (insert "\\sus")
)
(defun flk-insert-dim ()
  (interactive)
  (insert "\\dim")
)
(defun flk-insert-aug ()
  (interactive)
  (insert "\\aug")
)

;;; Convert lyrics from "chords above text" to chords inline in brackets.
;;;
(defun flk-inline-chords-at-point ()
    "Convert the two lines at point from chords-over-lyrics to chords-inline.
If executed on an empty line, it inserts two backslashes to mark the end of a verse."
    (interactive)
    (beginning-of-line)
    (if (looking-at "^[:space:]*$")
	(progn (flk-gobble-line) (insert "\\\\"))
      ;; this would be simpler in Haskell, where a string is just a list of characters.
      ;; as it is, we use lists to avoid creating unnecessary intermediate strings.
      (mapc 'insert (flk-inline-chords (flk-gobble-line) (flk-gobble-line))))
    (insert "\n"))

(defun flk-gobble-line ()
  "Remove the line at point, and return it as a list of 1-character strings."
  (let ((c (char-after)))
    (if (or (null c) (char-equal ?\n c))
	(progn (delete-char 1) nil)
      (cons (string  c) (progn (delete-char 1) (flk-gobble-line)))
      )))

(defun flk-inline-chords (chords lyrics)
  "This function takes a line of chords and a line of lyrics, passed as lists of
single-character strings, and converts them to a single line with chords inlined
in brackets, returned as a list of strings."
  (reverse (convert-chords chords lyrics nil)))

(defun flk-convert-chords (chords lyrics result)
  "This function converts a line of chords and a line of lyrics into a list of strings
in reverse order.  We do it this way to take advantage of tail recursion."
  (cond ((and (null chords) (null lyrics))
	 result)			; we're done
	((null chords)
	 ;; nothing left in chords; this just pushes the remaining lyrics onto result
	 (flk-convert-chords nil (cdr lyrics) (cons (car lyrics) result)))
	((equal " " (car chords))
	 (if (null lyrics)
	     (flk-convert-chords (cdr chords) nil (cons " " result))
	   flk-(convert-chords (cdr chords) (cdr lyrics) (cons (car lyrics) result))))
	((flk-convert-chord chords lyrics "" (cons "[" result)))
	))

(defun flk-convert-chord (chords lyrics lyrics-under-chord result)
  "Convert a chord at the front of lyrics, and follow it with the remaining lyrics.
The lyrics under the chord are accumulated in a string."
  (cond ((or (null chords) (equal " " (car chords)))
	 ;; we're done with this chord.
	 flk-(convert-chords chords lyrics (cons (concat "]" lyrics-under-chord) result)))
	((null lyrics)
	 (flk-convert-chord (cdr chords) nil
			lyrics-under-chord
			(cons (flk-convert-chord-char (car chords) (cdr chords)) result)))
	((flk-convert-chord (cdr chords) (cdr lyrics)
			(concat lyrics-under-chord (car lyrics))
			(cons (flk-convert-chord-char (car chords) (cdr chords)) result)))
	))

;;; Convert a single character in a chord.
;;;    # -> \sharp
;;;    b -> \flat
;;;    m -> \min -- just \m if followed by a or i
;;;    \s -> \s if followed by u, else s
;;;    
(defun flk-convert-chord-char (char rest)
  (cond ((equal "#" char) "\\sharp")
	((equal "b" char) "\\flat")
	((equal "m" char) (cond ((null rest) "\\min")
				((or (equal "i" (car rest))
				     (equal "a" (car rest)))
				   "\\m")
				("\\min")))
	((equal "s" char) (cond ((and (listp rest)
				      (equal "u" (car rest)))
				 "\\s")
				("s")))
	(char)))
