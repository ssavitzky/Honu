;;; Convert lyrics from "chords above text" to chords inline in brackets.
;;;

(provide 'flktex-utils)
(setq max-lisp-eval-depth 6000)

(defun inline-chords-at-point ()
    "Convert the two lines at point from chords-over-lyrics to chords-inline.
If executed on an empty line, it inserts two backslashes to mark the end of a verse."
    (interactive)
    (beginning-of-line)
    (if (looking-at "^[:space:]*$")
	(progn (gobble-line) (insert "\\\\"))
      ;; this would be simpler in Haskell, where a string is just a list of characters.
      ;; as it is, we use lists to avoid creating unnecessary intermediate strings.
      (mapc 'insert (inline-chords (gobble-line) (gobble-line))))
    (insert "\n"))

(defun gobble-line ()
  "Remove the line at point, and return it as a list of 1-character strings."
  (let ((c (char-after)))
    (if (or (null c) (char-equal ?\n c))
	(progn (delete-char 1) nil)
      (cons (string  c) (progn (delete-char 1) (gobble-line)))
      )))

(defun inline-chords (chords lyrics)
  "This function takes a line of chords and a line of lyrics, passed as lists of
single-character strings, and converts them to a single line with chords inlined
in brackets."
  (cond ((null chords) lyrics)
	((equal " " (car chords))
	 (if (null lyrics)
	     (cons " " (inline-chords (cdr chords) nil))
	   (cons (car lyrics) (inline-chords (cdr chords) (cdr lyrics)))))
	((cons "[" (convert-chord chords lyrics "")))
	))

(defun convert-chord (chords lyrics lyrics-under-chord)
  "Convert a chord at the front of lyrics, and follow it with the remaining lyrics.
The lyrics under the chord are accumulated in a string."
  (cond ((null chords) (cons "]" (if (equal "" lyrics-under-chord)
				     lyrics
				   (cons lyrics-under-chord lyrics))))
	((equal " " (car chords))
	 (cons "]" (if (equal "" lyrics-under-chord)
		       (inline-chords chords lyrics)
		     (cons lyrics-under-chord (inline-chords chords lyrics)))))
	((null lyrics)
	 (cons (convert-chord-char (car chords) (cdr chords))
	       (convert-chord (cdr chords) nil lyrics-under-chord)))
	((cons (convert-chord-char (car chords) (cdr chords))
	       (convert-chord (cdr chords)
			      (cdr lyrics)
			      (concat lyrics-under-chord (car lyrics)))))
	))

		 
;;; Convert a single character in a chord.
;;;    # -> \sharp
;;;    b -> \flat
;;;    m -> \min -- just \m if followed by a or i
;;;    \s -> \s if followed by u, else s
;;;    
(defun convert-chord-char (char rest)
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

(defun concat-list (strings)
  (if (null strings)
      ""
    (concat (car strings) (concat-list (cdr strings)))))

