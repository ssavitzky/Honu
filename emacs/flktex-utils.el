;;; Convert lyrics from "chords above text" to chords inline in brackets.
;;

(provide 'flktex-utils)
(setq max-lisp-eval-depth 6000)

(defun inline-chords-at-point ()
    "Convert the two lines at point from chords-over-lyrics to chords-inline.
If executed on an empty line, it inserts two backslashes to mark the end of a verse."
    (interactive)
    (beginning-of-line)
    (if (looking-at "^[:space:]*$")
	(progn (gobble-line) (insert "\\\\"))
      (insert (inline-chords (gobble-line) (gobble-line))))       
    (insert "\n"))

(defun gobble-line ()
  "Remove the line at point, and return it as a list of 1-character strings."
  (let ((c (char-after)))
    (if (or (null c) (char-equal ?\n c))
	(progn (delete-char 1) nil)
      (cons (string  c) (progn (delete-char 1) (gobble-line)))
      )))

(defun explodec (string)
  (cond ((equal string "") nil)
	((equal "\n" (substring string 0 1)) nil)
	((cons (substring string 0 1) (explodec (substring string 1))))
	))

(defun remove-newline (list)
  (if (equal "\n" (car list))
      nil
    (cons (car list) (remove-newline (cdr list)))))

;;; convert a list of chords and a list of lyrics
(defun inline-chords (chords lyrics)
  (cond ((null chords) (concat-list lyrics))
	((null lyrics) "")
	((equal " " (car chords))
	 ;; things are simple if the chords line starts with space
	 (concat (car lyrics) (inline-chords (cdr chords) (cdr lyrics))))
	 ;; otherwise, put out a left bracket and start converting the chord
	 ;; the third argument to in-chord accumulates characters from lyrics
	((concat "[" (convert-chord chords lyrics "")))
	))

(defun convert-chord (chords lyrics lyrics-under-chord)
  ;;"convert a chord at the front of lyrics, and follow it with the remaining lyrics"
  (cond ((null chords) (concat "]" lyrics-under-chord (concat-list lyrics)))
	((equal " " (car chords))
	 ;; space in chords means we're done with the one we were working on
	 (concat "]"
		 lyrics-under-chord
		 (inline-chords chords lyrics)))
	;; otherwise, add a converted chord character to the output and handle the tail.
	((concat (convert-chord-char (car chords) (cdr chords))
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

