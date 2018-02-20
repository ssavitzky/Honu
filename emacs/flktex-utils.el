;;; Convert lyrics from "chords above text" to chords inline in brackets.
;;

(defun inline-chords-at-point ()
    "Convert the two lines at point from chords-over-lyrics to chords-inline"
    (interactive)
    (let ((chords (prog1 (thing-at-point 'line) (kill-line t)))
	  (lyrics	 (prog1 (thing-at-point 'line) (kill-line))))
      (insert (inline-chords chords lyrics)))
    (forward-char))

;;; convert a line of chords and a line of lyrics
;;;
(defun inline-chords (chordstr lyricstr)
  (inline-lists (explodec chordstr) (explodec lyricstr)))

(defun explodec (string)
  (remove-newline (split-string string "" t)))

(defun remove-newline (list)
  (if (equal "\n" (car list))
      nil
    (cons (car list) (remove-newline (cdr list)))))

;;; convert a list of chords and a list of lyrics
(defun inline-lists (chords lyrics)
  (cond ((null chords) nil)
	((equal " " (car chords))
	 ;; things are simple if the chords line starts with space
	 (concat (car lyrics) (inline-lists (cdr chords) (cdr lyrics))))
	(t
	 ;; otherwise, put out a left bracket and start converting the chord
	 ;; the third argument to in-chord accumulates characters from lyrics
	 (concat "[" (convert-chord chords lyrics nil)))))

(defun convert-chord (chords lyrics lyrics-under-chord)
  (cond ((null chords) "]")
	((or (equal " " (car chords)) (equal "\n" (car chords))
	 ;; space in chords means we're done with the one we were working on
	 (concat "]"
		 (reverse-concat-list lyrics-under-chord)
		 (inline-lists chords lyrics)))
	;; otherwise, add a character to the output and handle the tail.
	;; note that characters are pushed onto lyrics-under-chord
	((concat (convert-chord-char (car chords) (cdr chords))
		 (convert-chord (cdr chords) (cdr lyrics)
				(cons (car lyrics) lyrics-under-chord))))))

;;; Convert a single character in a chord.
;;;    # -> \sharp
;;;    b -> \flat
;;;    m -> \min -- just \m if followed by a or i
;;;    \s -> \s if followed by u, else s
;;;    
(defun convert-chord-char (char rest)
  (cond ((equal "#" char) "\\sharp")
	((equal "b" char) "\\flat")
	((equal "m" char) (cond ((or (equal "i" (car rest))
				     (equal "a" (car rest)))
				   "\\m")
				(t "\\min")))
	((equal "s" char) (if (equal "u" (car rest))
			      "\\s"
			    "s"))
	(t char)))

(defun reverse-concat-list (list)
  (if (null list)
      ""
    (concat (reverse-concat-list (cdr list)) (car list))))





	  




