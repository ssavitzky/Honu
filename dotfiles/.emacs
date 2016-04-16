;;; Gnu Emacs Initialization File -- Steve Savitzky

(setq my-home-emacs-dir (concat (getenv "HOME") "/Config/emacs"))
(if (file-exists-p my-home-emacs-dir)
    (setq load-path (cons my-home-emacs-dir load-path))
  (setq my-home-emacs-dir nil))

;;; Load the things we always need.  
;;;	Use load rather than require to prevent errors if files don't exist.
;;;	In some cases we need to define things we use in hooks.

(load "uniquify" t)
(load "thing" t)
(load "crypt++" t)
(or (load "gin-mode" t) 
    (defun gin-mode-on () "no gin-mode: fake it"))

(defun load-from-subdir (subdir)
  (if (file-exists-p (concat my-home-emacs-dir "/" subdir))
      (setq load-path (cons (concat my-home-emacs-dir "/" subdir) load-path))
    ))

;;; Many html-helper-mode commands are useful in other modes

(load "tempo" t)			; required for html-helper-mode
(load-from-subdir "html-helper-mode")
(load "html-helper-mode" t)
(setq html-helper-use-expert-menu t)
(setq html-helper-do-write-file-hooks nil)


(if (file-exists-p "~/.abbrev_defs") (quietly-read-abbrev-file nil))

(cond (my-home-emacs-dir
       ;; ss-browse-hacks, ss-key-hacks haven't been used for years
       (autoload 'ss-gnus-hacks "ss-gnus-hacks" "hacks for gnus" t)
       (autoload 'ss-dired-hacks "ss-dired-hacks" "hacks for dired" t)
       (add-hook 'dired-mode-hook 'ss-dired-hacks)
       
       ))

(setq ljupdir (concat my-home-emacs-dir "/ljupdate"))
(if (file-exists-p ljupdir)
    (progn (setq load-path (cons ljupdir load-path))
	   (if (fboundp 'html-helper-mode)
	       ;; we use html-helper-mode as a base for lj-update, and
	       ;; don't use html-mode anyway.
	       (defalias 'html-mode 'html-helper-mode))
	   (require 'ljupdate)
	   ;(setq lj-default-profile (lj-defprofile 'livejournal "mdlbear"))
	   )
)

;;; lj-compose to compose; ^C-s to post


(or (fboundp 'gin-mode-on) (defun gin-mode-on () "no gin-mode: fake it"))

(if (fboundp 'global-font-lock-mode) (global-font-lock-mode t))
(add-hook 'c-mode-hook 'turn-on-font-lock)
(add-hook 'java-mode-hook 'turn-on-font-lock)

(setq font-lock-maximum-decoration 3
      ;;font-lock-background-mode 'light
      )

(setq terminal-uses-flow-control-chars 
   '("vt100" "vt320" "vt52" "vt101" "vt131"))

;;;
;;; Key Bindings
;;;

;; random stuff
(global-set-key "\C-x!"   'shell)	; since C-! is shell command...
(global-set-key "\C-x="   'my-what-cursor-position)
(global-set-key "\C-xS"   'my-save-some-buffers)
(global-set-key "\C-x\r"  'compile)
;;;(global-set-key "\C-x`"   'wsr:next-error) ;some terminal setups mung this
(global-set-key "\C-x8"   'eight-space-tabs)
(global-set-key "\C-xE"	  'eval-print-last-sexp)

(global-set-key "\M-\C-l" 'goto-line)
(global-set-key "\M-o"	  'overwrite-mode)
(global-set-key "\M-p"    'print-region)
(global-set-key "\M-P"    'my-print-buffer)
(global-set-key "\M-W"    'write-region)

;; manual, mail and news
(global-set-key "\M-L"    'lj-compose)
(global-set-key "\M-\C-m" 'manual-entry)
(setq gcs-vm-define-key nil)		; prevent gcs-vm from clobbering.
(global-set-key "\M-M"    'gnus)	;was vm
(global-set-key "\M-N"    'gnus)	;was gnus-home

;; Thing stuff
(global-set-key "\C-ck"	  'kill-thing-at-point)
(global-set-key "\C-cw"   'copy-thing-at-point)

;; Mode changes
;;(global-set-key "\C-Cc"   'c-mode)
;(global-set-key "\C-Ch"   'html-helper-mode)
(global-set-key "\C-Cj"   'java-mode)
(global-set-key "\C-Co"   'outline-mode)
(global-set-key "\C-Cp"   'picture-mode)
;(global-set-key "\C-Ct"   'indented-text-mode)

(global-set-key "\C-Cf"   'auto-fill-mode) ;toggle
(global-set-key "\C-C="   'align-equals) ; align = operators in region

;(global-set-key "\C-Ch"   'w3-use-hotlist)
;(global-set-key "\C-Cu"   'w3-follow-url-at-point)
;(global-set-key "\C-CU"   'w3-fetch)
;(global-set-key "\C-CV"   'w3-open-local)
;(global-set-key "\C-CW"   'w3)

;; HTML stuff, global because we embed html all over the place
(global-set-key "\C-Cl"   'tempo-template-html-hyperlink) ;a: link
(global-set-key "\C-Cy"   'generic-insert-last-link)
(global-set-key "\C-Cc"   'tempo-template-html-code) 		; c: CODE
(global-set-key "\C-Ce"   'tempo-template-html-emphasized)	; e: EM
(global-set-key "\C-Ci"   'tempo-template-html-italic)	; i: IT
(global-set-key "\C-Cb"   'tempo-template-html-bold)	; b: BOLD
(global-set-key "\C-Cr"   'tempo-template-html-citation) ; r: reference
(global-set-key "\C-CI"   'html-helper-smart-insert-item) ; I: list item
;; html-helper-smarter-insert-item is currently broken

;; Text manipulation
;(global-set-key "\C-C\C-t" 'transpose-sentences)

;; More stuff

(global-set-key "\C-C%"   'query-replace-regexp)

;;;
;;; Variable Bindings
;;;

(setq inhibit-startup-message t)
(setq require-final-newline t)
(setq version-control 't)
(setq trim-versions-without-asking 't)	;now called delete-old-versions
;;(setq delete-old-versions 't)
(setq shell-prompt-pattern "(.*) ")
(setq kept-old-versions 0)
(setq mail-archive-file-name "~/Mail/mail-archive")
(setq mail-self-blind t)
(setq Info-enable-edit t)
(setq fill-column 76)
(setq find-file-visit-truename t)	;keeps emacs from clobbering symlinks

(setq compilations-window-height 10)

(setq browse-file-paths			; Where to look on double-click
      '(
	"Archive"			; my news archives rel. to Index
	))

(if (string= (getenv "SLOWTERM") "yes")
    (setq search-slow-speed 19200 slow-term t)
  (setq slow-term nil) )


;;; File extensions to ignore when completing filenames
(setq completion-ignored-extensions
       (append completion-ignored-extensions
 	      '(".exe" ".obj" ".prj" ".ide" ".class" ".hi" ".errors"
		".lj" ".ps" ".dvi" ".log" ".aux")))

(setq TeX-default-mode 'LaTeX-mode)
(setq TeX-dvi-print-command "dvips")

(setq default-major-mode 'indented-text-mode)

(setq auto-mode-alist
      (append 
       '(
	 ("akefile" . makefile-mode)	; ensures tabs don't get munged.
	 ("\\.make$" . makefile-mode)	; Makefile template files
	 
	 ("\\.htm$"  . html-helper-mode)	;HTML files on a pc
	 ("\\.html$"  . html-helper-mode)	;HTML files
	 ("\\.shtml$"  . html-helper-mode)	;...with server-side includes
	 ("\\.xh$"  . html-helper-mode)		; XHTML files
	 ("\\.xcf$"  . html-helper-mode) 	; XML config files
	 ("\\.xci$"  . html-helper-mode) 	; XML config files
	 ("\\.ts$"  . html-helper-mode)		; tagset files
	 ("\\.inc$"  . html-helper-mode)	; include files
	 ("\\.ht.?$"  . html-helper-mode)	; .ht, .htt, etc.

	 ("\\.m4$"  . indented-text-mode)
	 ("\\.oc$"  . indented-text-mode)
	 ("\\.flk$" . LaTeX-mode)
	 ("\\.txt$" . indented-text-mode)
	 ("\\.not$" . indented-text-mode) ;notes
	 ("\\.do$"  . indented-text-mode) ;as in to.do
	 ("^read"   . indented-text-mode) ;as in read.me
	 ("[8-9][0-9]+$" . indented-text-mode) ;as in status (date)
	 
	 ("\\.otl$" . outline-mode)

	 ("\\.gwm$" . lisp-mode)	;generic window manager
	 ("\\.ds$"  . lisp-mode)	;devilspie rules

	 ("\\.pml$" . c-mode)		;parser meta-language (ARC local)
	 
	 ("\\.pl$" . perl-mode)
	 ("\\.pm$" . perl-mode)

	 ("\\.[CH]$" . c++-mode)
	 ("\\.[ch]pp$" . c++-mode)
	 
	 ("\\.newsrc" . fundamental-mode)

	 ("/Mail/" . vm-mode)
;	 ("/News/" . vm-mode)

	 )
       auto-mode-alist
       ))

;;;
;;; ange-ftp stuff
;;;

(setq ange-ftp-default-user "anonymous")
(setq ange-ftp-generate-anonymous-password t)

;;;
;;; Property-list Bindings
;;;

(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;;;
;;; Major-Mode Hooks (for modes with no new functions defined)
;;;

;;; Tweaks for font-lock in dired-mode 
(setq dired-font-lock-keywords
  '(;; Put directory headers in italics.
    ("^  \\(/.+\\)" 1 font-lock-type-face)
    ;; Put symlinks in bold italics.
    ("\\([^ ]+\\) -> [^ ]+$" . font-lock-function-name-face)
    ;; Put marks in bold.
    ("^[^ ]" . font-lock-reference-face)
    ;; Put files that are subdirectories in bold.
    ("^..d.* \\([^ ]+\\)$" 1 font-lock-keyword-face)
    ;; Put unimportant files in a light face.
    ("[^ ]*[#~]$" . font-lock-string-face)
    ("\\.?[#][^ ]*$" . font-lock-string-face)
    ("[^ ]*\\.\\(aux\\|bak\\|class\\|dvi\\|elc\\|log\\|o\\|toc\\|ps\\|pdf\\|lj\\)$"
     . font-lock-string-face)
    ))

(defvar gin-left-hang-indent-re
  "\\s *\\([+o*?!@%#$:&\"([{]\\|[=-]+>*\\|([0-9.]+)\\|[0-9]+.\\)\\s +"
  "*Regexp that defines a hanging indent of a paragraph.
If it is seen by gin-guess-prefix, the next lines are indented with
white space beyond the hanging indent.  Setting this variable makes
it buffer-local.")

(defvar gin-retain-indent-re
  "[a-zA-Z]*>+\\s *\\|\\s +\\|# *"
  "*Regexp that defines how a fill-prefix can look like.
If such a string is seen by gin-guess-prefix in the current line,
the next line will be indented with it, too.  Setting this variable
makes it buffer-local.")

(defun my-buffer-menu-mode-hook ()
  "Hook for buffer menus"
  (setq browse-hook 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map " " 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map "\r" 'next-line)
  )
(add-hook 'buffer-menu-mode-hook 'my-buffer-menu-mode-hook)

(defun my-text-mode-hook ()
  "text mode hook using gin-mode, auto-fill-mode, and some local keydefs"
  (auto-fill-mode 1)
  (gin-mode-on)
  (setq fill-column 78)
  (setq gin-left-hang-indent-re
	"\\s *\\([-+o*?!@#$%&:\"([{]\\|[=-]+>*\\|([0-9.]+)\\|[0-9]+.\\)\\s +")
  ;(local-set-key "\C-Co" 'outline-mode) ; O = outline mode change
  ;(local-set-key "\C-Cp" 'picture-mode) ; P = picture mode change
  ;(local-set-key "\C-Cc" 'c-mode)       ; C = c mode change
  ;(local-set-key "\C-C\C-S" 'sort-lines)
  ;(local-set-key "\C-Cs" 'sort-hanging-paragraphs)
  (local-set-key "\C-CS" 'sort-paragraphs)
  ;; (make-local-variable 'browse-enter-hook)
  ;; (setq browse-enter-hook 'browse-file-at-point)
  )
(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-message-mode-hook ()
  "text mode hook using gin-mode, auto-fill-mode, and some local keydefs"
  (auto-fill-mode 1)
  ;; (gin-mode-on)
  (setq fill-column 72)
  )
(add-hook 'message-mode-hook 'my-message-mode-hook)


(setq html-helper-do-write-file-hooks nil)
(setq html-helper-build-new-buffer t)
(setq html-helper-timestamp-start nil)
(setq html-helper-timestamp-end nil)

(defun my-tex-mode-hook ()
  (local-set-key "\C-C;" 'tex-comment-lines)
  ;; strictly speaking we only need flk-define-keys if the file
  ;;   extension is .flk -- there's probably a good way to do that, 
  ;;   but I'm too lazy to figure it out right now.
  (flk-define-keys)
  )

;(add-hook 'tex-mode-hook 'my-tex-mode-hook)
(add-hook 'latex-mode-hook 'my-tex-mode-hook)

(defun my-lisp-mode-hook ()
  ;; (font-lock-mode 1)
  )
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)

(defun my-picture-mode-hook ()
  (local-set-key "\C-Ct" 'indented-text-mode) ; T = text mode change
  )
(add-hook 'picture-mode-hook 'my-picture-mode-hook)

(defun my-outline-mode-hook ()
  (local-set-key "\C-Ct" 'indented-text-mode) ; T = text mode change
  (local-set-key "\C-Cp" 'picture-mode) ; P = picture mode change
  (local-set-key "\C-Cc" 'c-mode)	; C = c mode change
    
  (local-set-key "\C-Ca" 'hide-body)	; make ALL bodies invisible
  (local-set-key "\C-Cv" 'show-all)	; make all bodies Visible
  (local-set-key "\C-Ch" 'hide-leaves) ; hide bodies under . 
  (local-set-key "\C-Ci" 'show-branches) ; show all subheadings under .
  (local-set-key "\C-Cb" 'show-entry)	; show entry body
  (gin-mode-on)
  )
(add-hook 'outline-mode-hook 'my-outline-mode-hook)

(defun my-mail-mode-hook ()
  ;; (cd "~/Mail")
  (setq fill-column 72)			; allow room for quotation.
  (setq gin-retain-indent-re "[ 	>]*")
  )
(add-hook 'mail-mode-hook 'my-mail-mode-hook)

(add-hook
 'view-hook
 (function 
  (lambda ()
    (setq browse-hook 'exit-recursive-edit)
    )))

;;;
;;; FlkTex stuff
;;;
(defun flk-define-keys ()
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

;;; Insert a chord symbol in square brackets.
;;; There's undoubtedly a way to get at the key that invoked us; 
;;;   I need to spend some time with the emacs lisp manual to find it.
(defun flk-insert-chord (chord &optional arg)
  (insert (concat "[" chord "]"))
  (backward-char 1)
  ;(if (zerop arg) () (insert "\\min"))
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

;;;
;;; HTML stuff
;;;

(setq html-helper-do-write-file-hooks nil)
(setq html-helper-build-new-buffer t)

(setq my-html-domain "savitzky.net")
(setq my-html-home-site
      (concat "http://"
	      (user-real-login-name) "." my-html-domain)
      )

(setq html-helper-address-string
      (concat "<a href=\"" my-html-home-site "\""
	      "\n         >" (user-full-name) "</a> " 
	      "\n         &lt;"
	      (user-real-login-name) "&nbsp;&#64;&nbsp;" my-html-domain "&gt;"
	      ))

(setq html-helper-new-buffer-template
      '("<html><head>\n"
	"<title>" p "</title>\n"
	"</head><body>\n"
	"<h1>" p "</h1>\n\n"
	"<hr />\n"
       	"<address>" html-helper-address-string "</address>"
	"\n</body></html>\n"))

(defun html-helper-smarter-insert-item (&optional arg)
  "Insert a new item, either in a regular list or a dictionary."
  ;; Work smarter by searching forward.  This is more likely to be
  ;; right, since it does the right thing following a sublist.
  (interactive "*P")
  (let ((case-fold-search t)
	(s "<li>\\|<dd>\\|<dt>\\|</ul>\\|</ol>\\|</menu>\\|</dir>\\|</dl>"))
    (if
        (save-excursion
          (re-search-forward s nil t) 
	  (re-search-backward s nil t)
          (looking-at "<dt>\\|<dd>\\|</dl>"))
        (tempo-template-html-definition-item arg)
      (tempo-template-html-item arg))))

(defun html-helper-insert-link-item (&optional arg)
  "Insert a list item with a link into the buffer"
  (interactive "*P")
  (html-helper-smarter-insert-item arg)
  (tempo-template-html-anchor arg)
  )

(defun html-helper-insert-RCS-id ()
  "Insert a list item with a link into the buffer"
  (interactive)
  (insert "<b>$Id: .emacs,v 1.2 1998/02/21 16:41:34 steve Exp $</b><br>\n")
  )

(defun my-html-mode-hook ()
  "Hook for HTML editing"
  (setq browse-hook 'w3-follow-url-at-point)
  (local-set-key "\C-Cv" 'w3-preview-this-buffer)
  (auto-fill-mode 1)
  (setq gin-left-hang-indent-re
	"\\s *\\([+o*?!@$\"([{]\\|[=-]+>*\\|<[^> ]+>\\)\\s +")
  (setq gin-retain-indent-re
	"\\s +\\|[#]+\\s *" )
  (gin-mode-on)
  (setq fill-column 78)

  (local-set-key [C-M-return]   'html-helper-smarter-insert-item)
  (local-set-key [M-S-return]   'html-helper-insert-link-item)
  (local-set-key "\C-Cb"   'tempo-template-html-bold) 		; b: B
  (local-set-key "\C-Cc"   'tempo-template-html-code) 		; c: CODE
  (local-set-key "\C-Cd"   'tempo-template-html-definition-list); d: DL
  (local-set-key "\C-Ce"   'tempo-template-html-emphasized)	; e: EM
  (local-set-key "\C-Ci"   'html-helper-smarter-insert-item)	; i: item
  (local-set-key "\C-Cl"   'tempo-template-html-hyperlink)	; l: link
  (local-set-key "\C-Co"   'tempo-template-html-ordered-list)	; o: OL
  (local-set-key "\C-Cs"   'tempo-template-html-strong)		; s: STRONG
  (local-set-key "\C-Cu"   'tempo-template-html-unordered-list)	; u: UL
  )
(add-hook 'html-mode-hook 'my-html-mode-hook)
(add-hook 'html-helper-mode-hook 'my-html-mode-hook)

;;;
;;; Function Definitions (mostly not for specific modes)
;;;

;To: unix-emacs@bbn.com
;Date: 18 Nov 88 10:55:11 GMT
;From: MIT.EDU!@EDDIE
;Subject: Fn. to line up assigment ops.
;
;The following little bit of lisp will ensure the first assignment operators
;on each of the lines line up. This is part of our local formatting style
;'cos it looks nice ;-)
;
;The style of the lisp however, is atrocious. All the problems come from ==,
;which looks too much like 'op='.
;
;  (typically bound by the author to M-C-= -- I prefer C-C=)
;
;Enjoy.
;
;Paul Hudson 
;
;Snail mail: Monotype ADG	Email:	...!ukc!acorn!moncam!paul
;	    Science Park,		paul@moncam.co.uk
;	    Milton Road,	"Sun Microsysytems:
;	    Cambridge,		 The Company is Arrogant (TM)"
;	    CB4 4FQ

(defun align-equals (start end)
  "make the first assignment operator on each line line up vertically"
  (interactive "*r")
  (save-excursion
    (save-restriction
     (let ((indent 0))
       (narrow-to-region start end)
       (beginning-of-buffer)
       (while (not (eobp))
	 (if (find-assignment)
	     (progn
	       (exchange-point-and-mark)
	       (setq indent (max indent (current-column)))
	       (delete-horizontal-space)
	       (insert " ")))
	 (forward-line 1))
       (beginning-of-buffer)
       (while (not (eobp))
	 (if (find-assignment)
	     (indent-to-column (1+ (- indent  (- (mark) (point))))))
	 (forward-line 1)))
     )))


(defun find-assignment ()
  (if (re-search-forward
	     "[^<>=!]=\\|\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>="
	     (save-excursion (end-of-line) (point)) t)
      (progn
	(goto-char (match-beginning 0))
	(if (looking-at ".==")
	    nil
	  (if (looking-at "\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>=")
	      (set-mark (match-end 0))
	    (forward-char 1)
	    (set-mark (1+ (point))))
	  (delete-horizontal-space)
	  t))
    nil))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond 
    ((looking-at "[([{]") (forward-sexp 1) (backward-char))
    ((looking-at "[])}]") (forward-char) (backward-sexp 1))
    (t (self-insert-command (or arg 1)))))

(global-set-key "\C-X%" 'match-paren)

(defun tex-comment-lines ()
  "Attach a comment to multiple lines"
  (interactive)
  (query-replace-regexp "^" "% ")
)

(defun eight-space-tabs ()
  "Set the tab-width for the current buffer to 8"
  (interactive nil)
  (setq tab-width 8)
)

(defun retabify ()
  "Convert 8-space tabs in current buffer to current buffer's tabs"
  (interactive nil)
  (let ((buf-tab-width tab-width))
    (mark-whole-buffer)
    (setq tab-width 8)
    (untabify)
    (setq tab-width buf-tab-width)
    (tabify)
  )
)

(defun my-print-buffer ()
  "use mp to pretty-print the buffer.  Of course it loses if mp doesn't exist."
  (interactive nil)
;  (shell-command-on-region
;   (point-min)
;   (point-max)
;   (concat "mp -o -s " (buffer-name) " | lpr -Plw"))
  (print-buffer)
)

(defun my-save-some-buffers ()
  "Save all buffers without asking."
  (interactive nil)
  (save-some-buffers 't)
)

(defun my-what-cursor-position ()
  "Print info on cursor position (on screen and within buffer)."
  (interactive)
  (let* ((char (following-char))
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format " Hscroll=%d" (window-hscroll))))
	 (row (1+ (count-lines 1 (point))))
	 (col (current-column)))
    (if (= pos end)
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "point=%d of %d(%d%%) <%d - %d>  x=%d %s"
		     pos total percent beg end col hscroll)
	  (message "point=%d of %d(%d%%)  x=%d y=%d %s"
		   pos total percent col row hscroll))
      (if (or (/= beg 1) (/= end (1+ total)))
	  (message "Char: %s (0%o)  point=%d of %d(%d%%) <%d - %d>  x=%d %s"
		   (single-key-description char) char pos total
		   percent beg end col hscroll)
	(message "Char: %s (0%o)  point=%d of %d(%d%%)  x=%d y=%d %s"
		 (single-key-description char) char pos total
		 percent col row hscroll)))))

(defun sort-hanging-paragraphs (reverse beg end)
  "Sort paragraphs in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
Paragraphs are delimited only by empty lines."
  (interactive "P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (sort-subr reverse
	       (function (lambda () (skip-chars-forward "\n\t\f")))
	       (function (lambda () (re-search-forward "^$"))) )))

;;;
;;; Perl-mode stuff
;;;

(add-hook 'perl-mode-hook 'my-perl-mode-hook)
(defun my-perl-mode-hook ()
  "C++ mode hook"
  (interactive nil)
  (setq fill-column 78)
)

;;;
;;; C- and C++-mode stuff
;;;
 
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(defun my-c++-mode-hook ()
  "C++ mode hook"
  (interactive nil)
  ;; C++-mode doesn't really handle indentation right.
  ;;    Use gin-mode to properly handle paragraph filling.
  ;;    Set paragraph-start to handle paragraphs in block
  ;;    comments, and set comment-start-skip to properly handle
  ;;    indentation after comment when filling.
  (gin-mode-on)
  ;; (font-lock-mode 1)
  ;;(setq paragraph-separate "^\\s *$\\|^\\s *//\\s *$")
  (setq paragraph-separate "^\\s *$")
  (setq paragraph-start
	(concat "^\\s *$\\|^\\s *//\\s *$"
		"\\|" page-delimiter))
  (setq comment-start-skip "/\\*+\\s *\\|//\\s *")
  (setq gin-left-hang-indent-re "^$")
  (setq gin-retain-indent-re "\\s *//\\s *")
  (setq fill-column 78)
  (setq comment-column 8)
  (setq indent-tabs-mode t)
  (auto-fill-mode 1)
  (setq c++-comment-only-line-offset 4)	; doesn't appear to work.
    
  (local-set-key "\C-Cc" 'c++-mode)	; C = back to C++ mode
  (local-set-key "\C-Co" 'outline-mode) ; O = outline mode change
  (local-set-key "\C-Cp" 'picture-mode) ; P = picture mode change
  (local-set-key "\C-Ct" 'indented-text-mode) ; T = text mode change
  (local-set-key "%" 'match-paren)	; % matches paren if on one
  ;; (setq browse-enter-hook 'mouse-find-tag)
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)
(defun my-c-mode-hook ()
  "C mode hook"
  (gin-mode-on)
  ;; (font-lock-mode 1)
  ;; (setq gin-left-hang-indent-re "\\s */*\\*+\\s *")
  ;; (setq gin-retain-indent-re "\\s */?\\*+\\s +")
  (setq fill-column 78)
  (setq comment-column 40)
    
  (local-set-key "\C-Cc" 'c-mode)		; C = back to C mode
  (local-set-key "\C-Co" 'outline-mode)	; O = outline mode change
  (local-set-key "\C-Cp" 'picture-mode)	; P = picture mode change
  (local-set-key "\C-Ct" 'indented-text-mode) ; T = text mode change
  (local-set-key "%" 'match-paren) ; % matches paren if on one
  ;; (setq browse-enter-hook 'mouse-find-tag)
  )

(setq c-indent-level 4)
(setq c-argdecl-indent 4)
(setq c-label-offset -2)
(setq c-continued-statement-offset 4)
(setq c-tab-always-indent nil)
(setq c-brace-offset -4)
(setq comment-column 48)

(defun beginning-of-C-defun ()
  "Move point to the beginning of the C function in which point finds itself"
  (interactive nil)
  (beginning-of-defun)
  (backward-paragraph)
)

(defun mouse-find-tag () (find-tag (find-tag-default)))

;;;
;;; gnus newsreader stuff
;;;
;;;    See also ss-gnus-hacks, which is autoloaded as gnus-Startup-hook.
;;;    Some variables want to be set before gnus is loaded.
;;;


;(setq gnus-auto-select-same t)		; like rn -s
;(setq gnus-auto-select-next t)

;;; The following are used by local hacks

(setq gnus-newsrc-save-frequency 20)
(setq gnus-read-articles 0)

;;; Autoload the file ss-gnus-hacks for the real goodies

(and my-home-emacs-dir
     (add-hook 'gnus-startup-hook
	       'ss-gnus-hacks))

;;; Use default article sorting.
(setq ss-sort-compatibility t)

;(setq message-default-headers "Fcc: ~/\n")
(setq message-default-mail-headers "Bcc: steve\nFcc: ~/Mail/mail-archive\n")
(setq message-default-news-headers "Fcc: ~/News/.outgoing\n")


;;; 
;;; Misc stuff from other people
;;;

;; because I have so many screens open all the time, and idle C-xC-c can
;; really screw things up for me.  So make sure we dont exit without
;; confirmation, and rebind C-xC-c appropriately

(defun my-exit-from-emacs ()
  (interactive)
  (if (yes-or-no-p "Do you want to exit ")
      (save-buffers-kill-emacs)))

;;;
;;; Final setup
;;;

(if (eq window-system 'x)
    (progn
      (tool-bar-mode -1)
      (if (null (cdr command-line-args))
	  ;; This stuff is only done in the main Emacs of an X session.
	  (find-file "~")))
  (progn
    ;; this stuff is done if emacs is being run from a terminal or console
    ;; the idea is that if you're in via ssh, the time updates will keep 
    ;; the connection alive.
    t)
  )
(display-time)

;;; Set the window manager's title.  This has the same format as mode-line-format
(setq frame-title-format
      (quote ("%b  "  (vc-mode vc-mode) " - " invocation-name "@" system-name)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(browse-url-browser-function (quote browse-url-mozilla))
 '(browse-url-mozilla-program "iceweasel")
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "ASCII")
 '(delete-old-versions t)
 '(display-time-mode t)
 '(font-use-system-font t)
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-mode-hook (quote (turn-on-haskell-doc turn-on-haskell-indentation)))
 '(lj-default-headers "Music: 
Preformatted: yes
")
 '(lj-default-server "dreamwidth.org")
 '(lj-default-username "mdlbear")
 '(lj-fill-function (quote ignore))
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info system-name mode-line-end-spaces)))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t nil (paren))
 '(text-mode-hook
   (quote
    (turn-on-auto-fill my-text-mode-hook text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "steve@savitzky.net")
 '(x-select-enable-clipboard t))
;(custom-set-faces)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))
