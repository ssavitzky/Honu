;;;
;;; Bindings for function keys, keypads, and the like.
;;;
(provide 'ss-key-hacks)

(defvar fn-key-map (make-sparse-keymap) "*Keymap for ESC-[ encoded keyboard")
(defun make-esc-lbracket-prefix ()
  "Make M-[ a prefix character"

  (global-unset-key "\M-[")
  (define-key esc-map "[" fn-key-map)
  ;; restore the original binding of "M-[" (except you have to double it)
  (define-key fn-key-map "\M-[" 'backward-paragraph)
  (define-key esc-map "[[" 'backward-paragraph)
)

(defun move-to-window-top (num)
  "move point to top of window"
  (interactive "p")
  (move-to-window-line 0))

(defun move-to-window-bottom (num)
  "move point to bottom of window"
  (interactive "p")
  (move-to-window-line -1))

;;;
;;; Sun function and keypad keys
;;;
(defun sunkeys ()
  "Set up Sun function keys."
  (interactive nil)
  (make-esc-lbracket-prefix)
  
  ;; At this point, meta-keypad produces numeric arguments (mostly not in X)
  ;;                shift-keypad produces numbers (also capslock)
  
  (define-key fn-key-map "-1z"  nil) ;undefined key in X
  ;; This includes KP-0, Enter, and Help.
  
  ;; Redefine the Apollo keypad stuff for Sun/X.
  ;; === kp . doesn't seem to work as advertised, ===
  ;; ===      probably because it generates "del" ===
  
  (define-key fn-key-map "208z" 'execute-extended-command)	;Enter
  (define-key fn-key-map "209z" 'ignore-key) ;nothing on apollo-- Sun R2
  (define-key fn-key-map "210z" 'exchange-point-and-mark)	;KP_Decimal
  (define-key fn-key-map "211z" 'ignore-key)			;KP_0 (Sun R4)
  (define-key fn-key-map "212z" 'ignore-key)			;KP_- (Sun R5)
  (define-key fn-key-map "213z" 'ignore-key)			;KP_+ (Sun R6)
  ;; Really ought to define *, 0, +, - in outline mode:
  ;; * = show-body, 0 = hide-all, + = show-branches, - = hide-leaves
  (define-key fn-key-map "214z" 'beginning-of-buffer )		;KP_7
  (define-key fn-key-map "215z" 'previous-line)			;KP_8
  (define-key fn-key-map "216z" 'scroll-down)			;KP_9
  (define-key fn-key-map "217z" 'backward-char)			;KP_4
  (define-key fn-key-map "218z" 'ignore-key)			;KP_5
  (define-key fn-key-map "219z" 'forward-char)			;KP_6
  (define-key fn-key-map "220z" 'end-of-buffer)			;KP_1
  (define-key fn-key-map "221z" 'next-line)			;KP_2
  (define-key fn-key-map "222z" 'scroll-up)			;KP_3
  
  ;; Now override the bindings for the L keys to match their Sun markings:
  
  ;;(define-key fn-key-map "192z" 'set-mark-command) ;Mark (Select) (L1)
  ;;(define-key fn-key-map "192y" 'overwrite-mode)   ;Insert (Overwrite)
  ;;(define-key fn-key-map "193z" 'delete-char )     ;Char Delete ( L2)
  ;;(define-key fn-key-map "194z" 'copy-region-as-kill )	;Copy (sun L3)
  ;;(define-key fn-key-map "194y" 'kill-region-and-unmark )	;Cut ( S-L3)
  (define-key fn-key-map "195z" 'undo )		;undo (sun L4)
  (define-key fn-key-map "195y" 'yank )		;yank (sun L4 shift)
  (define-key fn-key-map "196z" 'beginning-of-line )		;Front (sun L5)
  (define-key fn-key-map "197z" 'copy-region-as-kill )		;Copy (Sun L6)
  (define-key fn-key-map "198z" 'scroll-right-10 )		;?Open (sun L7)
  (define-key fn-key-map "199z" 'yank )				;paste (sun L8)
  (define-key fn-key-map "200z" 're-search-forward)		;find (sun L9)
  (define-key fn-key-map "200y" 'research-forward )		;shift find
  (define-key fn-key-map "201z" 'kill-region-and-unmark )	;cut (Sun L10)
  ;;(define-key fn-key-map "201y" 'scroll-other-window )  	;shift DownBox
  
  ;; Define F1 through F3 be C-X1 through C-X3
  (define-key fn-key-map "224z" 'delete-other-windows )		;F1
  (define-key fn-key-map "225z" 'split-window-vertically )	;F2
  (define-key fn-key-map "226z" 'split-window-horizontally )	;F3
  (define-key fn-key-map "227z" 'delete-window )		;F4
  nil
  )

;;;
;;; DEC Function and keypad keys
;;;	Note that the DEC keys appear to be insensitive to shift.
;;;
(defun deckeys ()
  "Set up DEC function keys.  VT-100 and Decish X bindings."
  (interactive nil)
  (make-esc-lbracket-prefix)

  (define-key fn-key-map "11~" 'delete-other-windows) 		; F1
  (define-key fn-key-map "12~" 'split-window-vertically )	; F2
  (define-key fn-key-map "13~" 'split-window-horizontally )	; F3
  (define-key fn-key-map "14~" 'delete-window )			; F4
  (define-key fn-key-map "15~" 'ignore-key)			; F5

  (define-key fn-key-map "17~" 'beginning-of-buffer) 		; F6
  (define-key fn-key-map "18~" 'move-to-window-top)		; F7
  (define-key fn-key-map "19~" 'recenter)			; F8
  (define-key fn-key-map "20~" 'move-to-window-bottom)		; F9
  (define-key fn-key-map "21~" 'end-of-buffer)			; F10

  ;; F11 is modmap'ed to ESC, but is available shifted
  (define-key fn-key-map "23~" 'backward-paragraph)		; F11
  (define-key fn-key-map "24~" 'backward-word)			; F12
  (define-key fn-key-map "25~" 'forward-word)			; F13
  (define-key fn-key-map "26~" 'forward-paragraph)		; F14

  (define-key fn-key-map "28~" 'help-for-help)			; Help
  (define-key fn-key-map "29~" 'call-last-kbd-macro)		; Do

  (define-key fn-key-map "31~" 'other-window)			; F17
  (define-key fn-key-map "32~" 'find-file-other-window)		; F18
  (define-key fn-key-map "33~" 'switch-to-buffer-other-window)	; F19
  (define-key fn-key-map "34~" 'list-buffers)			; F20

  (define-key fn-key-map "1~" 'isearch-forward)			; Find
  (define-key fn-key-map "2~" 'yank)				; Insert here
  (define-key fn-key-map "4~" 'set-mark-command)		; Select

  (define-key fn-key-map "3~" 'kill-region)			; Remove
  (define-key fn-key-map "5~" 'scroll-down)			; Prev-screen
  (define-key fn-key-map "6~" 'scroll-up)			; Next-screen

  ;; The DEC keypad keys don't appear to be mapped in X

  nil
  )

(if (eq window-system 'x) (progn
			    (require 'ss-mouse-hacks)
			    (setq term-setup-hook 'mouse-buttons)
			    ;; The two fn. key maps are entirely disjoint
			    ;; so it's ok to do both!!
			    ;;(sunkeys)
			    (deckeys)
			    ))
