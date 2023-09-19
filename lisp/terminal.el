;;; terminal.el --- Managing multiple terminal buffers in Emacs.

;; Original Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Features that might be required by this library:
;;
;;  `term' `cl-lib' `advice'
;;
;;; Installation:
;;
;; Copy terminal.el to your load-path and add to your ~/.emacs
;;
;;  (require 'terminal)
;;
;; And setup program that `terminal' will need:
;;
;; (setq terminal-program "/bin/bash")
;;
;;      or setup like me "/bin/zsh" ;)
;;
;; Below are the commands you can use:
;;
;;      `terminal'                    Create a new term buffer.
;;      `terminal-next'               Switch to next term buffer.
;;      `terminal-prev'               Switch to previous term buffer.
;;      `terminal-dedicated-open'     Open dedicated term window.
;;      `terminal-dedicated-close'    Close dedicated term window.
;;      `terminal-dedicated-toggle'   Toggle dedicated term window.
;;      `terminal-dedicated-select'   Select dedicated term window.
;;
;; Tips:
;;
;;      You can type `C-u' before command `terminal' or `terminal-dedicated-open'
;;      then will prompt you shell name for creating terminal buffer.
;;

;;; Customize:
;;
;; `terminal-program' default is nil, so when creating new term buffer,
;; send environment variable of `SHELL' (`ESHELL', `/bin/sh') to `make-term'.
;;
;; And you can set it to your liking, like me: ;-)
;;
;; (setq terminal-program "/bin/zsh")
;;
;; `terminal-default-dir' default is `~/', only use when current buffer
;; is not in a real directory.
;;
;; `terminal-buffer-name' is the name of term buffer.
;;
;; `terminal-scroll-show-maximum-output' controls how interpreter
;; output causes window to scroll.
;;
;; `terminal-scroll-to-bottom-on-output' controls whether interpreter
;; output causes window to scroll.
;;
;; `terminal-switch-after-close' try to switch other `terminal' buffer
;; after close current one.
;; If you don't like this feature just set it with nil.
;;
;; `term-unbind-key-list' is a key list to unbind some keystroke.
;;
;; `term-bind-key-alist' is a key alist that binds some keystroke.
;; If you don't like default, modify it.
;;
;; `terminal-dedicated-window-height' the height of a dedicated term window.
;;
;; `terminal-dedicated-max-window-height' the max height limit that dedicated
;; window is allowed.
;;
;; `terminal-dedicated-skip-other-window-p' whether skip dedicated term
;; window when use command `other-window' to cycle windows order.
;;
;; All of the above can be customize by:
;;      M-x customize-group RET terminal RET
;;
;;; Require:
(require 'term)
(require 'cl-lib)
(require 'advice)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup terminal nil
  "Multi term manager."
  :group 'term)

(defcustom terminal-program nil
  "The program of term.
If this is nil, setup to environment variable of `SHELL'."
  :type 'string
  :group 'terminal)

(defcustom terminal-program-switches nil
  "The command-line switches to pass to the term program."
  :type 'string
  :group 'terminal)

(defcustom terminal-try-create t
  "Try to create a new term buffer when switch.

When use `terminal-next' or `terminal-prev', switch term buffer,
and try to create a new term buffer if no term buffers exist."
  :type 'boolean
  :group 'terminal)

(defcustom terminal-default-dir "~/"
  "The default directory for terms if current directory doesn't exist."
  :type 'string
  :group 'terminal)

(defcustom terminal-buffer-name "terminal"
  "The buffer name of term buffer."
  :type 'string
  :group 'terminal)

(defcustom terminal-scroll-show-maximum-output nil
  "*Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled.

See variable `terminal-scroll-to-bottom-on-output'."
  :type 'boolean
  :group 'terminal)

(defcustom terminal-scroll-to-bottom-on-output nil
  "*Controls whether interpreter output causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing buffer.
If `this', scroll only the selected window.
If `others', scroll only those that are not the selected window.

The default is nil.

See variable `terminal-scroll-show-maximum-output'."
  :type 'boolean
  :group 'terminal)

(defcustom terminal-switch-after-close 'NEXT
  "Try to switch other `terminal' buffer after close current one.
If this option is 'NEXT, switch to next `terminal' buffer;
If this option is 'PREVIOUS, switch to previous `terminal' buffer.
If this option is nil, don't switch other `terminal' buffer."
  :type 'symbol
  :group 'terminal)

(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "C-SPC" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'terminal)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-c C-e" . term-send-esc)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-return)
    ("C-y" . term-paste)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("<C-backspace>" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-d" . term-send-delete-word)
    ("M-," . term-send-raw)
    ("M-." . comint-dynamic-complete))

  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'terminal)

(defcustom terminal-dedicated-window-height 14
  "The height of `terminal' dedicated window."
  :type 'integer
  :group 'terminal)

(defcustom terminal-dedicated-max-window-height 30
  "The max height limit of `terminal' dedicated window.
Default, when hide `terminal' dedicated window, will remember
window height before hide, except height is larger than this.`"
  :type 'integer
  :group 'terminal)

(defcustom terminal-dedicated-skip-other-window-p nil
  "Default, can have `other-window' select window in cyclic ordering of windows.
In cases you don't want to select `terminal' dedicated window, use `other-window'
and make `terminal' dedicated window as a viewable sidebar.

So please turn on this option if you want to skip `terminal' dedicated window with `other-window'.

Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         ;; ad-advised-definition-p no longer exists on Emacs 24.4 as of 2014-01-03.
         (when (fboundp 'terminal-dedicated-handle-other-window-advice)
           (if (fboundp 'ad-advised-definition-p)
               (when (ad-advised-definition-p 'other-window)
                 (terminal-dedicated-handle-other-window-advice value))
             (when (ad-is-advised 'other-window)
               (terminal-dedicated-handle-other-window-advice value)))))
  :group 'terminal)

(defcustom terminal-dedicated-select-after-open-p nil
  "Default, terminal won't focus terminal window after you open dedicated window.
Please make this option with t if you want focus terminal window.

Default is nil."
  :type 'boolean
  :group 'terminal)

(defcustom terminal-dedicated-close-back-to-open-buffer-p nil
  "Some userlike the cursor return to the position it was before I opened the dedicated terminal window.
Please make this option with t if you want it. ;)

Default is nil."
  :type 'boolean
  :group 'terminal
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constant ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst terminal-dedicated-buffer-name "TERMINAL-DEDICATED"
  "The buffer name of dedicated `terminal'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar terminal-dedicated-window nil
  "The dedicated `terminal' window.")

(defvar terminal-dedicated-buffer nil
  "The dedicated `terminal' buffer.")

(defvar terminal-dedicated-close-buffer nil
  "The buffer that first time open dedicated `terminal' buffer.
Details look option `terminal-dedicated-close-back-to-open-buffer-p'.")

(defvar terminal-buffer-list nil
  "The list of non-dedicated terminal buffers managed by `terminal'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun terminal ()
  "Create new term buffer.
Will prompt you shell name when you type `C-u' before this command."
  (interactive)
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (terminal-get-buffer current-prefix-arg))
    (setq terminal-buffer-list (nconc terminal-buffer-list (list term-buffer)))
    (set-buffer term-buffer)
    ;; Internal handle for `terminal' buffer.
    (terminal-internal)
    (terminal-switch-buffer term-buffer default-directory)))

;;;###autoload
(defun terminal-next (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (terminal-switch 'NEXT (or offset 1)))

;;;###autoload
(defun terminal-prev (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will goto previous term buffer with OFFSET."
  (interactive "P")
  (terminal-switch 'PREVIOUS (or offset 1)))

;;;###autoload
(defun terminal-dedicated-open ()
  "Open dedicated `terminal' window.
Will prompt you shell name when you type `C-u' before this command."
  (interactive)
  (if (not (terminal-dedicated-exist-p))
      (let ((current-window (selected-window)))
        (if (terminal-buffer-exist-p terminal-dedicated-buffer)
            (unless (terminal-window-exist-p terminal-dedicated-window)
              (terminal-dedicated-get-window))
          ;; Set buffer.
          (setq terminal-dedicated-buffer (terminal-get-buffer current-prefix-arg t))
          (set-buffer (terminal-dedicated-get-buffer-name))
          ;; Get dedicate window.
          (terminal-dedicated-get-window)
          ;; Whether skip `other-window'.
          (terminal-dedicated-handle-other-window-advice terminal-dedicated-skip-other-window-p)
          ;; Internal handle for `terminal' buffer.
          (terminal-internal))
        (set-window-buffer terminal-dedicated-window (get-buffer (terminal-dedicated-get-buffer-name)))
        (set-window-dedicated-p terminal-dedicated-window t)
        ;; Select window.
        (select-window
         (if terminal-dedicated-select-after-open-p
             ;; Focus dedicated terminal window if option `terminal-dedicated-select-after-open-p' is enable.
             terminal-dedicated-window
           ;; Otherwise focus current window.
           current-window)))
    (message "`terminal' dedicated window has exist.")))

(defun terminal-dedicated-close ()
  "Close dedicated `terminal' window."
  (interactive)
  (if (terminal-dedicated-exist-p)
      (let ((current-window (selected-window)))
        ;; Remember height.
        (terminal-dedicated-select)
        (terminal-dedicated-remember-window-height)
        ;; Close window.
        (if (and (require 'ecb nil t)
                 ecb-activated-window-configuration)
            ;; Toggle ECB window when ECB window activated.
            (progn
              (ecb-deactivate)
              (ecb-activate))
          ;; Otherwise delete dedicated window.
          (delete-window terminal-dedicated-window)
          (if (terminal-window-exist-p current-window)
              (select-window current-window))))
    (message "`terminal' window is not exist.")))

(defun terminal-dedicated-remember-window-height ()
  "Remember window height."
  (let ((win-height (terminal-current-window-take-height)))
    (if (and (terminal-dedicated-window-p) ;in `terminal' window
             (> win-height 1)
             (<= win-height terminal-dedicated-max-window-height))
        (setq terminal-dedicated-window-height win-height))))

;;;###autoload
(defun terminal-dedicated-toggle ()
  "Toggle dedicated `terminal' window."
  (interactive)
  (if (terminal-dedicated-exist-p)
      (progn
        (terminal-dedicated-close)
        (if (and terminal-dedicated-close-back-to-open-buffer-p
                 terminal-dedicated-close-buffer)
            (switch-to-buffer terminal-dedicated-close-buffer)
          ))
    (if terminal-dedicated-close-back-to-open-buffer-p
        (setq terminal-dedicated-close-buffer (current-buffer)))
    (terminal-dedicated-open)
    (other-window 1)
    ))

;;;###autoload
(defun terminal-dedicated-select ()
  "Select the `terminal' dedicated window."
  (interactive)
  (if (terminal-dedicated-exist-p)
      (select-window terminal-dedicated-window)
    (message "`terminal' window is not exist.")))

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun term-send-return ()
  "Use term-send-raw-string \"\C-m\" instead term-send-input.
Because term-send-input have bug that will duplicate input when you C-a and C-m in terminal."
  (interactive)
  (term-send-raw-string "\C-m")
  )

(defun term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

(defun term-send-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun term-send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

(defun term-send-reverse-search-history ()
  "Search history reverse."
  (interactive)
  (term-send-raw-string "\C-r"))

(defun term-send-delete-word ()
  "Delete word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun term-send-quote ()
  "Quote the next character in term-mode.
Similar to how `quoted-insert' works in a regular buffer."
  (interactive)
  (term-send-raw-string "\C-v"))

(defun term-send-M-x ()
  "Type M-x in term-mode."
  (interactive)
  (term-send-raw-string "\ex"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilise Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun terminal-internal ()
  "Internal handle for `terminal' buffer."
  ;; Add customize keystroke with `term-mode-hook'
  (remove-hook 'term-mode-hook 'terminal-keystroke-setup)
  (add-hook 'term-mode-hook 'terminal-keystroke-setup)
  ;; Load term mode
  (term-mode)
  (term-char-mode)
  ;; Handle term buffer close
  (terminal-handle-close)
  ;; Handle `output' variable.
  (setq term-scroll-show-maximum-output terminal-scroll-show-maximum-output
        term-scroll-to-bottom-on-output terminal-scroll-to-bottom-on-output)
  ;; Add hook to be sure `term' quit subjob before buffer killed.
  (add-hook 'kill-buffer-hook 'terminal-kill-buffer-hook))

(defun terminal-switch-buffer (term-buffer default-dir)
  "If we are in `tramp-mode', switch to TERM-BUFFER based on DEFAULT-DIR."
  (switch-to-buffer term-buffer)
  ;; Just test tramp file when library `tramp' is loaded.
  (when (and (featurep 'tramp)
             (tramp-tramp-file-p default-dir))
    (with-parsed-tramp-file-name default-dir path
      (let ((method (cadr (assoc `tramp-login-program (assoc path-method tramp-methods)))))
        (term-send-raw-string (concat method " " (when path-user (concat path-user "@")) path-host "\C-m"))
        (term-send-raw-string (concat "cd '" path-localname "'\C-m"))))))

(defun terminal-get-buffer (&optional special-shell dedicated-window)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input.
If option DEDICATED-WINDOW is `non-nil' will create dedicated `terminal' window ."
  (with-temp-buffer
    (let ((shell-name (or terminal-program ;shell name
                          (getenv "SHELL")
                          (getenv "ESHELL")
                          "/bin/sh"))
          (index 1)                     ;setup new term index
          term-name)                    ;term name
      (if dedicated-window
          (setq term-name terminal-dedicated-buffer-name)
        ;; Compute index.
        (while (buffer-live-p (get-buffer (format "*%s<%s>*" terminal-buffer-name index)))
          (setq index (1+ index)))
        ;; switch to current local directory,
        ;; if in-existence, switch to `terminal-default-dir'.
        (cd (or default-directory (expand-file-name terminal-default-dir)))
        ;; adjust value N when max index of term buffer is less than length of term list
        (setq term-name (format "%s<%s>" terminal-buffer-name index)))
      ;; Try get other shell name if `special-shell' is non-nil.
      (if special-shell
          (setq shell-name (read-from-minibuffer "Run program: " shell-name)))
      ;; Make term, details to see function `make-term' in `term.el'.
      (if terminal-program-switches
          (make-term term-name shell-name nil terminal-program-switches)
        (make-term term-name shell-name)))))


(defun terminal-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun terminal-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'term-mode)
    ;; Quit the current subjob
    ;; when have alive process with current term buffer.
    ;; Must do this job BEFORE `terminal-switch-after-close' action.
    (when (term-check-proc (current-buffer))
      ;; Quit sub-process.
      (term-quit-subjob))
    ;; Remember dedicated window height.
    (terminal-dedicated-remember-window-height)
    (let ((killed-buffer (current-buffer)))
      ;; Try to switch other terminal buffer
      ;; when option `terminal-switch-after-close' is non-nil.
      (when terminal-switch-after-close
        (terminal-switch-internal terminal-switch-after-close 1))
      ;; Remove killed buffer from the buffer list if it's in there
      (setq terminal-buffer-list
            (delq killed-buffer terminal-buffer-list)))))

(defun terminal-switch (direction offset)
  "Switch `terminal' buffers.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (unless (terminal-switch-internal direction offset)
    (if terminal-try-create
        (progn
          (terminal)
          (message "Created a new `terminal' buffer."))
      (message "No `terminal' buffers exist."))))

(defun terminal-switch-internal (direction offset)
  "Internal `terminal' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (if terminal-buffer-list
      (let ((buffer-list-len (length terminal-buffer-list))
            (my-index (cl-position (current-buffer) terminal-buffer-list)))
        (if my-index
            (let ((target-index (if (eq direction 'NEXT)
                                    (mod (+ my-index offset) buffer-list-len)
                                  (mod (- my-index offset) buffer-list-len))))
              (switch-to-buffer (nth target-index terminal-buffer-list)))
          (switch-to-buffer (car terminal-buffer-list))))
    nil))

(defun terminal-keystroke-setup ()
  "Keystroke setup of `term-char-mode'.

By default, the key bindings of `term-char-mode' conflict with user's keystroke.
So this function unbinds some keys with `term-raw-map',
and binds some keystroke with `term-raw-map'."
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (cl-dolist (unbind-key term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `term-bind-key-alist'
    (cl-dolist (element term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))

(defun terminal-dedicated-handle-other-window-advice (activate)
  "Handle advice for function `other-window'.
If ACTIVATE is `non-nil', will enable advice
`terminal-dedicated-other-window-advice'.
Otherwise, disable it."
  (if activate
      (ad-enable-advice 'other-window 'after 'terminal-dedicated-other-window-advice)
    (ad-disable-advice 'other-window 'after 'terminal-dedicated-other-window-advice))
  (ad-activate 'other-window))

(defun terminal-current-window-take-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun terminal-dedicated-get-window ()
  "Get `terminal' dedicated window."
  (setq terminal-dedicated-window
        (split-window
         (selected-window)
         (- (terminal-current-window-take-height) terminal-dedicated-window-height))))

(defun terminal-dedicated-get-buffer-name ()
  "Get the buffer name of `terminal' dedicated window."
  (format "*%s*" terminal-dedicated-buffer-name))

(defun terminal-dedicated-exist-p ()
  "Return `non-nil' if `terminal' dedicated window exist."
  (and (terminal-buffer-exist-p terminal-dedicated-buffer)
       (terminal-window-exist-p terminal-dedicated-window)))

(defun terminal-window-exist-p (window)
  "Return `non-nil' if WINDOW exist.
Otherwise return nil."
  (and window (window-live-p window)))

(defun terminal-buffer-exist-p (buffer)
  "Return `non-nil' if `BUFFER' exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun terminal-dedicated-window-p ()
  "Return `non-nil' if current window is `terminal' dedicated window.
Otherwise return nil."
  (equal (terminal-dedicated-get-buffer-name) (buffer-name (window-buffer))))

(defun terminal-window-dedicated-only-one-p ()
  "Only have one non-dedicated window."
  (interactive)
  (let ((window-number 0)
        (dedicated-window-number 0))
    (walk-windows
     (lambda (w)
       (with-selected-window w
         (cl-incf window-number)
         (if (window-dedicated-p w)
             (cl-incf dedicated-window-number)))))
    (if (and (> dedicated-window-number 0)
             (= (- window-number dedicated-window-number) 1))
        t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice delete-other-windows (around terminal-delete-other-window-advice activate)
  "This is advice to make `terminal' avoid dedicated window deleted.
Dedicated window can't deleted by command `delete-other-windows'."
  (let ((terminal-dedicated-active-p (terminal-window-exist-p terminal-dedicated-window)))
    (if terminal-dedicated-active-p
        (let ((current-window (selected-window)))
          (cl-dolist (win (window-list))
            (when (and (window-live-p win)
                       (not (eq current-window win))
                       (not (window-dedicated-p win)))
              (delete-window win))))
      ad-do-it)))

(defadvice delete-window (before terminal-delete-window-advice activate)
  "Use `delete-window' delete `terminal' dedicated window.
Have same effect as command `terminal-dedicated-close'.
This advice to remember `terminal' dedicated window height before deleting."
  ;; Remember window height before deleted.
  (terminal-dedicated-remember-window-height))

(defadvice pop-to-buffer (before terminal-pop-to-buffer-advice activate)
  "This advice fix the problem between `pop-to-buffer' and dedicated window.
By default, function `display-buffer' can't display buffer in selected window
if current window is `dedicated'.

So function `display-buffer' conflicts with `sr-speedbar' window, because
`sr-speedbar' window is a `dedicated' window.

That is to say, when current frame just have one `non-dedicated' window,
any functions that uses `display-buffer' can't split windows
to display buffer, even when the option `pop-up-windows' is enabled.

And the example function that can induce the problem is `pop-to-buffer'.

This advice will fix this problem when current frame just have one `non-dedicated' window."
  (when (and pop-up-windows             ;`pop-up-windows' is enable
             (terminal-window-dedicated-only-one-p) ;just have one `non-dedicated' window.
             (terminal-window-exist-p terminal-dedicated-window)
             (not (terminal-dedicated-window-p))) ;not in `sr-speedbar' window
    (split-window-vertically)
    (windmove-down)))

(defadvice other-window (after terminal-dedicated-other-window-advice)
  "Default, can use `other-window' select window in cyclic ordering of windows.
But sometimes we don't want to select `sr-speedbar' window,
but use `other-window' and just make `terminal' dedicated
window as a viewable sidebar.

This advice can make `other-window' skip `terminal' dedicated window."
  (let ((count (or (ad-get-arg 0) 1)))
    (when (and (terminal-window-exist-p terminal-dedicated-window)
               (eq terminal-dedicated-window (selected-window)))
      (other-window count))))

(provide 'terminal)

;; Local Variables:
;; time-stamp-line-limit: 10
;; time-stamp-start: "Last-Updated: <"
;; time-stamp-end: ">"
;; End:

;;; terminal.el ends here

;;; LocalWords:  multi el dir sr Hawley eb ef cd
