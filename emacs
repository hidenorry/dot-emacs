;;hidenorry
;;hidenorry@gmail.com

(eval-when-compile
  (require 'cl))

(defmacro add-elements-to-list (lis &rest elems)
  "add values to top of a list"
  (declare (indent 1))
  `(setq ,lis
         (append (list ,@elems) ,lis)))

(add-elements-to-list load-path
  "~/.emacs.d/elpa/"
  "~/.emacs.d/self-install/")

;(setq eshell-path-env
;      (concat "/opt/gaussian/gv:"
;              eshell-path-env))
;(setenv "PATH"
;        (concat "/opt/gaussian/gv:"
;                (getenv "PATH")))

;; use generic-mode instead of fundamental mode
;;when show the files (/etc/hosts, /var/log/http.conf, et al)
(require 'generic-x)

(defun os-is (name)
  (string-match name system-configuration))

(when (os-is "linux")
  (add-hook 'after-save-hook
            '(lambda ()
               (save-restriction
                 (widen)
                 (when (string= "#!" (buffer-substring 1 (min 3 (point-max))))
                   (let ((name (buffer-file-name)))
                     (or (char-equal ?. (string-to-char (file-name-nondirectory name)))
                         (let ((mode (file-modes name)))
                           (set-file-modes name (logior mode (logand (/ mode 4) 73)))
                           (message (concat "Wrote " name " (+x)"))))))))))

;;e-mail settings. call::"C-x m", send::"C-c C-c" kill::"C-c C-k"
(setq user-mail-address "hidenorry@gmail.com"
      user-full-name "hidenorry"
      smtpmail-smtp-server ""
      mail-user-agent 'message-user-agent
      message-send-mail-function 'message-smtpmail-send-it)


;; scroll settings
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
;; eshell-mode settings
(setq comint-scroll-show-maximum-output t
      comint-input-ignoredups t  ;ignore duplicates in a history
      eshell-glob-include-dot-dot nil ; ../ is removed from results of *
      eshell-history-file-name "~/.bash_history"
      eshell-history-size 10000
      eshell-scroll-to-bottom-on-output t
      eshell-scroll-show-maximum-output t
      comint-completion-addsuffix t 
      comint-completion-autolist t
      eshell-cmpl-cycle-completions nil
      eshell-cmpl-cycle-cutoff-length 100
      )


(defmacro define-multiple-keys (mapname &rest difinitions)
  "define key settings of arbitary key-map"
  `(progn
     ,@(mapcar (lambda (d) `(define-key ,mapname (kbd ,(first d)) ,(second d)))
                      difinitions)))
(define-multiple-keys global-map
  ("C-h" 'backward-delete-char)
  ("C-x C-b" 'ibuffer)
  ("C-x x C-c" 'save-buffers-kill-terminal)  
  ("C-x C-c" (lambda () (interactive) (values "dummy")))
  ("C-x x k" (lambda () (interactive) (kill-buffer (buffer-name))))
  ("C-M-h"   (lambda () (interactive) (move-to-window-line 0)))
  ("C-M-m"   (lambda () (interactive) (move-to-window-line nil)))
  ("C-M-l"   (lambda () (interactive) (move-to-window-line -1)))
  ;("M-g" (lambda (x) (interactive "nLine: ") (goto-line x)))
  ("C-x p" (lambda () (interactive) (other-window -1)))
  ;("C-M-SPC" 'mark-sexp)
  ("C-o" (lambda () (interactive) (other-window 1)))
  ("M-%" 'query-replace-regexp)
  ("C-M-%" 'query-replace)
  ("C-s" 'isearch-forward-regexp)
  ("C-M-s" 'isearch-forward)
  ("C-r" 'isearch-backward-regexp)
  ("C-M-r" 'isearch-barrier)
  )
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
 
(show-paren-mode t)
(setq show-paren-style 'mixed)
(iswitchb-mode t) ;use iswitchb instead of switch-to
(line-number-mode t)
(column-number-mode t)
;;enable (Tool|Menu) Bar mode if ARG is positive, and disable it otherwise
(tool-bar-mode -1) ;don't show too-bar 
(menu-bar-mode -1) ;don't show menue-bar
(find-function-setup-keys) ;setup find-function => "C-x F":find-function
(setq large-file-warning-threshold (* 1024 1024 256)) ;256MB
(setq-default tab-width 2 indent-tabs-mode nil)
(global-font-lock-mode t)

;; show line feed code
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; color setting for rgexp
(set-face-foreground 'font-lock-regexp-grouping-backslash "green3")
(set-face-foreground 'font-lock-regexp-grouping-construct "green")

;;(which-func-mode 1)
;;(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
;;(setq-default header-line-format '(which-func-mode "" which-func-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro require-when-exist (reqlis &rest body)
  (declare (indent 0))
  (let ((lib-name (gensym)))
    `(let ((,lib-name ,(format "%s" (second (second reqlis)))))
       (if (locate-library ,lib-name)
           (progn ,reqlis
                  ,@body)
         (message (format "cannot find `%s' and skip it." ,lib-name))))))

(require-when-exist
  (require 'open-junk-file)
  (defvar switch-to-buffer-other-window-display-function nil)
  (defadvice switch-to-buffer-other-window (around display-customize activate)
    (if switch-to-buffer-other-window-display-function
        (apply switch-to-buffer-other-window-display-function (ad-get-args 0))
      ad-do-it))
  (defun open-junk-file-here ()
    (interactive)
    (let ((switch-to-buffer-other-window-display-function 'switch-to-buffer))  
      (open-junk-file)))
  ;(ad-disable-advice 'switch-to-buffer-other-window 'around 'display-customize)
  ;(ad-activate 'switch-to-buffer-other-window)
  (define-key global-map (kbd "C-x C-z") 'open-junk-file-here))

;; add a repository to a repository alist of `package-list-packages'
(require-when-exist
 (require 'package)
 (add-to-list 'package-archives
	      '("marmalade" .
		"http://marmalade-repo.org/packages/"))
 (package-initialize))


;;Instead of adding a number to the buffer name,
;;adding a directory name if you make a new one.
(require-when-exist
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(require-when-exist
  (require 'w3m-load))

;; org mode settings originating from 'Emacs technic bible'
(require-when-exist
  (require 'org)
  (defun org-insert-upheading (arg)
    "insert to upper level"
    (interactive "P")
    (org-insert-heading arg)
    (cond ((org-on-heading-p) (org-do-promote))
          ((org-at-item-p) (org-indent-item -1))))
  (defun org-insert-heading-dwim (arg)
    (interactive "p")
    (case arg
      (4  (org-insert-subheading nil))  ;C-u
      (16 (org-insert-upheading nil))   ;C-u C-u
      (t (org-insert-heading nil))))
  (define-key org-mode-map (kbd "<C-return>") 'org-insert-heading-dwim)

  ;;memoize
  (org-remember-insinuate)
  ;; for org file
  (setq org-directory "~/mem/")
  (setq org-default-notes-file (expand-file-name "memo.org" org-directory))
  ;; for templete
  (setq org-remember-templates
        '(("Note" ?n "** %?\n   %i\n   %a\n   %t" nil "Inbox")
          ("Todo" ?t "** TODO %?\n  %i\n  %a\n  %t" nil "Inbox")))
  ;; Select template: [n]ote [t]odo
  (global-set-key (kbd "M-g m") 'org-remember)

  ;;hyper link
  (global-set-key (kbd "C-c l") 'org-store-link)
  )


(require-when-exist
  (require 'undo-tree)
  (undo-tree-mode))

;; insert evaluated values following the literal `;=>'
(require-when-exist
  (require 'lispxmp)
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp))

;;(load "quack")
(require-when-exist
  (require 'quack)
  (setq quack-pretty-lambda-p t))

;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require-when-exist
  (require 'auto-complete-config)
  (ac-config-default))

;; use undo-tree. ("C-x u")
(require-when-exist
  (require 'undo-tree)
  (global-undo-tree-mode))

(require-when-exist
  (require 'popwin)
  (setq display-buffer-function 'popwin:display-buffer
        ;;popwin:popup-window-width 80
        popwin:popup-window-height 10
        popwin:popup-window-position 'bottom))

(defmacro add-to-add-hook (hooks &rest body)
  `(progn
     ,@(loop for hook in hooks
                  collect
                       `(add-hook ,hook ,@body))))

(require-when-exist
  (require 'paredit)
  (define-key global-map (kbd "M-s") 'paredit-splice-sexp)
  (add-to-add-hook 
    ('emacs-lisp-mode-hook
     'scheme-mode-hook
     'lisp-mode-hook
     )
    'enable-paredit-mode))

;; (require-when-exist
;;  (require 'bm)
;;  (setq-default bm-buffer-persistence nil)
;;  (setq bm-restore-repository-on-load t)
;;  (add-to-add-hook  ('find-file-hook
;;                     'after-revert-hook)
;;                    'bm-buffer-restore)
;;  (add-to-add-hook  ('kill-buffer-hook
;;                     'after-save-hook
;;                     'vc-before-checkin-hook)
;;                    'bm-buffer-save)
;;  (define-multiple-keys global-map
;;    ("M-SPC" 'bm-toggle)
;;    ("M-[" 'bm-previous)
;;    ("M-]" 'bm-next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;setting of ElDoc 
;; (add-to-add-hook
;;   ('emacs-lisp-mode-hook
;; ;;  'scheme-mode-hook
;; ;;  'lisp-mode-hook
;;    'lisp-interaction-mode-hook
;;    'ielm-mode-hook
;;    )
;;    'turn-on-eldoc-mode)
;; (setq eldoc-idele-delay 0.2) ; short delay
;; (setq eldoc-minor-mode-string "") ;don't show message 'ElDoc'

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(define-key global-map (kbd "C-o") 'other-window-or-split)


(setq bookmark-save-flag 1)
(progn
 (setq bookmark-sort-flag nil)
 (defun bookmark-arrange-latest-top ()
   (let ((latest (bookmark-get-bookmark bookmark)))
     (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
   (bookmark-save))
 (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))


;; create tags file automatically
(when (os-is "linux")
  (defadvice find-tag (before c-tag-file activate)
    "Automatically create tags file."
    (let ((tag-file (concat default-directory "TAGS")))
      (unless (file-exists-p tag-file)
        (when (y-or-n-p "No TAGS file. Make it?")
          (shell-command "etags *.[ch] *.scm *.f *.f90 *.el .*.el 2>/dev/null")))
      (visit-tags-table tag-file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yank-pop/anything ()
  "use anything-show-kill-ring instead of yank-pop"
  (interactive)
  (cond ((eq last-command 'yank)
          (anything-show-kill-ring))
        ((eq last-command 'yank-pop/anything)
          (anything-next-line))
        (t (yank-pop))))
(define-key global-map (kbd "M-y") 'yank-pop/anything)


;; Read Only Setteings
(defun set-read-only () (toggle-read-only 1))

;; Outputfile or logfile will be opend with read-only.
(defun gaussian-file ()
  "for gaussin output files"
  (when (string-match "\\.\\(out\\|log\\)$" buffer-file-name)
    (set-read-only)))
(add-hook 'find-file-hook 'gaussian-file)

(defadvice find-function (after ad-find-function activate)
  (set-read-only))
(defadvice find-tag (after ad-find-function activate)
  (set-read-only))


(defun emacs-setting-file-hook ()
  (when (string-match "emacs$" (buffer-file-name))
    (emacs-lisp-mode)))
(add-hook 'find-file-hook 'emacs-setting-file-hook)


;; C-x r w 'registar' ;=>save window configuration
;; C-x r j 'registar' ;=>load window configuration
(global-unset-key (kbd "C-x r w"))
(lexical-let ((wc-alist nil))
  (defun save-windows (key)
    (interactive "cQuasi-Resister : ")
    (push (cons key (current-window-configuration)) wc-alist))
  (defun load-windows (key)
    (interactive "cQuasi-Resister : ")
    (set-window-configuration
     (cdr (assoc key wc-alist)))))
(define-key global-map (kbd "C-x r w") 'save-windows)
(define-key global-map (kbd "C-x r q") 'load-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; settings for scheme implementation Gauche
(modify-coding-system-alist 'process "gosh" '(utf-8 . utf-8))
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*")
   (run-scheme scheme-program-name))
  (other-window -1))
(define-key global-map (kbd "C-c s") 'scheme-other-window)

;; ;;dictionary
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (add-to-list 'ac-modes 'scheme-mode)
;; ;;; from shiro
;; (defun gauche-info-index (topic)
;;   (interactive
;;    (list (read-string
;;             (concat "Gauche help topic : ")
;;           (current-word))))
;;   (switch-to-buffer-other-window (get-buffer-create "*info*"))
;;   (info "/usr/share/info/gauche-refe.info.gz")
;; ;;(info "/usr/local/share/info/gauche-refe.info.gz") ;;/usr/local/
;;   (Info-index topic))
;; ;;(define-key global-map (kbd "C-x H") 'gauche-info-index) ;; original
;; (define-key global-map (kbd "C-x C-j") 'gauche-info-index) ;; naoya_

(defun split-at (lis num)
  (labels ((rec (n lis acc)
                (if (or (<= n 0) (null lis))
                    (values (reverse acc) lis)
                  (rec (1- n) (cdr lis) (cons (car lis) acc)))))
    (rec num lis nil)))
;(split-at 9 '(1 2 3 4 5))

(defun group (lis num)
  (labels ((rec (lis acc)
                (if (null lis)
                    (reverse acc)
                  (multiple-value-bind (r f) (split-at lis num)
                    (rec f (cons r acc))))))
    (rec lis nil)))

(defmacro define-indent (name &rest alist)
  `(progn
     ,@(mapcar (lambda (l) `(put ',(first l) ,name ,(second l)))
                      alist)))
(define-indent 'scheme-indent-function
  (and-let* 1)
  (begin0 0)
  (call-with-client-socket 1)
  (call-with-input-conversion 1)
  (call-with-input-file 1)
  (call-with-input-process 1)
  (call-with-input-string 1)
  (call-with-iterator 1)
  (call-with-output-conversion 1)
  (call-with-output-file 1)
  (call-with-output-string 0)
  (call-with-temporary-file 1)
  (call-with-values 1)
  (dolist 1)
  (dotimes 1)
  (if-match 2)
  (let*-values 1)
  (let-args 2)
  (let-keywords* 2)
  (let-match 2)
  (let-optionals* 2)
  (let-syntax 1)
  (let-values 1)
  (let/cc 1)
  (let1 2)
  (letrec-syntax 1)
  (make 1)
  (multiple-value-bind 2)
  (match 1)
  (parameterize 1)
  (parse-options 1)
  (receive 2)
  (rxmatch-case 1)
  (rxmatch-cond 0)
  (rxmatch-if 2)
  (rxmatch-let 2)
  (syntax-rules 1)
  (unless 1)
  (until 1)
  (when 1)
  (while 1)
  (with-builder 1)
  (with-error-handler 0)
  (with-error-to-port 1)
  (with-input-conversion 1)
  (with-input-from-port 1)
  (with-input-from-process 1)
  (with-input-from-string 1)
  (with-iterator 1)
  (with-module 1)
  (with-output-conversion 1)
  (with-output-to-port 1)
  (with-output-to-process 1)
  (with-output-to-string 1)
  (with-port-locking 1)
  (with-string-io 1)
  (with-time-counter 1)
  (with-signal-handlers 1)
  (with-locking-mutex 1)
  (guard 1))

(global-font-lock-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;common lisp setting
;;PATH of ccl
(require-when-exist
  (require 'slime)
  (require 'lisp-mode)
  (cond ((os-is "mingw")
         (setq inferior-lisp-program "wx86cl.exe")
         (autoload 'lisp-mode "Clozure Common Lisp" "Major mode for Common Lisp." t)
         ;(setq slime-net-coding-system 'sjis-dos)
         )
        (t
         (setq inferior-lisp-program "ccl")
         (autoload 'lisp-mode "Clozure Common Lisp" "Major mode for Common Lisp." t)
         (setq slime-net-coding-system 'utf-8-unix)
         (add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))))
  (slime-setup '(slime-repl slime-fancy slime-banner))
  (put 'upcase-region 'disabled nil)
  (add-hook 'lisp-mode-hook
            (lambda ()
              (slime-mode t)
              (show-paren-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fortran(F77)
(add-hook 'fortran-mode-hook
          '(lambda () (setq
                       fortran-do-indent 2
                       fortran-if-indent 2
                       fortran-program-indent 2
                       fortran-continuation-indent 2
                       )
             (turn-on-font-lock)))
;; Fortran(f90 or later)
(add-hook 'f90-mode-hook
          '(lambda () (setq
                       f90-do-indent 2
                       f90-if-indent 2
                       f90-program-indent 2
                       f90-continuation-indent 2
                       )
             (turn-on-font-lock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; start up 
(cond ((os-is "mingw")
       (set-language-environment 'Japanese)           
       (setenv "USERPROFILE" "C:/Users/emacs-home/")
       (prefer-coding-system 'sjis-dos)
       (add-elements-to-list initial-frame-alist
         '(width . 145)
         '(height . 27))
       (add-hook 'after-init-hook
                 (lambda ()
                   ;; window configuration setteings
                   (save-selected-window
                     (slime)
                     (delete-other-windows)
                     (select-window (split-window-horizontally -60))
                     (eshell))))
       (setq default-frame-alist initial-frame-alist))
      (t (prefer-coding-system 'utf-8)
         (eshell)
         (eshell 2)))
