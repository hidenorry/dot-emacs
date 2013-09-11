;; settings for programing languages
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

(font-lock-add-keywords 'scheme-mode 
'(
  (",|.*?|" . 'font-lock-variable-name-face)
  (" $..*?\\( \\|)\\)" . 'font-lock-variable-name-face)))


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
;; (add-hook 'scheme-mode-hook
;;           '(lambda ()
;;              (font-lock-mode 1)
;;              ; キーワード用にFaceを作成 (1)
;;              (make-face 'emphasis-face)
;;              (set-face-foreground 'emphasis-face "red")
;;              ; キーワード定義 (2)
;;              (setq font-lock-keywords
;;                    (append
;;                     '((",|\\(.*\\)|" 1 emphasis-face)
;;                       ("device" 0 emphasis-face))
;;                     font-lock-keywords))
;;              (font-lock-fontify-buffer)))

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

;perl setteings
(add-hook 'perl-mode-hook
          #'(lambda () (flymake-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;haskell settings
(require-when-exist
  (require 'haskell-mode)
  (require 'haskell-cabal)
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
  ;;for #!/usr/bin/env runghc
  (add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     
  ;;for #!/usr/bin/env runhaskell
  (add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) 
  (autoload 'ghc-init "ghc" nil t)
  (add-hook 'haskell-mode-hook
            (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;verilog settings
(add-hook 'verilog-mode-hook
 '(lambda ()
    (define-multiple-keys verilog-mode-map
      ("C-h" 'backward-delete-char)
      ("C-x C-b" 'ibuffer)
      ("C-x x C-c" 'save-buffers-kill-terminal)  
      ("C-x C-c" (lambda () (interactive) (values "dummy")))
      ("C-x x k" (lambda () (interactive) (kill-buffer (buffer-name))))
      ("C-M-h"   (lambda () (interactive) (move-to-window-line 0)))
      ("C-M-m"   (lambda () (interactive) (move-to-window-line nil)))
      ("C-M-l"   (lambda () (interactive) (move-to-window-line -1)))
      ;;("M-g" (lambda (x) (interactive "nLine: ") (goto-line x)))
      ("C-x p" (lambda () (interactive) (other-window -1)))
      ;;("C-M-SPC" 'mark-sexp)
      ("C-o" (lambda () (interactive) (other-window 1)))
      ("M-%" 'query-replace-regexp)
      ("C-M-%" 'query-replace)
      ("C-s" 'isearch-forward-regexp)
      ("C-M-s" 'isearch-forward)
      ("C-r" 'isearch-backward-regexp)
      ("C-M-r" 'isearch-barrier)
      )))

 (add-to-list 'hs-special-modes-alist '(my-mode "{{{" "}}}" ...)) 
;; doesn't work
;; (add-hook 'verilog-mode-hook
;;           '(lambda ()
;;             (hs-minor-mode 1)))
;; (let ((verilog-mode-hs-info
;;        '(verilog-mode
;;           "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do"
;;           "end"
;;           "#"
;;           forward-word
;;           nil)))
;;   (if (not (member verilog-mode-hs-info hs-special-modes-alist))
;;       (setq hs-special-modes-alist
;;             (cons verilog-mode-hs-info hs-special-modes-alist))))


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
