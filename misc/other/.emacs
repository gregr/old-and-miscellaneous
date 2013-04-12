;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

(setq x-select-enable-clipboard t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

(column-number-mode 1)

(setq scroll-step 1)

(mouse-wheel-mode t)

(defun matches-file ()
  (interactive)
  (if (verify-visited-file-modtime (current-buffer))
      (message "current buffer MATCHES file")
    (message "current buffer DIFFERS from file")))

;; force special modes for these file-types
(push (cons "\\.h\\'"  'c++-mode) auto-mode-alist)
(push (cons "\\.kid\\'"  'html-mode) auto-mode-alist)
(push (cons "\\.css\\'" 'c++-mode) auto-mode-alist)
(push (cons "\\.js\\'" 'c++-mode) auto-mode-alist)

;(load "~/lib/emacs/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(push (cons "\\.py\\'" 'python-mode) auto-mode-alist)
;(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-variable-buffer-local 'beginning-of-defun-function)
		 'py-beginning-of-def-or-class)
	    (setq outline-regexp "def\\|class ")))

;; color schemes
(defun lightbg ()
  (interactive)
  (set-foreground-color "black")
  (set-background-color "white")
  (set-face-background 'region "blue")
  (set-cursor-color "green"))

(defun darkbg ()
  (interactive)
  (set-foreground-color "white")
  (set-background-color "black")
  (set-face-background 'region "blue")
  (set-cursor-color "green"))

(defun lightcs ()
  (interactive)
  (lightbg)
  (mapcar '(lambda (cp) (set-face-foreground (car cp) (cadr cp)))
	  '((font-lock-builtin-face "DarkBlue")
	    (font-lock-comment-face "bisque4")
	    (font-lock-constant-face "DarkRed")
	    (font-lock-function-name-face "Blue")
	    (font-lock-keyword-face "RoyalBlue")
	    (font-lock-string-face "DarkViolet")
	    (font-lock-type-face "Black")
	    (font-lock-variable-name-face "firebrick")
	    (font-lock-warning-face "darkolivegreen"))))
;(font-lock-highlighting-face "gray")
;(font-lock-reference-face "MediumPurple")


(defun greencs ()
  (interactive)
  (darkbg)
  (mapcar '(lambda (cp) (set-face-foreground (car cp) (cadr cp)))
	  '((font-lock-builtin-face "brown1")
	    (font-lock-comment-face "gray50")
	    (font-lock-constant-face "cyan")
	    (font-lock-function-name-face "dark turquoise")
	    (font-lock-keyword-face "lime green")
	    (font-lock-string-face "goldenrod")
	    (font-lock-type-face "dark turquoise")
	    (font-lock-variable-name-face "light goldenrod")
	    (font-lock-warning-face "red"))))

(defun goldcs ()
  (interactive)
  (darkbg)
  (mapcar '(lambda (cp) (set-face-foreground (car cp) (cadr cp)))
	'((font-lock-builtin-face "brown1")
	  (font-lock-comment-face "gray50")
	  (font-lock-constant-face "cyan")
	  (font-lock-function-name-face "dark turquoise")
	  (font-lock-keyword-face "light goldenrod")
	  (font-lock-string-face "slate blue")
	  (font-lock-type-face "dark turquoise")
	  (font-lock-variable-name-face "lime green")
	  (font-lock-warning-face "red"))))

(goldcs) ; use this colorscheme by default

;; header/source toggling
(defun jba-c++-switch-to-other-source-file ()
  (interactive)
  (let* ((basename (file-name-sans-extension buffer-file-name))
         (ext (file-name-extension buffer-file-name))
         (other-exts (if (string= ext "h") '("cpp" "cc") '("h")))
         (found nil))
    (while (and other-exts (not found))
      (let ((filename (format "%s.%s" basename (car other-exts))))
        (if (file-exists-p filename)
            (progn
              (find-file filename)
              (setq found t))
          (setq other-exts (cdr other-exts)))))
    (if (not found)
        (message "could not find other source file"))))

;; c stuff
(setq c-mode-hook
      '(lambda ()
         (setq c-basic-offset 4)
         (setq tab-width 8)
         (setq indent-tabs-mode nil) ; always use spaces, never tabs
         (push '(case-label . +) c-offsets-alist)
;        (define-key c-mode-map "\C-c\C-s" 'jba-shell)
         (abbrev-mode 1)
         ;(find-file-noselect "~/Code/jni-dabbrev-words.txt")
))

;; c++ stuff
(setq c++-mode-hook
      '(lambda ()
         (setq c-basic-offset 4)
         (setq tab-width 8)
         (setq indent-tabs-mode nil) ; always use spaces, never tabs
         (push '(case-label . +) c-offsets-alist)
         (push '(substatement-open . 0) c-offsets-alist)
;        (jba-setup-font-lock-mode)
;        (define-key c++-mode-map "\C-c\C-s" 'jba-shell)
         (define-key c++-mode-map [?\C-\;] 'jba-c++-switch-to-other-source-file)
         ; for putty, where C-; doesn't work:
         (define-key c++-mode-map [?\C-\\] 'jba-c++-switch-to-other-source-file)
;        (define-key c++-mode-map "\M-z" 'jba-c++-compile)
;        (define-key c++-mode-map "\C-c\C-u" '(lambda () (interactive) (insert "using namespace std;\n")))
;;       (define-key c++-mode-map "{"
;;         '(lambda () (interactive) (jba-java-open-brace t)))
;;       (define-key c++-mode-map [?\C-{]
;;         '(lambda () (interactive) (jba-java-open-brace nil)))

))

(iswitchb-mode)
