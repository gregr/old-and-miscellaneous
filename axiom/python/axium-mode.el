;; insert the following into .emacs; note the path may be different for you
;; (setq-default indent-tabs-mode nil) ; will set up a hook for this later...
;; (setq load-path
;;       (cons (expand-file-name "~/svn/uriel/axiom/python/") load-path))
;; (require 'axium-mode)
;; (add-to-list 'auto-mode-alist '("\\.ax\\'" . axium-mode))

;; (defvar axium-mode-hook nil)

;; (defvar axium-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key map "\C-j" 'newline-and-indent)
;;     map)
;;   "Axium mode keymap")

(defvar axium-tab-width 2) ; how to use default-tab-width better?

(defun axium-wrap-angle (s) (concat "\\<" s "\\>"))

;; (defvar axium-keywords
;;   "\\<\\(__\\(?:abstract\\|ca\\(?:ll\\|se\\)\\|def_\\(?:syntax\\|var\\)\\|let\\(?:rec\\)?\\|macro\\)\\|operators?\\)\\>")

(defvar axium-builtins
  (axium-wrap-angle (regexp-opt '("__def_var" "__def_syntax" "__data"
				  "__abstract" "__macro"
				  "__switch" "__prim_case" "__case" "__let" "__letrec"
                                  "__pack" "__unpack" "__tagswitch"
				  "__call" "__unboxed" "__type_tagged") t)))
(defvar axium-keywords
  (axium-wrap-angle (regexp-opt '("__operator" "prefix" "infixl" "infixr"
                                  "infixtl" "infixtr"
                                  "quote" "close_syntax" "squote"
				  "lambda" "defun" "defsyn" "defmac"
                                  "if" "throw"
                                  "print") t)))

;; (defvar axium-keywords
;;   (regexp-opt '("__def_var" "__def_syntax" "__abstract"
;; 		"__macro" "__case" "__let" "__letrec"
;; 		"__call" "operator" "operators") t))

;; (defconst axium-font-lock-keywords
;;   (list
;;    '("" . font-lock-builtin-face)
;;    '("\\('\\w*'\\)" . font-lock-variable-name-face)
;;    '(axium-keywords . font-lock-keyword-face)
;; ;   '("" . font-lock-constant-face)
;;    '("\\(##.*\\)" . 'font-lock-comment-face)
;;    )
;;   "Axium mode expression highlighting")

(defvar axium-font-lock-keywords
  (list
;;   '("\\(\\sw+\\)" 1 font-lock-variable-name-face)
;   '("\\<\\(w+\\)\\>" 1 font-lock-variable-name-face)
   `(,axium-builtins 1 font-lock-builtin-face)
   `(,axium-keywords 1 font-lock-keyword-face)
   '("\\(`\\([a-zA-Z_]\\|\\(\\\\.\\)\\)\\([[:word:]]\\|\\(\\\\.\\)\\)*`\\)" 1 font-lock-variable-name-face)
   '("\\([`~!@$%^&*\\=+|;:,.<>/?-]\\)" 1 font-lock-variable-name-face)
   '("\\<\\([A-Z][A-Za-z0-9]+\\)" 1 font-lock-type-face)
;;    '("\\([`~!@$%^&*\\=+|;:,.<>/?-]\\)" 1 font-lock-function-name-face)
;;   '("\\<\\([0-9]+\\)\\>" . font-lock-constant-face)
   '("\\(##.*\\)" 0 'font-lock-comment-face)
   )
  "Axium mode expression highlighting")

(defun axium-nulldef (x def-x) (if (null x) def-x x))
(defun axium-cardef (xs def-x) (if (null xs) def-x (car xs)))

(defun axium-prev-cols ()
  ""
  (defun axium-prev-cols-inner (margin)
    (forward-line -1)
    (if (bobp) (list 0)
      (let ((cur (min (axium-nulldef margin (current-indentation))
		      (current-indentation))))
	(if (> cur 0)
	    (let ((next (axium-prev-cols-inner cur)))
	      (if (eq cur (car next)) next (cons cur next)))
	  (list 0)))))
  (save-excursion (axium-prev-cols-inner '())))

(defun axium-find-col (cur cols)
  ""
  (if (null cols) cols
    (if (eq cur (car cols)) (cdr cols)
      (axium-find-col cur (cdr cols)))))

(defun axium-next-indent ()
  ""
  (let* ((cur (current-indentation)) (cols (axium-prev-cols))
	 (most (+ (axium-cardef cols (- 0 axium-tab-width))
		  axium-tab-width))
	 (next (axium-cardef (axium-find-col cur (cons most cols)) most)))
    next))

(defun axium-indent-line ()
  "Axium mode indentation"
  (interactive)
  (if (bobp) (indent-line-to 0)
    (indent-line-to (axium-next-indent))))

(defvar axium-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

;; (defun axium-mode ()
;;   (interactive)
;;   (kill-all-local-variables)
;;   (use-local-map axium-mode-map)
;; ;  (set-syntax-table axium-mode-syntax-table)
;; ;  (set (make-local-variable 'font-lock-defaults) '(axium-font-lock-keywords))
;;   (set (make-local-variable 'indent-line-function) 'axium-indent-line)
;;   (setq major-mode 'axium-mode)
;;   (setq mode-name "Axium")
;;   (run-hooks 'axium-mode-hook))

(define-derived-mode axium-mode fundamental-mode "Axium"
   "Major mode for editing .ax files"
   :syntax-table axium-mode-syntax-table
   (set (make-local-variable 'comment-start) "## ")
   (set (make-local-variable 'comment-start-skip) "##+\\s-*") ; todo?
;;    (set (make-local-variable 'font-lock-keywords)
;; 	'(axium-font-lock-keywords))
   (set (make-local-variable 'font-lock-defaults)
	'(axium-font-lock-keywords))
   (set (make-local-variable 'indent-line-function) 'axium-indent-line)
;;    (set (make-local-variable 'imenu-generic-expression)
;; 	axium-imenu-generic-expression)
;;   (set (make-local-variable 'outline-regexp) axium-outline-regexp)
   )

(provide 'axium-mode)
