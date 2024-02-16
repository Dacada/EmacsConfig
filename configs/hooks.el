;; highlight indent
(add-hook 'prog-mode-hook #'highlight-indentation-mode)

;; line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; automatic completion
(add-hook 'prog-mode-hook #'company-mode)

;; show current function in mode line
(add-hook 'prog-mode-hook #'which-function-mode)

;; make colors work in compilation buffer
(add-hook 'compilation-filter-hook
	  (lambda ()
	    (ansi-color-apply-on-region compilation-filter-start (point))))

;; start eglot with python mode
(add-hook 'python-mode-hook #'eglot-ensure)

;; make python mode separate comments from code according to pep8
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'comment-inline-offset) 2)))

;; syntax highlighting in python docstrings and correct automatic wrapping
(add-hook 'python-mode-hook
	  (lambda ()
	    (python-docstring-mode)))
