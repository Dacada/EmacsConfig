;; This file handles installing and doing any necessary initializations on installed packages.

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; configure straight.el
(setq straight-use-package-by-default t)

;; install use-package, which integrates with straight.el
(straight-use-package 'use-package)

;; now we can install all other packages...

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

(use-package treemacs
  :config (setq treemacs-show-hidden-files nil)
  :bind (("C-c t" . treemacs)))

(use-package python-pytest
  :config (setq python-pytest-executable "brazil-test-exec pytest --no-cov --disable-warnings"))

(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

(use-package magit
  :bind (("C-c g" . magit-status)))

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

(use-package highlight-indentation)

(use-package ido-vertical-mode)

(use-package python-docstring)

(use-package imenu-list)

(use-package company)

;; this gives a ton of warnings (at least on Emacs 29) but seems to be benign, it still loads
(use-package realgud)

(use-package treemacs-magit)

(use-package markdown-mode)

(use-package gptel
  :after markdown-mode)

(use-package eglot-booster
  :straight (eglot-booster
             :type git
             :host github
             :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))
