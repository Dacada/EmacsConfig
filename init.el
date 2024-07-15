;;; -*- lexical-binding: t; -*-

;; See https://github.com/a13/emacs.d for inspiration

;; External dependencies:
;;   - Cascadia Code font must be installed: https://github.com/microsoft/cascadia-code/releases

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
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Garbage collector improvements
(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))

;; Potentially slightly improve startup time
(use-package fnhh
  :straight
  (:host github :repo "a13/fnhh")
  :config
  (fnhh-mode 1))

;; Add a custom-update keyword to use-package (see https://github.com/a13/use-package-custom-update)
(use-package use-package-custom-update
  :straight
  (:host github :repo "a13/use-package-custom-update"))

;; Anything defined in the C code, use emacs pseudo-package to set it up
(use-package emacs
  :custom
  (frame-resize-pixelwise t "Resize frames pixelwise, obviously")
  (default-frame-alist '((menu-bar-lines 0)
                         (tool-bar-lines 0)
                         (vertical-scroll-bars)) "No menus, no tool bars, yes scroll bars")
  (scroll-conservatively 101 "Scroll one line at at ime")
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes even when using the mouse")
  (x-gtk-use-system-tooltips nil "Don't use the gtk flavor of tooltip")
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (tab-width 4 "And if tabs, 4 spaces wide")
  (debug-on-quit nil "Set this to t to enter the debugger on C-g")
  :custom-face
  (default ((t (:font "Cascadia Code")))))  ;; Use Cascadia Code font by default

;; Enable ligatures, snippet from https://github.com/mickeynp/ligature.el?tab=readme-ov-file#example-font-configuration-cascadia-code
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Disable suspending on C-z
(use-package frame
  :straight nil
  :bind
  ("C-z" . nil)
  :custom
  (initial-frame-alist '((vertical-scroll-bars))))

;; Remap list-buffers to use ibuffer instead
(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

;; On save, delete trailing whitespace and add final newline
;; Fix the backup settings to not be infuriating
(use-package files
  :straight nil
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (require-final-newline t)
  ;; backup settings
  (backup-by-copying t)
  (backup-directory-alist
   `((".*" . ,(locate-user-emacs-file "backups"))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

;; We don't use Customize in this house
(use-package cus-edit
  :straight nil
  :defer t
  :custom
  ;; (custom-file (make-temp-file "emacs-custom") "Store customizations in a temp file")
  (custom-file null-device "Don't store customizations"))

;; Use the forward style to uniquify buffer names that visit identically named files
(use-package uniquify
  :straight nil
  :defer 0.1
  :custom
  (uniquify-buffer-name-style 'forward))

;; Bind finding a file as root to C-x M-s
(use-package sudo-edit
  :ensure t
  :config (sudo-edit-indicator-mode)
  :bind (:map ctl-x-map
              ("M-s" . sudo-edit)))

;; Get the user shell's PATH variable for Emacs to use
(use-package exec-path-from-shell
  :ensure t
  :defer 0.1
  :config
  (exec-path-from-shell-initialize))

;; eshell stuff
(use-package em-smart
  :straight nil
  :defer t
  :config
  (eshell-smart-initialize)
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))
(use-package esh-help
  :ensure t
  :defer t
  :config
  (setup-esh-help-eldoc))
(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))
(use-package eshell-prompt-extras
  :ensure t
  :after (eshell esh-opt)
  :custom
  (eshell-prompt-function #'epe-theme-dakrone))

;; Almost smooth mouse scrolling
(use-package mwheel
  :straight nil
  :custom
  (mouse-wheel-scroll-amount '(1
                               ((shift) . 5)
                               ((control))))
  (mouse-wheel-progressive-speed nil))
(use-package pixel-scroll
  :straight nil
  :config
  (pixel-scroll-mode))

;; cool mode line inspired by doom emacs
(use-package mood-line
  :ensure t
  :custom-face
  (mode-line ((t (:inherit default (:box (:line-width -1 :style released-button))))))
  :hook
  (after-init . mood-line-mode))

;; Highlight the current line in programming modes
(use-package hl-line
  :hook
  (prog-mode . hl-line-mode))

;; Highlight TODOs
(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

;; Colorize parenthesis pairwise
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; For files with very long lines
(use-package so-long
  :config (global-so-long-mode))
