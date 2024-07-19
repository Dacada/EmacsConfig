;;; -*- lexical-binding: t; -*-

;; See https://github.com/a13/emacs.d for inspiration

;; External dependencies:
;;   - Cascadia Code font must be installed: https://github.com/microsoft/cascadia-code/releases
;;   - ag (the silver searcher) must be installed
;;   - mise must be installed and the typescript language server through it

;; The fonts for all-the-icons must be installed once by running, within Emacs:
;;     M-x all-the-icons-install-fonts

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
  (default ((t (:font "Cascadia Code"))))  ;; Use Cascadia Code font by default
  (fill-column 120 "line wrap after 120 columns"))  

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
  :bind
  ("C-c r" . revert-buffer)
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
(use-package eshell
  :bind
  ("C-c e" . eshell))
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

;; icons!
(use-package all-the-icons :ensure t :defer t)

;; icons in dired!
(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; icons in ivy!
(use-package all-the-icons-ivy
  :defer t
  :ensure t
  :after ivy
  :custom
  (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  :config
  (all-the-icons-ivy-setup))

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

;; counsel-M-x can use this one -- it's an alternative to the usual M-x interface
(use-package amx :ensure t :defer t)

;; ivy-mode ensures that any Emacs command using completing-read-function uses ivy for completion.
(use-package ivy
  :ensure t
  :custom
  (ivy-count-format "%d/%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :config
  (ivy-mode t))

(use-package ivy-xref
  :ensure t
  :defer t
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs "Use Ivy to show xrefs"))

;; Keeping this mapping from the source, not sure what this prefix map is
(use-package counsel
  :ensure t
  :bind
  (([remap menu-bar-open] . counsel-tmm)
   ([remap insert-char] . counsel-unicode-char)
   ([remap isearch-forward] . counsel-grep-or-swiper)
   :map mode-specific-map
   :prefix-map counsel-prefix-map
   :prefix "c"
   ("a" . counsel-apropos)
   ("b" . counsel-bookmark)
   ("B" . counsel-bookmarked-directory)
   ("c w" . counsel-colors-web)
   ("c e" . counsel-colors-emacs)
   ("d" . counsel-dired-jump)
   ("f" . counsel-file-jump)
   ("F" . counsel-faces)
   ("g" . counsel-org-goto)
   ("h" . counsel-command-history)
   ("H" . counsel-minibuffer-history)
   ("i" . counsel-imenu)
   ("j" . counsel-find-symbol)
   ("l" . counsel-locate)
   ("L" . counsel-find-library)
   ("m" . counsel-mark-ring)
   ("o" . counsel-outline)
   ("O" . counsel-find-file-extern)
   ("p" . counsel-package)
   ("r" . counsel-recentf)
   ("s g" . counsel-grep)
   ("s r" . counsel-rg)
   ("s s" . counsel-ag)
   ("t" . counsel-org-tag)
   ("v" . counsel-set-variable)
   ("w" . counsel-wmctrl)
   :map help-map
   ("F" . counsel-describe-face))
  :custom
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never %s %s")
  :init
  (counsel-mode))

;; Replacement for C-s using ivy/counsel
(use-package swiper :ensure t)

;; ivy completions have descriptions and stuff
(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-project-root-cache-mode t)
  (ivy-rich-mode 1))

;; indicate minibuffer depth
(use-package mb-depth
  :config
  (minibuffer-depth-indicate-mode 1))

;; allows expanding the region with a keyboard shortcut
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)))

;; create a the second of the pair automatically when creating the first, eg '(' becomes '()'
(use-package elec-pair
  :config
  (electric-pair-mode))

;; deleting a whitespace character deletes all of them
(use-package hungry-delete
  :ensure t
  :hook
  (text-mode . hungry-delete-mode)
  (prog-mode . hungry-delete-mode))

;; blatant copy-paste
(use-package ibuffer-vc
  :defer t
  :ensure t
  :config
  (define-ibuffer-column icon
    (:name "Icon" :inline t)
    (all-the-icons-ivy--icon-for-mode major-mode))
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process)) "include vc status info")
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

;; for git config files
(use-package git-modes
  :ensure t
  :defer t)

;; magit
(use-package magit
  :bind
  (("C-c g" . magit-status)
   (:map magit-hunk-section-map ("RET" . magit-diff-visit-file-other-window))
   (:map magit-file-section-map ("RET" . magit-diff-visit-file-other-window))))

;; walk through versions of a discrete file
(use-package git-timemachine
  :ensure t
  :defer t)

;; for git merging
(use-package smerge-mode
  :defer t)

;; highlight changes from vc in buffer and dired
(use-package diff-hl
  :ensure t
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)))

;; grep replacement
(use-package ag
  :ensure t
  :defer t
  :custom
  (ag-highlight-search t "Highlight the current search term."))

;; completion
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

;; quickhelp if you linger in the completion
(use-package company-quickhelp
  :ensure t
  :defer t
  :custom
  (company-quickhelp-delay 3)
  (company-quickhelp-mode 1))

;; completion for shell
(use-package company-shell
  :ensure t
  :after company
  :defer t
  :custom-update
  (company-backends '(company-shell)))

;; show issues in the code (underlining it)
(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

;; coverage
(use-package cov
  :ensure t
  :defer t)

;; kill unused buffers automatically
(use-package midnight
  :custom
  (clean-buffer-list-delay-general 5 "Five days before buffers are autokilled")
  (midnight-delay 3637 "Seconds after midnight when midnight will do its thing")
  (midnight-mode t "Enable midnight mode"))

;; move around windows with S-left/right/down/up
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package delsel
  :custom
  (delete-selection-mode 1))

;; lsp servers
(use-package eglot
  :straight nil
  :ensure t
  :hook
  (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map ("C-c C-n"))
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(typescript-ts-base-mode . ,(eglot-alternatives
                                               '(("mise" "exec" "--" "typescript-language-server" "--stdio")))))))

;; highlight indentation
(use-package highlight-indentation
  :hook
  (prog-mode . highlight-indentation-mode))

;; display line numbers
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

;; clors in compilation buffer
(use-package fancy-compilation
  :init
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

(use-package newcomment
  :straight nil
  :hook
  (python-base-mode . (lambda () (set (make-local-variable 'comment-inline-offset) 2))))

(use-package python
  :bind
  ("C-c p" . run-python))

(use-package browse-url
  :bind
  ("C-c u" . browse-url-at-point))

;; Some misc useful stuff
(use-package crux
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package treemacs
  :custom
  (treemacs-show-hidden-files nil)
  :bind
  ("C-c t" . treemacs))

(use-package python-pytest
  :bind
  ("C-c y" . python-pytest-dispatch))

;; TODO: WHEN INSTALLING THE PYTHONG DOCSTRING MODE PACKAGE, HOOK PYTHON BASE MODE TO python-docstring-mode


;;; init.el ends here
