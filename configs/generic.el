;; Generic configurations that don't fit in any other category

;; shift-arrow to move between windows
(windmove-default-keybindings)

;; ido
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)

;; i hate typing two extra characters
(defalias 'yes-or-no-p #'y-or-n-p)

;; versioned backups (https://www.emacswiki.org/emacs/BackupDirectory)
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.saves/")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; replace selected text when typing
(delete-selection-mode 1)

;; enable mouse click detection while in console mode
(xterm-mouse-mode 1)

;; disable menu bar -- THIS MESSES UP CONSOLE MODE
;; (menu-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; create open alias for opening files through eshell
(defalias 'open #'find-file)
