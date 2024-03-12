;; Generic keybinds that do not belong to any package.

;; bind revert buffer
(global-set-key (kbd "C-c r") #'revert-buffer)

;; open eshell with C-c e and set its current directory to the current buffer's default-directory
(global-set-key (kbd "C-c e") #'my:open-eshell-with-set-default-directory)

;; more accessible window resize keybinds
(global-set-key (kbd "S-C-<left>") #'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") #'shrink-window)
(global-set-key (kbd "S-C-<up>") #'enlarge-window)

;; imenu
(global-set-key (kbd "C-c i") #'imenu-list-smart-toggle)

;; run python
(global-set-key (kbd "C-c p") #'run-python)

;; eglot
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-n") #'eglot-rename))

;; python
(define-key python-mode-map (kbd "C-c y") #'python-pytest-dispatch)

;; open magit files in other window
(define-key magit-hunk-section-map (kbd "RET") #'magit-diff-visit-file-other-window)
(define-key magit-file-section-map (kbd "RET") #'magit-diff-visit-file-other-window)

;; make <home> call eshell-bol instead of the default (like C-a does)
;; and make <up> and <down> move around the buffer
;; instead of going up/down the history

;; Eshell does some very strange things with its keymap definition, this doesn't work
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "<up>") #'previous-line)
;;             (local-set-key (kbd "<down>") #'next-line)
;;             (local-set-key (kbd "<home>") #'eshell-bol)))

;; manual eldoc in prog modes
(define-key prog-mode-map (kbd "C-c C-h") #'eldoc)

;; browse url at point
(global-set-key (kbd "C-c u") #'browse-url-at-point)

;; create a new line on the current/previous line from point
(global-set-key (kbd "C-o") (lambda ()
                              (interactive)
                              (move-end-of-line nil)
                              (newline)))
(global-set-key (kbd "C-S-o") (lambda ()
                                (interactive)
                                (previous-line)
                                (move-end-of-line nil)
                                (newline)))
