;;; custom/evil-training/config.el -*- lexical-binding: t; -*-

(defvar evil-training-mode-map (make-sparse-keymap)
  "Keymap for `evil-training-mode.")

(defvar evil-training/map-equivalence
  '(("<up>", "k")
    ("<down>", "j")
    ("<left>", "h")
    ("<right>", "l")
    ("<home>", "^")
    ("<end>", "$")
    ("<backspace>", "X")
    ("<delete>", "x")
    ("C-<up>", "{")
    ("C-<down>", "}")
    ("C-<left>", "b")
    ("C-<right>", "w")
    ("C-<home>", "gg")
    ("C-<end>", "G"))
  "Alist of keys to disable and their Vim equivalents")

(dolist (key-pair evil-training/map-equivalence)
  (let ((key (kbd (car key-pair)))
        (replacement (cdr key-pair)))
    (evil-define-key 'motion evil-training-mode-map key
      (lambda ()
        (interactive)
        (message "Use '%s' instead of '%s'" replacement (car key-pair))))))

(define-minor-mode evil-training-mode
  "Minor mode to disable certain keys and encourage using Vim equivalents."
  :global t
  :lighter " EvilTrain"
  :keymap evil-training-mode-map)
