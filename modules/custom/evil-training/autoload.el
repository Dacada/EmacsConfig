;;; custom/evil-training/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil-training/toggle ()
  "Toggle evil-training mode."
  (interactive)
  (if (bound-and-true-p evil-training-mode)
      (evil-training-mode -1)
    (evil-training-mode 1)))
