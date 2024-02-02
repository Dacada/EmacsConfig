(defun my:open-eshell-with-set-default-directory ()
  "Open eshell but set its buffer local default directory
variable to be the same as the current buffer's. If the current
buffer has it set."
  (interactive)
  (let ((dir nil))
    (if (local-variable-p 'default-directory)
	(progn
	  (setq dir default-directory)
	  (eshell)
	  (setq default-directory dir)
	  (eshell-reset))
      (eshell))))
