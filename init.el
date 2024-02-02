;; a mix of these two sources, to succintly load files for a modularized setup

;; https://stackoverflow.com/questions/2079095/how-to-modularize-an-emacs-configuration
;; https://www.emacswiki.org/emacs/DotEmacsModular

;; define the location of the rest of the config
(defconst toc:emacs-config-dir
  (concat (file-name-as-directory
	   (cond ((boundp 'user-emacs-directory)
		  user-emacs-directory)
		 ((boundp 'user-init-directory)
		  user-init-directory)
		 (t "~/.emacs.d/")))
	  "configs/"))

;; succintly load a configuration (instead of every file in the directory, this way parts can be easily commented out, e.g. to debug)
(defun toc:load-user-file (file)
  (interactive "f")
  "Load a file in user's configuration directory"
  (load-file (expand-file-name file toc:emacs-config-dir)))

;; i also want you to put the customize interface's files into their own file
(setq custom-file (expand-file-name "custom.el" toc:emacs-config-dir))
(load-file custom-file)

(toc:load-user-file "packages.el")
(toc:load-user-file "performance.el")
(toc:load-user-file "functions.el")
(toc:load-user-file "generic.el")
(toc:load-user-file "keybinds.el")
(toc:load-user-file "hooks.el")
(toc:load-user-file "advice.el")

;; TODO
;; json-navigator and make it always run when opening json
