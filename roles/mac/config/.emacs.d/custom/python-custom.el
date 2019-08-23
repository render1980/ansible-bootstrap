;;; package --- python-mode custom config;;; Commentary:
;;;
;;; Code:

(global-flycheck-mode 1)
(autoload 'pylint "pylint")

(defun jedi-config:setup-keys ()
  "Setup keys for Jedi."
  (local-set-key (kbd "s-b") 'jedi:goto-definition)
  (local-set-key (kbd "s-2") 'jedi:show-doc)
  (local-set-key (kbd "s-8") 'elpy-rgrep-symbol)
  (local-set-key (kbd "s-i") 'py-import-check))
(add-hook 'python-mode-hook 'jedi-config:setup-keys)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook' python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq-default py-shell-name "ipython")
(put 'scroll-left 'disabled nil)
(autoload 'jedi:setup "jedi" nil t)

(require 'pymacs)

(provide 'python-custom)
;;; python-custom.el ends here
