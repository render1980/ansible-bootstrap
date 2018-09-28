;;; package --- python-mode custom config
;;; Commentary:
;;;
;;; Code:
(global-flycheck-mode 1)
(autoload 'pylint "pylint")

(defun jedi-config:setup-keys ()
  "Setup keys for Jedi."
  (local-set-key (kbd "s-b") 'jedi:goto-definition)
  (local-set-key (kbd "s-2") 'jedi:show-doc))
(add-hook 'python-mode-hook 'jedi-config:setup-keys)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook' python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;; (add-hook 'python-mode-hook 'jedi:ac-setup)
(setq-default py-shell-name "ipython")
(put 'scroll-left 'disabled nil)
(autoload 'jedi:setup "jedi" nil t)

;; (add-hook 'python-mode-hook
;; 	  '(pylint-add-menu-items
;; 	    '(pylint-add-key-bindings))
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "s-i") 'py-import-check)))
;; autoimport

(require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

(provide 'python-custom)
;;; python-custom.el ends here
