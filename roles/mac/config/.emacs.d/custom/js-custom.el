;;; package --- js-mode custom config
;;; Commentary:
;;;
;;; Code:
(provide 'js-custom)

(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)

(js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;;; js-custom.el ends here
