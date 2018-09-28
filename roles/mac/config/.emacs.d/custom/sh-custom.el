;;; package --- sh-mode custom config
;;; Commentary:
;;;
;;; Code:

;; (defun shell-mode-fun ()
;;   ((ansi-term)))
(add-hook 'shell-mode-hook (lambda() (ansi-term)))

(add-hook 'sh-mode-hook
	  (lambda ()
	    (local-set-key (kbd "s-b") 'dumb-jump-go)
	    (local-set-key (kbd "s-[") 'dumb-jump-back)
	    (local-set-key (kbd "s-]") 'dumb-jump-quick-look)
	    (auto-complete-mode 1)))


(provide 'sh-custom)
