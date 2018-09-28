
;;; package --- java-mode custom config
;;; Commentary:
;;;
(require 'meghanada)
;;; Code:
(add-hook 'java-mode-hook
	  (lambda ()
	    (local-set-key (kbd "s-b") 'meghanada-jump-declaration)
	    (local-set-key (kbd "M-s f") 'meghanada-use-flycheck)
	    (local-set-key (kbd "M-s i") 'meghanada-optimize-import)
	    (local-set-key (kbd "M-p") 'meghanada-back-jump)
	    (local-set-key (kbd "s-7") meghanada-reference)
	    (local-set-key (kbd "M-s l") 'meghanada-code-beautify)))

(provide 'java-custom)
;;; java-custom.el ends here
