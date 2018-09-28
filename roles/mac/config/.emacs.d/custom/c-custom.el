;;; package --- c-mod/cpp-mode custom config
;;; Commentary:
;;;
;;; Code:
(require 'ggtags)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1)
	      
	      (define-key ggtags-mode-map (kbd "C-g c") 'ggtags-create-tags)
	      (define-key ggtags-mode-map (kbd "C-g u") 'ggtags-update-tags)
	      (define-key ggtags-mode-map (kbd "C-g s") 'ggtags-find-other-symbol)
	      (define-key ggtags-mode-map (kbd "C-g h") 'ggtags-view-tag-history)
	      (define-key ggtags-mode-map (kbd "C-g r") 'ggtags-find-reference)
	      (define-key ggtags-mode-map (kbd "C-g f") 'ggtags-find-file)
	      (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))))


(provide 'c-custom)
;;; java-custom.el ends here
