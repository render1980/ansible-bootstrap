;;; package --- lisp-mode custom config;;; Commentary:
;;;
;;; Code:

(add-hook 'emacs-lisp-mode-hook
              (lambda ()
                ;; Use spaces, not tabs.
                (setq indent-tabs-mode nil)
                ;; Keep M-TAB for `completion-at-point'
                (define-key flyspell-mode-map "\M-\t" nil)
                ;; Pretty-print eval'd expressions.
                (define-key emacs-lisp-mode-map
                            "\C-x\C-e" 'pp-eval-last-sexp)
                (define-key emacs-lisp-mode-map
                            "\r" 'reindent-then-newline-and-indent)))
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

(provide 'lisp-custom)
;;; lisp-custom.el ends here
