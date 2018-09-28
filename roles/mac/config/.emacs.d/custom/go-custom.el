;;; package --- go-mode custom config
;;; Commentary:
;;;
;;; Code:

;; GOLANG ;;

(require 'go-guru)
(require 'go-eldoc)

(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOROOT" "/usr/local/Cellar/go/1.10.3/libexec")
(setq exec-path (cons (concat (getenv "GOROOT") "/bin") exec-path))
(add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/go/src"))

(defun set-exec-path-from-shell-PATH ()
  "Set exec path from shell."
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(add-hook 'before-save-hook 'gofmt-before-save)
(setq-default gofmt-command "goimports")
(add-hook 'go-mode-hook 'go-eldoc-setup)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(defun my-go-mode-hook ()
  "Call Gofmt before saving."
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "s-b") 'godef-jump)
  (local-set-key (kbd "s-7") 'go-guru-callers)
  (local-set-key (kbd "s-2") 'godoc-at-point)
  (local-set-key (kbd "M-i") 'go-goto-imports)
  (local-set-key (kbd "M-f") 'go-goto-function)
  (local-set-key (kbd "M-l") 'gofmt)
  (local-set-key (kbd "M-s") 'go-rename)
  (go-guru-hl-identifier-mode)
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'my-go-mode-hook)
;; (add-hook 'go-mode-hook (lambda ()
                            ;; (set (make-local-variable 'company-backends) '(company-go))
                            ;; (company-mode)))

(provide 'go-custom)
