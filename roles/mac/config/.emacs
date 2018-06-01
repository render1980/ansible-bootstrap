;;; Code:
(package-initialize)
(auto-fill-mode nil)
(transient-mark-mode t)
(tool-bar-mode -1)
(global-font-lock-mode t)
(setq display-time-24hr-format t)
(setq visible-bell nil)
(display-time-mode t)
(show-paren-mode 1)
(desktop-save-mode 1)
(setq default-directory (concat (getenv "HOME") "/" "go/src"))
(dumb-jump-mode 1)
(global-linum-mode 1)

;;; KEY BINDINGS ;;;
(global-set-key (kbd "C-d") 'neotree)
(global-set-key (kbd "C-x C-d") 'neotree-hide)
(global-set-key (kbd "M-/") 'complete-tag)
(global-set-key (kbd "C-x C-p") 'python-mode)
(global-set-key (kbd "C-f") 'projectile-find-file)
(global-set-key (kbd "C-}") 'end-of-buffer)
(global-set-key (kbd "C-{") 'beginning-of-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-}") 'tabbar-forward)
(global-set-key (kbd "s-{") 'tabbar-backward)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<left>") 'beginning-of-line)
(global-set-key (kbd "s-w") 'kill-buffer)
(global-set-key (kbd "s-<backspace>") 'kill-line)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-D") 'split-window-below)
(global-set-key (kbd "s-d") 'split-window-right)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-\\") 'uncomment-region)
(global-set-key (kbd "s-e") 'eval-last-sexp)
(global-set-key (kbd "s-]") 'other-window)
(global-set-key (kbd "s-[") 'previous-multiframe-window)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "s-R") 'rgrep)
(global-set-key (kbd "C-r") 'replace-search-function)
(global-set-key (kbd "s-.") 'shell-command)

;; LOAD ;;
(setq load-path
      (cons (concat (getenv "HOME") "/.emacs.d/elpa")
	    load-path))
(add-to-list 'load-path "~/.emacs.d/elpa/")

;;; REMOVE DEFAULT BUFFERS ;;;
(setq initial-scratch-message "")
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(setq inhibit-startup-buffer-menu t)

;;; BUFFERS ;;;
(defun start-other-emacs ()
  "Start another Emacs process to open an independent Emacs window."
  (interactive)
  ;;; Run the command "emacs", piping output into a
  ;;; buffer called "*other-emacs*"
  (start-process "emacs" "*other-emacs*" "emacs"))
(global-set-key (kbd "C-x 5 2") 'start-other-emacs)

;;; REPO ;;;
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; VARS ;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell go-tag go-rename go-imports go-fill-struct go-errcheck go-eldoc go-direx go-dlv go-add-tags go-complete go-mode pymacs flycheck-pycheckers py-import-check python-pylint pylint zlc yaml-mode tabbar spacemacs-theme projectile php-mode php+-mode nlinum neotree jedi-direx jdee inf-clojure hlinum go-guru go-autocomplete git-commit git-command git-blame fsharp-mode flymake-php flymake-go flymake flycheck-clojure flx-ido evil-tabs ensime ein egg dumb-jump dracula-theme cyberpunk-theme autopair atom-dark-theme flycheck-go-build-tags flycheck-go-build-executable all))))

;;; COMMANDS ;;;

;; RGREP ;;
(eval-when-compile (require 'cl))
(defun kill-grep-window ()
  "Kill 'grep-mode' window."
  (destructuring-bind (window major-mode)
      (with-selected-window (next-window (selected-window))
        (list (selected-window) major-mode))
    (when (eq major-mode 'grep-mode)
      (delete-window window))))

(add-hook 'next-error-hook 'kill-grep-window)

;; THEME ;;
(load-theme 'cyberpunk t)
(set-face-attribute 'default nil :height 135)

;;; MODULES ;;;

;; IDO ;;
(require 'ido)
(ido-mode t)
(ido-mode 1)
(ido-everywhere 1)

;; COMPLETE ;;
(require 'auto-complete-config)
(ac-config-default)
(require 'autopair)
(autopair-global-mode)

;; NEOTREE ;;
(require 'neotree)
(setq neo-window-width 30)

;; PROJECTILE ;;
(require 'projectile)

;; TABBAR ;;
(require 'tabbar)

(global-set-key (kbd "C-x <left>") 'tabbar-backward)
(global-set-key (kbd "C-x <right>") 'tabbar-forward)
(setq tabbar-buffer-groups-function
      '(lambda ()
         (list
          (cond
           ((find (aref (buffer-name (current-buffer)) 0) " *") "*")
           (t "All Buffers")))))

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode 1)

;;; LANG ;;;

;; ENSIME ;;
;; (setq exec-path (append exec-path '("/opt/local/bin")))
;; (setenv "PATH" (shell-command-to-string "/bin/bash -c 'echo -n $PATH'"))
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-mode 'ensime-scala-mode-hook)

;; GOLANG ;;
(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOROOT" "/usr/local/Cellar/go/1.10.2/libexec")

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

(setq exec-path (cons "/usr/local/Cellar/go/1.10.2/libexec/bin" exec-path))
(add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))
(add-hook 'before-save-hook 'gofmt-before-save)

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
  (go-guru-hl-identifier-mode)
  (auto-complete-mode 1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-guru)


;;; PYTHON ;;;
(global-flycheck-mode 1)
(autoload 'pylint "pylint")

(defun jedi-config:setup-keys ()
  "Setup keys for Jedi."
  (local-set-key (kbd "s-b") 'jedi:goto-definition)
  (local-set-key (kbd "C-/") 'jedi:show-doc))
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

;; PHP ;;

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; FLYCHECK ;;
(require 'flymake)

(defun flymake-php-init ()
  "Use php to check the syntax of the current file."
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	 (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(add-to-list 'flymake-err-line-patterns
  '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))

(add-hook 'php-mode-hook (lambda () (flymake-mode 1)))
;; (define-key php-mode-map '[M-S-up] 'flymake-goto-prev-error)
;; (define-key php-mode-map '[M-S-down] 'flymake-goto-next-error)

;; Clojure ;;
(autoload 'inf-clojure "inf-clojure" "Run an inferior Clojure process" t)
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; ;;; SHELL ;;; ;;

(add-hook 'sh-mode-hook
	  (lambda ()
	    (local-set-key (kbd "s-b") 'dumb-jump-go)
	    (local-set-key (kbd "s-[") 'dumb-jump-back)
	    (local-set-key (kbd "s-]") 'dumb-jump-quick-look)
	    (auto-complete-mode 1)))

(provide '.emacs)
;;; .emacs ends here
