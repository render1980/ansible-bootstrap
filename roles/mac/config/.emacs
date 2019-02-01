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
(global-flycheck-mode 1)
(exec-path-from-shell-initialize)
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; --- KEY BINDINGS --- ;;;
(global-set-key (kbd "C-d") 'neotree)
(global-set-key (kbd "C-x C-d") 'neotree-hide)
(global-set-key (kbd "M-/") 'complete-tag)
(global-set-key (kbd "C-x C-p") 'python-mode)
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
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)

;; --- SETTINGS --- ;;
;;; LOAD ;;;
(setq load-path
      (cons (concat (getenv "HOME") "/.emacs.d/elpa")
	    load-path))
(add-to-list 'load-path "~/.emacs.d/custom/")


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
 '(flymake-cppcheck-command "/usr/local/bin/cppcheck")
 '(flymake-cppcheck-enable "all")
 '(package-selected-packages
   (quote
    (iedit function-args ggtags company company-go pygen elpygen elpy ranger company-c-headers company-cmake auto-complete-clang flycheck-clang-analyzer cpputils-cmake cppcheck flymake-cppcheck exec-path-from-shell go-tag go-rename go-imports go-fill-struct go-errcheck go-eldoc go-direx go-dlv go-add-tags go-complete go-mode pymacs flycheck-pycheckers py-import-check python-pylint pylint zlc yaml-mode tabbar spacemacs-theme projectile php-mode php+-mode nlinum neotree jedi-direx jdee inf-clojure hlinum go-guru go-autocomplete git-commit git-command git-blame fsharp-mode flymake-php flymake-go flymake flycheck-clojure flx-ido evil-tabs ensime ein egg dumb-jump dracula-theme cyberpunk-theme autopair atom-dark-theme flycheck-go-build-tags flycheck-go-build-executable all))))

;;; --- COMMANDS --- ;;;

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
(setq-default neo-show-hidden-files t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (local-set-key (kbd "s-d") 'neotree-delete-node)
	    (local-set-key (kbd "s-n") 'neotree-create-node)
	    (local-set-key (kbd "s-r") 'neotree-rename-node)
	    (local-set-key (kbd "s-c") 'neotree-copy-node)
	    (local-set-key (kbd "s-R") 'neotree-refresh)))

;; RANGER ;;
(setq ranger-cleanup-on-disable t)
(setq ranger-show-hidden t)
(setq ranger-modify-header t)
(setq ranger-header-func 'ranger-header-line)
(setq ranger-parent-header-func 'ranger-parent-header-line)
(setq ranger-preview-header-func 'ranger-preview-header-line)
(setq ranger-hide-cursor nil)
(setq ranger-footer-delay 0.2)
(setq ranger-preview-delay 0.040)
(setq ranger-parent-depth 0)
(setq ranger-preview-file t)

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

;; iEdit ;;
(require 'iedit)

;;; --- LANG --- ;;;
(require 'python-custom)
(require 'php-custom)
(require 'c-custom)
(require 'sh-custom)
(require 'go-custom)
(require 'js-custom)
(require 'yaml-custom)

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
