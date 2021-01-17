;;;;;
;;
;; use package-show-package-list to see packages
;;
;; use i to install, ? for information, U to mark upgradeable, x to execute
;; use M-x package-install-selected-packages to install packages in
;;  package-selected-packages
;;
;;;;;
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default)))
 '(display-time-mode t)
 '(elpy-rpc-python-command "python3")
 '(global-linum-mode t)
 '(package-selected-packages
   (quote
    (ssh ess-R-data-view ess-smart-equals ess-smart-underscore
         ess-view org csharp-mode csproj-mode omnisharp importmagic
         eshell-bookmark eshell-did-you-mean eshell-git-prompt
         eshell-prompt-extras eshell-toggle eshell-up powerline
         virtualenvwrapper ng2-mode neotree ess treemacs-icons-dired
         use-package treemacs tabbar graphviz-dot-mode
         fill-column-indicator smart-mode-line pdf-tools sql-indent
         pkg-info dired-du zenburn-theme plsql material-theme magit
         magit-annex better-defaults blacken sql-indent)))
 '(pyvenv-virtualenvwrapper-python "/usr/bin/python3")
 '(safe-local-variable-values (quote ((org-confirm-babel-evaluate))))
 '(scroll-bar-mode (quote right))
 '(send-mail-function nil)
 '(tool-bar-mode nil)
 '(visible-bell t)
 )

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and
  ;; MELPA Stable as desired
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable"
  ;;                         (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
                 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme
    zenburn-theme
    git-commit
    magit
    magit-tbdiff
    plsql
    pkg-info
    pdf-tools
    smart-mode-line
    fill-column-indicator
    paredit
    markdown-mode
    markdown-mode+
    js2-mode
    expand-region
    aggressive-indent
    ;; python
    elpy
    ein ;; emacs ipython notebook M-x ein:note
    flycheck
    apache-mode
    py-autopep8
    polymode
    web-mode
    csv-mode
    graphviz-dot-mode
    tabbar
    neotree
    eshell-bookmark
    eshell-did-you-mean
    eshell-git-prompt
    eshell-prompt-extras
    eshell-toggle
    eshell-up
    powerline
    virtualenvwrapper
    ng2-mode
))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(setq sml-theme 'dark)
(sml/setup)
;(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load theme
(global-linum-mode t) ;; enable line numbers globally
(tool-bar-mode -1)
(menu-bar-mode)
;; mouse yank should go to where the mouse is, not to the point
(setq mouse-yank-at-point nil)
;(setq mouse-autoselect-window t)
;; configure expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-=") 'er/contract-region)
;; fill column indicator
;;; call w/ M-x fci-mode
(require 'fill-column-indicator)

;; webmode:  http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun jensen/vsplit-last-buffer (prefix)
  "Split the window vertically, display most recent buffer"
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))
(defun jensen/hsplit-last-buffer (prefix)
  "Split the window horizontally, display most recent buffer"
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))
(global-set-key (kbd "C-x 3") 'jensen/hsplit-last-buffer)
(global-set-key (kbd "C-x 2") 'jensen/vsplit-last-buffer)

;; printing
(setq ps-paper-type 'letter
      ps-font-size 7.0
      ps-print-header t
      ps-landscape-mode t
      ps-number-of-columns 2)

(display-time)
(setq display-time-and-date t)

(load "server")
(unless (server-running-p) (server-start))
(setq-default indent-tabs-mode nil)
;; make next-line move by line, not displayed line
(setq line-move-visual nil)
(setq c-basic-offset 2)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
;; for pdf-tools
(pdf-tools-install)
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode 0)))

(eval-after-load "sql"
  (load-library "sql-indent"))
(require 'plsql)

(defun my-csharp-mode-hook ()
  ;; enable the stuff you want for C# here
  (electric-pair-local-mode 1) ;; Emacs 25
  )
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(setq auto-mode-alist
   (append '(
             ("\\.sql$" . sql-mode)
             ("\\.tps$" . sql-mode)
             ("\\.tpb$" . sql-mode)
             ("\\.pks$" . plsql-mode)
             ("\\.pkb$" . plsql-mode)
             ("\\.vew$" . plsql-mode)
             ("\\.prc$" . plsql-mode)
             ("\\.trg$" . plsql-mode)
             ("\\.fnc$" . plsql-mode)
             ("\\.java$" . java-mode)
             ("\\.cs$" . csharp-mode)) auto-mode-alist))

;; PYTHON CONFIGURATION
;; --------------------------------------
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            ))
(elpy-enable)
;; ipython
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(require 'find-file-in-project)

;;;
;; R (ESS) config
;(require 'ess-site)


;;;
;; mkvirtualenv Scratch -p /usr/bin/python3
;; pip install rope jedi importmagic autopep8 flake8
;; python -m pip install jupyter yapf black
;; and install elisp package importmagic, epc
;; pay attention to Time-stamp: tags:
(add-hook 'before-save-hook 'time-stamp)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#263238" :foreground "dim gray")))))

;; fix hang waiting for x clipboard manager
(setq x-selection-timeout 10)

;; eshell - git
;(eshell-git-prompt-use-theme 'jensen)

;; https://github.com/kpurdon/.emacs.d
;; https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup

(use-package treemacs-icons-dired
             :after treemacs dired
             :ensure t
             :config (treemacs-icons-dired-mode))


;;;; fix gnutls issue: (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; R execution in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (python . t)
   (R . t)))
