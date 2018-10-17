(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(global-linum-mode t)
 '(package-selected-packages
   (quote
    (xkcd fill-column-indicator smart-mode-line pdf-tools sql-indent pkg-info dired-du zenburn-theme plsql material-theme magit better-defaults)))
 '(pyvenv-virtualenvwrapper-python "/usr/bin/python3")
 '(scroll-bar-mode (quote right)))

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(package-refresh-contents)

(defvar myPackages
  '(better-defaults
    material-theme
    zenburn-theme
    git-commit
    magit
    magit-tbdiff
    plsql
    pkg-info
    sql-indent
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
(global-set-key [C-x 2] 'jensen/hsplit-last-buffer)
(global-set-key [C-x 3] 'jensen/vsplit-last-buffer)

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
;; for pdf-tools
(pdf-tools-install)

(eval-after-load "sql"
  (load-library "sql-indent"))
(require 'plsql)



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
;(setq python-shell-interpreter "ipython"
;      python-shell-interpreter-args "-i --simple-prompt")
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;;
;; mkvirtualenv Scratch -p /usr/bin/python3
;; pip install rope jedi importmagic autopep8 flak8e
;; python -m pip install jupyter yapf black

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#263238" :foreground "dim gray")))))


;; https://github.com/kpurdon/.emacs.d
;; https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup
