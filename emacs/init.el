(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" default)))
 '(package-selected-packages
   (quote
    (sqlplus ace-window golden-ratio ztree persp-projectile paredit markdown-mode js2-mode helm-projectile geiser flx-ido expand-region editorconfig aggressive-indent zenburn-theme sql-indent smart-mode-line py-autopep8 powerline plsql pdf-tools paper-theme org-bullets material-theme malabar-mode magit flycheck fill-column-indicator elpy ein better-defaults afternoon-theme)))
 '(scroll-bar-mode (quote right)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-bar ((t (:background "black" :foreground "dark magenta")))))

;; init.el --- Emacs configuration
;;(load-file "~/Projects/cedet/cedet-devel-load.el")
;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    highlight-indentation
    flycheck
    paper-theme
    material-theme
    afternoon-theme
    zenburn-theme
    git-commit
    magit
    plsql
    pkg-info
    
    ;pdf-tools
    ;pdf-tools-server
    tablist
    ace-window
    sql-indent
    pdf-tools
    smart-mode-line
    fill-column-indicator
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(smart-mode-line-enable)
;(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load theme
;(global-linum-mode t) ;; enable line numbers globally
(menu-bar-mode)
;; mouse yank should go to where the mouse is, not to the point
(setq mouse-yank-at-point nil)
;(setq mouse-autoselect-window t)

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

;; PYTHON CONFIGURATION
;; --------------------------------------
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
;; for ace-window
(global-set-key (kbd "C-x o") 'ace-window)
;; for pdf-tools
(pdf-tools-install)
;; fill column indicator - turn on with M-x fci-mode
(require 'fill-column-indicator)

;;(defvar jensen/vendor-dir (expand-file-name "vendor" user-emacs-directory))

;;(add-to-list 'load-path jensen/vendor-dir)

;;(dolist (project (directory-files jensen/vendor-dir t "\\w+"))
;; (when (file-directory-p project)
;;    (add-to-list 'load-path project)))

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


(elpy-enable)
(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; virtualenvwrapper.el https://github.com/porterjamesj/virtualenvwrapper.el
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/PythonEnvs")
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;(add-hook 'shell-mode-hook (setq shell-dirstack-query "command dirs"))
;; init.el ends here
