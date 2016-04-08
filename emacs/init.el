;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    material-theme
    afternoon-theme
    zenburn-theme
    git-commit
    magit
    plsql
    sql-indent
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load theme
;(global-linum-mode t) ;; enable line numbers globally

;; PYTHON CONFIGURATION
;; --------------------------------------
(display-time)
(setq display-time-and-date t)

;(server-start)
(setq-default indent-tabs-mode nil)
;; make next-line move by line, not displayed line
(setq line-move-visual nil)
(setq c-basic-offset 2)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

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

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
