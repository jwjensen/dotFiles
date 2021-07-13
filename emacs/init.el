;; Initial Configuration
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;----------------------------------------------------------
;; Setup Package management
(require 'package)
;; handle old versions that don't support ssl
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
  (add-to-list 'package-archives
               (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives
               (cons "org" (concat proto "://orgmode.org/elpa/")) t)

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
                 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;----------------------------------------------------------
;; Load and configure general packages
(use-package counsel)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(load "server")
(unless (server-running-p) (server-start))

(setq c-basic-offset 2)
;;----------------------------------------------------------
;; interactive behavior
(setq mouse-yank-at-point nil)
(setq-default intdent-tabs-mode nil)
(setq line-move-visual nil)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
;; fix hang waiting for x clipboard manager
(setq x-selection-timeout 10)
;;----------------------------------------------------------
;; UI Changes / packages
(setq scroll-bar-mode 'right)
(tool-bar-mode -1)
(menu-bar-mode)
(setq visible-bell nil)
;; needs font fonts-firacode installed (debian)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 80)
;; consider export GDK_DPI_SCALE=3 #for toolbar
(load-theme 'wombat)
(display-time)
(setq display-time-and-date t)
;;; run M-x all-the-icons-install-fonts
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 1)))

;; turn on line numbers, but off for some modes
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
		comint-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
