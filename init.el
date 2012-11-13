; emacs related
(server-start)
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/") load-path))
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
(add-hook 'kill-emacs-hook '(lambda () (save-current-configuration 1)))
(setq x-select-enable-clipboard t) ; enable system copy work with mouse region select
; save when closing
(fset'yes-or-no-p 'y-or-n-p) ; "y or n" instead of "yes or no"
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))) ; backup files directory
(setq inhibit-startup-screen t) ; hide the splashscreen in order for the resume to work
(add-hook 'emacs-startup-hook '(lambda() (resume 1) (keyboard-quit))) ; resume while opening, (keyboard-quit) fn makes the visible mark to be stopped.
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;delete trailing white-spaces before saving
(transient-mark-mode t) ; highlight the region when the mark is active
(require 'color-theme) ; color theme
(if (version<= emacs-version "23.1.1")
    (setq color-theme-is-global t)
  (color-theme-initialize))
(color-theme-dark-laptop)
(column-number-mode 1) ; show (line, col) in status bar(right term ?)
(ido-mode t) ; file name and buffer completion
(show-paren-mode t) ; show the matching parenthesis
(mouse-avoidance-mode 'exile) ; move the mouse cusor to the corner as you type and get back when done.
(setq command-line-default-directory (getenv "HOME")) ; by default C-x C-f shows in home directory
(menu-bar-mode nil) ; hide the menu bar
(set-scroll-bar-mode nil) ; hide the scroll bar
(tool-bar-mode nil) ; hide the tool bar
(setq revert-without-query (list ".*")) ; TODO-give the proper list of regexp
(setq scroll-step 1) ; M-v and C-v navigates a full screen
; packages from .emacs_packages
(setq load-path (cons (concat (getenv "HOME") "/.emacs_packages/buffer-move") load-path))
(require 'buffer-move)
; experimental bindings
(global-set-key [(control ?\;)] 'backward-kill-word)
;(load (concat (getenv "HOME") "/.emacs_packages/xscheme/xscheme.el"))
(load (concat (getenv "HOME") "/.emacs.d/org_settings.el"))
