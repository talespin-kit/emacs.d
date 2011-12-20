; emacs related
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/") load-path))
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
(add-hook 'kill-emacs-hook '(lambda () (save-current-configuration 1))) ; save when closing
(setq inhibit-startup-screen t) ; hide the splashscreen in order for the resume to work
(add-hook 'emacs-startup-hook '(lambda() (resume 1))) ; resume while opening
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
;(menu-bar-mode nil) ; hide the menu bar
(set-scroll-bar-mode nil) ; hide the scroll bar
(tool-bar-mode nil) ; hide the tool bar
; (setq revert-without-query t); TODO-takes regular expression not "t", verify
; org-mode settings
(setq org-directory "~/org")
(setq load-path (cons (concat org-directory "/org-mode/lisp") load-path))
(require 'org-install) ; ensures effect on all variables - explained in faq - http://orgmode.org/worg/org-faq.html
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ;; all files ending with .org opens in org-mode as the major mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files '("~/org/coder.org" "~/org/office.org" "~/org/minor.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CANCELED(c@!)")))
(setq org-refile-targets '((nil :level . 1) (org-agenda-files :level . 1)))
(setq org-todo-keyword-faces '(("NEXT" . (:foreground "cyan" :weight bold))))
(setq org-completion-use-ido t)
(setq org-use-speed-commands t)
(global-set-key (kbd "<f4>") (lambda () (interactive) (find-file (concat (getenv "HOME") "/org/office.org"))))
(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file (concat (getenv "HOME") "/org/coder.org"))))
(global-set-key (kbd "<f6>") (lambda () (interactive) (find-file (concat (getenv "HOME") "/org/notes.org"))))
(global-set-key (kbd "<f7>") (lambda () (interactive) (split-window-horizontally)
			       (find-file (concat (getenv "HOME") "/org/doc.org"))))
;; custom agenda views
(setq org-agenda-custom-commands
     '(("c" "NEXT followed by TODO"
	((alltodo
	  ""
	  ((org-agenda-sorting-strategy '(todo-state-down))
	   (org-agenda-files (list (concat (getenv "HOME") "/org/coder.org")))))))
       ("o" "OFFICE TODO"
	((alltodo
	  ""
	  ((org-agenda-sorting-strategy '(todo-state-down))
	   (org-agenda-files (list (concat (getenv "HOME") "/org/office.org")))))))
;;; TODO-use org-agenda-skip-function to display all the scheduled tasks which are not present in the org-agenda-ndays
;;;
       ("w" "DAY AGENDA"
	((agenda "" ((org-agenda-ndays 1)
		     (org-agenda-files (list (concat (getenv "HOME") "/org/office.org")))))
	 (todo "" ((org-agenda-sorting-strategy '(todo-state-down))
		   (org-agenda-todo-ignore-scheduled 'past); past days and todays schedule is shown in (agenda) block
		   (org-agenda-files (list (concat (getenv "HOME") "/org/office.org")))))))))

(global-set-key (kbd "<f11>") (lambda () (interactive) (org-agenda "" "c" )))
(global-set-key (kbd "<f12>") (lambda () (interactive) (org-agenda "" "w" )))
;; pomodoro
(defun my-after-load-org () (add-to-list 'org-modules 'org-timer))
(eval-after-load "org" '(my-after-load-org))
(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook '(lambda () (if (not org-timer-current-timer) (org-timer-set-timer '(16)))))
;; archive the DONE tasks
;;; TODO-take the file name as arg to archive, if not org-agenda-files, also  include CANCELLED state for archiving
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries '(lambda ()
		      (setq org-map-continue-from (point-at-bol))
		      (org-archive-subtree))
		   "/DONE" (org-agenda-files)))
