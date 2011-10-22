(setq org-directory "~/org")
(setq load-path (cons (concat org-directory "/org-mode/lisp") load-path))
;; all files ending with .org opens in org-mode as the major mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files 
      '("~/org/coder.org" "~/org/office.org" "~/org/minor.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")))
(setq org-refile-targets
      '((nil :level . 1) (org-agenda-files :level . 1)))
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "pink" :bold t))))
; TODO-do not use custom-set-variables whcih use gui
; org-mode buffer listing(C-cb) uses ido-mode
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(menu-bar-mode t)
 '(org-completion-use-ido t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
; TODO-i think no need of lambda, use ((find-file...)) form and test it.
;; any ways i guess (interactive) is not required.
;; <f4> opens the office.org file
(global-set-key (kbd "<f4>")
		(lambda () (interactive)
		  (find-file (concat (getenv "HOME") "/org/office.org"))))

;; <f5> opens the home.org file
(global-set-key (kbd "<f5>")
		(lambda () (interactive)
		  (find-file (concat (getenv "HOME") "/org/coder.org"))))

;; <f6> opens the notes.org file
(global-set-key (kbd "<f6>")
		(lambda () (interactive)
		  (find-file (concat (getenv "HOME") "/org/notes.org"))))

;; <f7> opens the notes.org file
(global-set-key (kbd "<f7>")
		(lambda () (interactive)
		  (split-window-horizontally)
		  (find-file (concat (getenv "HOME") "/org/doc.org"))))
;; custom agenda views
(setq org-agenda-custom-commands
     '(("c" "NEXT followed by TODO" ((alltodo
				      ""
				      ((org-agenda-sorting-strategy '(todo-state-down))
				            (org-agenda-files (list (concat (getenv "HOME") "/org/coder.org")))))))
       ("o" "OFFICE TODO" ((alltodo
			    ""
			    ((org-agenda-sorting-strategy '(todo-state-down)) 
			     (org-agenda-files (list (concat (getenv "HOME") "/org/office.org")))))))
     ))
;; <f11> open programming agenda
(global-set-key (kbd "<f11>")
		(lambda () (interactive)
		  (org-agenda "" "c" )))
;; <f12> open office agenda
(global-set-key (kbd "<f12>")
		(lambda () (interactive)
		  (org-agenda "" "o" )))
;; pomodoro 
(defun my-after-load-org ()
  (add-to-list 'org-modules 'org-timer))

(eval-after-load "org" '(my-after-load-org))

(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook '(lambda () 
      (if (not org-timer-current-timer)
      (org-timer-set-timer '(16)))))



; emacs related
;; session management
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/") load-path))
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)


;; ?? document 
(transient-mark-mode 1)


(require 'color-theme)
(if (version<= emacs-version "23.1.1")
    (setq color-theme-is-global t)
  (color-theme-initialize))

(color-theme-dark-laptop)

(column-number-mode 1) ; show (line, col) in status bar(right term ?)
(ido-mode t); switch buffers
(show-paren-mode 1) ; show the matching parenthesis
(global-visual-line-mode 1) ; ?
;; (mouse-avoidance-mode 'banish) ;move the cusor away to the end when typing
;; by default C-x C-f shows in home directory
(setq command-line-default-directory (getenv "HOME"))
