; emacs related
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/") load-path))
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
(add-hook 'kill-emacs-hook ; save when closing
	  '(lambda ()
	     (save-current-configuration 1)))
(setq inhibit-startup-screen t) ; hide the splashscreen in order for the resume to work
(add-hook 'emacs-startup-hook
	  '(lambda()
	     (resume 1))) ; resume while opening
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

; org-mode settings
; TODO-line by line test and document
(setq org-directory "~/org")
(setq load-path (cons (concat org-directory "/org-mode/lisp") load-path))
(require 'org-install) ; have effect on all variables -see faq - http://orgmode.org/worg/org-faq.html
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
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CANCELED(c@!)")))

(setq org-refile-targets
      '((nil :level . 1) (org-agenda-files :level . 1)))
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "cyan" :weight bold))))

(setq org-completion-use-ido t)
(setq org-use-speed-commands t)
;; TODO-i think no need of lambda, use ((find-file...)) form and test it.
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
       ("w" "DAY AGENDA" ((agenda "" ((org-agenda-ndays 1)))
			  (todo "" ((org-agenda-sorting-strategy '(todo-state-down)) ;; TODO-use (tags-todo) and filter "TODO and NEXT" states
				    (org-agenda-todo-ignore-scheduled 'all)
				    (org-agenda-todo-ignore-deadlines 'future)
				    ))))
       ))
;;; <f11> open programming agenda
(global-set-key (kbd "<f11>")
		(lambda () (interactive)
		  (org-agenda "" "c" )))
;;; <f12> open office agenda
(global-set-key (kbd "<f12>")
		(lambda () (interactive)
		  (org-agenda "" "w" )))
;; pomodoro
(defun my-after-load-org ()
  (add-to-list 'org-modules 'org-timer))

(eval-after-load "org" '(my-after-load-org))

(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook '(lambda ()
      (if (not org-timer-current-timer)
      (org-timer-set-timer '(16)))))

((lambda ()
   (defun print-line ()
     (print (buffer-substring-no-properties (point-at-bol) (point-at-eol)) (get-buffer "*scratch*")))
   (defun my-org-archive-done-tasks ()
     (interactive)
     (save-current-buffer (set-buffer (get-buffer "*scratch*")) (erase-buffer))
					;(org-map-entries 'org-archive-subtree)
     (org-map-entries 'print-line "" (list "/home/tieto/org/office.org"))
     (print "hello"))
   (my-org-archive-done-tasks)))


(buffer-substring-no-properties 1 9)

(setq hh 5)
((lambda ()
   (let ((hh 6))
     (print hh))))
(print hh)