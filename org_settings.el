; org-mode settings
(setq load-path (cons (concat (getenv "HOME") "/.emacs_packages/org-mode/lisp") load-path))
(setq load-path (cons (concat (getenv "HOME") "/.emacs_packages/org-mode/contrib/lisp") load-path))
(require 'org-depend)
(setq org-directory "~/org")
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ;open *.org files in org-mode
(setq org-hide-leading-stars t) ; clean view
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files '("~/org/coder.org" "~/org/office.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@!)")))
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-todo-keyword-faces '(("NEXT" . (:foreground "cyan" :weight bold))
			       ("WAIT" . (:foreground "purple" :weight bold))))
(setq org-completion-use-ido t)
(setq org-use-speed-commands t)
(setq org-use-property-inheritance t)
(global-set-key (kbd "<f4>") (lambda () (interactive) (find-file (concat (getenv "HOME") "/org/office.org"))))
(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file (concat (getenv "HOME") "/org/coder.org"))))
(global-set-key (kbd "<f6>") (lambda () (interactive) (find-file (concat (getenv "HOME") "/org/notes.org"))))
(global-set-key (kbd "<f7>") (lambda () (interactive) (find-file (concat (getenv "HOME") "/org/doc.org"))))
;; custom agenda views
(setq org-agenda-custom-commands
           `(("c" "Coder tasks"
	      ((agenda "" ((org-agenda-ndays 1)))
	       (tags-todo "ID={.+}-internet") ; blockers and not-internet
	       (tags-todo "ID={.+}+internet") ; blockers and internet
	       (tags-todo "-ID={.+}-blocked-internet") ; non-blockers and not-blocked and not-internet
	       (tags-todo "-ID={.+}-blocked+internet") ; non-blockers and not-blocked and internet
	       (tags-todo "+blocked-internet") ; blocked and not-internet
	       (tags-todo "+blocked+internet")) ; blocked and internet
	      ((org-agenda-sorting-strategy '(todo-state-down))
	       (org-agenda-files  `(,(concat (getenv "HOME") "/org/coder.org")))))
	     ("k" "Office Tasks"
	      ((agenda "" ((org-agenda-ndays 1)))
	       (tags-todo "ID={.+}")
	       (tags-todo "-ID={.+}-blocked")
	       (tags-todo "+blocked"))
	      ((org-agenda-sorting-strategy '(todo-state-down))
	       (org-agenda-files  `(,(concat (getenv "HOME") "/org/office.org")))))))


(global-set-key (kbd "<f11>") (lambda () (interactive) (org-agenda "" "c" )))
(global-set-key (kbd "<f12>") (lambda () (interactive) (org-agenda "" "k" )))
;; pomodoro
(defun my-after-load-org () (add-to-list 'org-modules 'org-timer))
(eval-after-load "org" '(my-after-load-org))
(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook '(lambda ()
				(if (not org-timer-current-timer)
				    (org-timer-set-timer '(16)))))
;; archive the DONE tasks
;;; TODO-take the file name as arg to archive, if not org-agenda-files
;;; also  include CANCELLED state for archiving
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries '(lambda ()
		      (setq org-map-continue-from (point-at-bol))
		      (org-archive-subtree))
		   "TODO=\"CANCELED\"|TODO=\"DONE\"" (org-agenda-files)))

(defun my-org-depend-add()
  (interactive)
  (org-set-property
   "BLOCKER"
   (completing-read
    "ID: "
    (save-excursion
      (with-current-buffer "coder.org"
	(goto-char (point-min))
	(let ((blockers (list)))
	  (while (outline-next-heading)
	  (if (org-entry-get nil "ID")
	      (setq blockers (cons (org-entry-get nil "ID") blockers))))
	  blockers))))))

(setq org-speed-commands-user '(("d" . my-org-depend-add)))
