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
; custom agenda views
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

(defun print2scratch (str)
"prints string and string-list to scratch buffer"
  (interactive)
  (if (stringp str)
      (print str (get-buffer "*scratch*")))
  (if (listp str)
      (dolist (ele str)
	(print ele))))

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
; python compilation
(require 'python)
(defun my-compile ()
  "Use compile to run python programs"
  (interactive)
  (save-buffer)
  (compile (concat "python " (buffer-file-name))))
(setq compilation-scroll-output t)

(setq python-mode-hook
      '(lambda()
	 (local-set-key "\C-c\C-c" 'my-compile)))
;;rope for python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; cscope
(setq load-path (cons (concat (getenv "HOME") "/.emacs_packages/cscope-15.7a/contrib/xcscope") load-path))
(require 'xcscope)
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]
  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]  'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)

(setq cscope-initial-directory (concat (getenv "HOME") "/src/linux"))

; http://stackoverflow.com/questions/9617183/how-to-find-which-file-provided-the-feature-in-emacs-elisp
(defun locate-feature (feature)
  "Return file name as string where `feature' was provided"
  (interactive "Sfeature: ")
  (dolist (file-info load-history)
    (mapc (lambda (element)
            (when (and (consp element)
                       (eq (car element) 'provide)
                       (eq (cdr element) feature))
              (when (called-interactively-p 'any)
                (message "%s defined in %s" feature (car file-info)))
              (return (car file-info))))
          (cdr file-info))))

 (defun th-outline-regexp ()
  "Calculate the outline regexp for the current mode."
  (let ((comment-starter (replace-regexp-in-string
                          "[[:space:]]+" "" comment-start)))
    (when (string= comment-start ";")
      (setq comment-starter ";;"))
    (concat "^" comment-starter "\\*+")))

(add-hook 'outline-minor-mode-hook
	  'th-outline-minor-mode-init)

; heading
;;data
;;; heading2
;;;data2
(load (concat (getenv "HOME") "/.emacs_packages/xscheme/xscheme.el")