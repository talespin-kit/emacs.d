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
