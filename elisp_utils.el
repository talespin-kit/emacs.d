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

(defun print2scratch (str)
"prints string and string-list to scratch buffer"
  (interactive)
  (if (stringp str)
      (print str (get-buffer "*scratch*")))
  (if (listp str)
      (dolist (ele str)
	(print ele))))
