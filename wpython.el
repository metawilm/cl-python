(defun wilm (start end)
  (interactive "r")

  ;; normalize region
  (goto-char end)   (beginning-of-line) (setq end   (point-marker))
  (goto-char start) (beginning-of-line) (setq start (point-marker))

  (let ((tmp-file (make-temp-name "/tmp/clpython_")))
    (write-region start end tmp-file nil)
    (message (format "written %S characters from region" (- end start)))

    ;; stuff with tmp-file ...

    (delete-file tmp-file)))
