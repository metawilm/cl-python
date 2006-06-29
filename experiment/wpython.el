(defun clpython-eval-region (start end)
  "Call with region, otherwise current line is executed"
  (interactive "r")

  (save-excursion

    ;; normalize region
    ;(goto-char start) (beginning-of-line) (setq start (point-marker))
    ;(goto-char end)   (beginning-of-line) (setq end (point-marker))

    (let* ((code-string (buffer-substring start end)))
      
      (set-buffer (get-buffer-create "clpython-tmp-buffer"))
      (erase-buffer)
      
      (let ((cmds `((in-package :python)
		    (run-py-code-string ,code-string))))
	
	(dolist (sexp cmds)
	  (insert (format "%S\n" sexp)))
	
	(fi::eval-region-internal (point-min) (point-max) nil)))))

(global-set-key "\C-x\p" 'clpython-eval-region)

" test:
print f, 1+2
def f(x): x+10
g()
x = 4
def g():
  print 3

"

