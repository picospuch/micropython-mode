(define-derived-mode micropython-mode python-mode "MicroPython Mode"
  (make-local-variable 'python-shell-interpreter)
  (make-local-variable 'python-shell-interpreter-args)
  (make-local-variable 'python-shell-interpreter-interactive-arg)
  (make-local-variable 'python-shell-completion-native-enable)
  (make-local-variable 'python-shell-setup-codes)
  (setq python-shell-interpreter "cu")
  (setq python-shell-interpreter-args "-l /dev/cu.usbmodem141101 -s 115200")
  (setq python-shell-interpreter-interactive-arg nil)
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq comint-process-echoes t)))

  (defun micropython-shell-send-string (string &optional process msg)
    "Send STRING to inferior Python PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
    (interactive
     (list (read-string "Python command: ") nil t))
    (let ((process (or process (python-shell-get-process-or-error msg))))
      (if (string-match ".\n+." string)   ;Multiline.
          (progn
            (setq string (format "\x01\x05A\x01%s\x04" string))
            (comint-send-string process string)
            (with-current-buffer (process-buffer process)
                (comint-delete-output)))
        (comint-send-string process string)
        (when (or (not (string-match "\n\\'" string))
                  (string-match "\n[ \t].*\n?\\'" string))
          (comint-send-string process "\n")))))

  (advice-remove 'python-shell-send-string
                 (symbol-function 'micropython-shell-send-string))
  (advice-add 'python-shell-send-string
              :override
              (symbol-function 'micropython-shell-send-string))

  (defun comint-send-string-with-cr-eol (&rest args)
    (let* ((args (car args))
           (process (car args))
           (string (cadr args)))
      (list process (replace-regexp-in-string "\n" "\r" string))))
  
  (defun micropython-shell-send-setup-code (origin &rest)
    (message "micropython-setup-code"))

  (defun run-micropython (&rest args) ()
         (comint-send-string
          (python-shell-get-process-or-error
           "no-micorpython-process")
          "\r\r"))
  
  (advice-remove 'comint-send-string
                 (symbol-function 'comint-send-string-with-cr-eol))
  (advice-add 'comint-send-string
              :filter-args
              (symbol-function 'comint-send-string-with-cr-eol))
  
  (advice-remove 'python-shell-send-setup-code
                 (symbol-function 'micropython-shell-send-setup-code))
  (advice-add 'python-shell-send-setup-code
              :around
              (symbol-function 'micropython-shell-send-setup-code))

  (advice-remove 'run-python
                 (symbol-function 'run-micropython))
  (advice-add 'run-python
            :after
            (symbol-function 'run-micropython)))

(defun list-micropython-advices (&optional clear)
  (advice-mapc
   (lambda (f p)
     (print f)
     (if clear (advice-remove 'comint-send-string f)))
   'comint-send-string)
     
  (advice-mapc
   (lambda (f p)
     (print f)
     (if clear (advice-remove 'python-shell-send-setup-code f)))
   'python-shell-send-setup-code)

  (advice-mapc
   (lambda (f p)
     (print f)
     (if clear (advice-remove 'run-python f)))
   'run-python))

(list-micropython-advices t)

(provide 'micropython-mode)
