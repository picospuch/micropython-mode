;;; micropython-mode.el --- micropython mode derived from python mode -*- lexical-binding: t; -*-

;; copyright 2021 picospuch
;; version 1.0

;;; Code:

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
          (setq string (format "\x01%s\x04\x02" string))
          (comint-send-string process string))
      (comint-send-string process string)
      (when (or (not (string-match "\n\\'" string))
                (string-match "\n[ \t].*\n?\\'" string))
        (comint-send-string process "\n")))))

(defun micropython-mode--comint-send-string-advice (args)
  (let ((process (car args))
        (string (cadr args)))
    (setq string (micropython-mode--nl-to-cr string))
    (list process string)))
  
(defun micropython-shell-leave-raw-mode (&optional process msg)
  (interactive)
  (let ((process (or process (python-shell-get-process-or-error msg))))
    (setq string (format "\x02"))
    (comint-send-string process string)))

(defun micropython-shell-send-simple-string (string &optional process msg)
  (let ((process (or process (python-shell-get-process-or-error msg))))
    (comint-send-string process string)))

(defun micropython-mode--nl-to-cr (input)
  (replace-regexp-in-string "\n" "\r" input))

(defun micropython-shell-send-setup-code (origin &rest args)
  (message "micropython-setup-code"))

(defun run-micropython (&rest args) ()
       (let ((process (python-shell-get-process-or-error "no-micorpython-process")))
         (comint-send-string process "\n\n")
         (with-current-buffer (process-buffer process)
           (local-set-key "\C-c\C-c" (lambda ()
                                       (interactive)
                                       (comint-send-string process "\C-c\C-b"))))))

(define-derived-mode micropython-mode python-mode "MicroPython Mode"
  (make-local-variable 'python-shell-interpreter)
  (make-local-variable 'python-shell-interpreter-args)
  (make-local-variable 'python-shell-interpreter-interactive-arg)
  (make-local-variable 'python-shell-completion-native-enable)
  (make-local-variable 'python-shell-setup-codes)
  ;;(setq python-shell-interpreter "/Users/spuch/process/uucp/work2/uucp-1.07/cu")
  ;;(setq python-shell-interpreter "~/ws/p-micropython/ports/unix/micropython")
  (setq python-shell-interpreter "cu")
  (setq python-shell-interpreter-args "-l /dev/cu.usbmodem141101 -s 115200")
  (setq python-shell-interpreter-interactive-arg nil)
  (setq python-shell-completion-native-enable nil)

  ;; disable eldoc
  (eldoc-mode 0)

  ;; presume the subprocess echoes
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq comint-process-echoes t)))
  
  (micropython-mode-remove-advice)
  (micropython-mode-add-advice))

(setq micropython-mode--repl-raw-mode-fsm-state-set
      (list
       'repl-raw-mode-start
       'repl-raw-mode-request-sent
       'repl-raw-mode-text-sent
       'repl-raw-mode-finished
       'repl-raw-mode-stop
       ))
(defvar micropython-mode--raw-mode-fsm-state 'repl-raw-mode-start)

(defun rx-fsm-input ())
(defun tx-fsm-input ())

(defun micropython-mode--raw-mode-output-filter (output)
  "rx-fsm-input of micropython raw mode protocol fsm."
  (when micropython-mode--raw-mode-state

          ;; Skip pdb prompts and reset the buffer.
          (setq python-shell--first-prompt-received-output-buffer nil)
        (set (make-local-variable 'python-shell--first-prompt-received) t)
        (setq python-shell--first-prompt-received-output-buffer nil)
        (with-current-buffer (current-buffer)
          (let ((inhibit-quit nil))
            (run-hooks 'python-shell-first-prompt-hook))))
  output)


(defun list-micropython-advices (&optional clear)
  (setq mpy-advice-list
        '(comint-send-string
          python-shell-send-setup-code
          run-python
          python-shell-send-string))
  (dolist (e mpy-advice-list)
    (advice-mapc
     (lambda (f p)
       (print f)
       (if clear (advice-remove e f)))
     e)))
     
(defun micropython-mode-add-advice()
  (advice-add 'comint-send-string
              :filter-args
              (symbol-function 'micropython-mode--comint-send-string-advice))
  (advice-add 'python-shell-send-setup-code
              :around
              (symbol-function 'micropython-shell-send-setup-code))
  (advice-add 'run-python
              :after
              (symbol-function 'run-micropython))
  (advice-add 'python-shell-send-string
              :override
              (symbol-function 'micropython-shell-send-string)))

(defun micropython-mode-remove-advice()
  (advice-remove 'comint-send-string
                 (symbol-function 'micropython-mode--comint-send-string-advice))
  (advice-remove 'python-shell-send-setup-code
                 (symbol-function 'micropython-shell-send-setup-code))
  (advice-remove 'run-python
                 (symbol-function 'run-micropython))
  (advice-remove 'python-shell-send-string
                 (symbol-function 'micropython-shell-send-string)))

(provide 'micropython-mode)
