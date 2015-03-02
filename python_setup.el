;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Emacs-wide loads, vars etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python-mode)

; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
                                ; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

                                ; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
                                ; don't split windows
(setq py-split-windows-on-execute-p nil)
                                ; try to automagically figure out indentation
(setq py-smart-indentation t)

(add-to-list 'load-path "/home/jsrn/local/Pymacs/")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(setq pymacs-python-command "python2")
;; The following is to add custom Pymacs code which is run by Python
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; Setup environment for when running Pymacs and python
(setq python-custom-libs (list "Pymacs" "rope" "ropemode" "ropemacs"))
(setq pythonpath "")
(cl-loop for lib in python-custom-libs
         do (setq pythonpath (concat pythonpath (if (string-equal "" pythonpath)
                                                    "" ":") "/home/jsrn/local/"
                                                    lib "/build/lib/")))
(setenv "PYTHONPATH" pythonpath)
(setenv "PYMACS_PYTHON" "python2")
(setq pymacs-load-path '("/home/jsrn/local/rope/build/lib/rope"
                         "/home/jsrn/local/ropemacs/build/lib/ropemacs"
                         "/home/jsrn/local/ropemode/build/lib/ropemode"
                         ))
(require 'pymacs)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for setting up each buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-python-mode-hook ()
  (interactive)
  (pymacs-load "ropemacs" "rope-")
  (require 'pretty-lambdada) ;typeset word "lambda" as the symbol
  (pretty-lambda-mode 1)
  )

(add-hook 'python-mode-hook 'jsrn-python-mode-hook)

;; This file is excecuted when a python buffer is opened, so the above hook is
;; not run for that file. Therefore, run the hook.
(jsrn-python-mode-hook)



(message "Loaded python_setup.el")
(provide 'python_setup)