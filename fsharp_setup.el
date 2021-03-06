(add-to-list 'load-path "~/local/emacs-fsharp-mode/")
(require 'fsharp-mode)

(setq fsharp-build-command (list "xbuild"))
(setq fsharp-ac-executable "/home/jsrn/local/FsAutoComplete/FSharp.AutoComplete/bin/Debug/fsautocomplete.exe")
(setq fsharp-ac-complete-command (list "mono" fsharp-ac-executable))

;; TODO: THIS IS STUPID!
;; Used by the function for getting all project files
(setq jsrn-fsharp-current-project "/home/jsrn/code/icfp/2015/icfp2015/icfp2015.fsproj")

;; arguments when running executable
(setq jsrn-fsharp-command-args (list ""))

;; NOTE: It seems that Auto Complete suffers from some mis-configuration: when
;; typing '.' after nothing, fsharp-ac/electric-dot is correctly invoked and
;; triggers auto complete. However, if typing quickly, it seems that we are
;; actually in some sort of auto complete sub-mode, and the '.' does *not*
;; trigger fsharp-ac/electric-dot, and so no auto complete suggestions are
;; presented.
;; Use C-c C-. to invoke auto-complete manually.

(setq jsrn-fsharp-is-debug-config nil)
(defun fsharp-toggle-configuration ()
  "Toggle between Debug and Release build configurations"
  (interactive)
  (setq jsrn-fsharp-is-debug-config (not jsrn-fsharp-is-debug-config))
  (let ((config (if jsrn-fsharp-is-debug-config "Debug" "Release")))
    (setq fsharp-build-command (list "xbuild" (concat "/p:Configuration=" config)))
    (-each (buffer-list)
      (lambda (buf)
        (let ((file (buffer-file-name buf)))
        (when (and file (string-match ".*\\.fs" file))
          (with-current-buffer buf
              (setq compile-command (fsharp-mode-choose-compile-command file))
          )))))
    (message "Set F# Build Configuration to %s" config)
    )
  )

(defun jsrn-fsharp-mode-hook ()
  (setq evil-shift-width 2)
  (column-number-mode)
  (defun fsharp-send-current-block ()
    "Find last blank line and next blank line, and send all in between
to Fsharp buffer"
    (interactive)
    (save-excursion
      (evil-backward-paragraph)
      (let ((beg (point)))
        (evil-forward-paragraph)
        (fsharp-eval-region beg (point))
      ))
    )
  (defun jsrn-fsharp-load-files (files)
    "Reload each file of the list of files into the inferior buffer"
    (interactive)
    (save-excursion
      (fsharp-run-process-if-needed)
      (dolist (file files)
        (fsharp-simple-send inferior-fsharp-buffer-name (concat "#load \"" file "\"")))
      ))
  (defun jsrn-fsharp-reload-project-entire ()
    "Reload ALL files of the project into the inferior buffer, including the
last main file"
    (interactive)
    (save-some-buffers)
    (jsrn-fsharp-load-files fsharp-ac--project-files)
    (fsharp-show-subshell)
    )
  (defun jsrn-fsharp-reload-project-libs ()
    "Reload all but the last file of the project into the inferior buffer"
    (interactive)
    (save-some-buffers)
    (require 'subr-x)
    (let ((files (gethash "Files" (gethash jsrn-fsharp-current-project fsharp-ac--project-data))))
      (jsrn-fsharp-load-files (butlast files))
      )
    (fsharp-show-subshell)
  )
  (fill-keymap fsharp-mode-map
               (kbd "C-<return>") 'fsharp-send-current-block
               (kbd "M-RET")   'fsharp-eval-region
               (kbd "C-SPC")   'completion-at-point
               (kbd "C-c e")   'fsharp-goto-block-up
               [(shift f2)]    'fsharp-toggle-configuration
               [(f5)]          'jsrn-fsharp-reload-project-libs
               [(shift f5)]    'jsrn-fsharp-reload-project-entire
               (kbd "C-.")     'next-error
               (kbd "C-,")     'previous-error
               (kbd "C-c C-z") '(lambda () (interactive)
                                  (fsharp-show-subshell) (other-window 1)))
)
(add-hook 'fsharp-mode-hook 'jsrn-fsharp-mode-hook)



(defun jsrn-inferior-fsharp-mode-hook ()
  (interactive)
  (fill-keymap evil-insert-state-local-map
               (kbd "<return>") 'fsharp-comint-send)
  (fill-keymap inferior-fsharp-mode-map
               (kbd "C-d")     '(lambda () (interactive) (evil-scroll-down 20))
               (kbd "RET")     'fsharp-comint-send
               ))
(add-hook 'inferior-fsharp-mode-hooks 'jsrn-inferior-fsharp-mode-hook) ;; note: non-standard hook

(defun fsharpi-fix-ac ()
  "Auto-complete regularly crashes. When it does, run this function to
fix it again."
  (interactive)
  (setq ac-cursor-color "red")
  (auto-complete-mode 1)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redefine some fsharp functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is redefined to suppress asking about which project file to read
(defun fsharp-ac/load-project (prefix)
  "Load the specified fsproj FILE as a project."
  (interactive "P")
  ;; Prompt user for an fsproj, searching for a default.
  (let* ((found-proj (fsharp-mode/find-fsproj buffer-file-name))
         (proj (if (not found-proj)
                   (read-file-name "Path to project: " nil found-proj t)
                 found-proj))
         )
    (when (fsharp-ac--valid-project-p proj)
      (setq fsharp-ac-intellisense-enabled t)
      (when (not (fsharp-ac--process-live-p))
        (fsharp-ac/start-process))
      ;; Load given project.
      (when (fsharp-ac--process-live-p)
        (log-psendstr fsharp-ac-completion-process
                      (format "project \"%s\"\n" (file-truename proj))))
      proj)
    (message "Project %s loaded" proj)))


;; This is redefined to allow fsharp-build-command being a list so that a config
;; flag can be specified (see fsharp-toggle-configuration)
(defun fsharp-mode-choose-compile-command (file)
  "Format an appropriate compilation command, depending on several factors:
1. The presence of a makefile
2. The presence of a .sln or .fsproj
3. The file's type.
"
  (let* ((fname    (file-name-nondirectory file))
         (dname    (file-name-directory file))
         (ext      (file-name-extension file))
         (proj     (fsharp-mode/find-sln-or-fsproj file))
         (makefile (or (file-exists-p (concat dname "/Makefile"))
                       (file-exists-p (concat dname "/makefile")))))
    (cond
     (makefile          compile-command)
     (proj              (combine-and-quote-strings (append fsharp-build-command (list "/nologo" proj))))
     ((equal ext "fs")  (combine-and-quote-strings (list fsharp-compile-command "--nologo" file)))
     ((equal ext "fsl") (combine-and-quote-strings (list "fslex" file)))
     ((equal ext "fsy") (combine-and-quote-strings (list "fsyacc" file)))
     (t                 compile-command))))


;; This is redefined to pipe output from the exe into a new buffer
;; And to run the configuration which has been built
(defun fsharp-run-executable-file ()
  "Execute a file with specified arguments. If a project is
currently loaded and the output is a .exe file (stored in
FSHARP-AC--OUTPUT-FILE), then this will be used as a default. If
the current system is not Windows then the command string will be
passed to `mono'."
  (interactive)
  (let*  ((config (if jsrn-fsharp-is-debug-config "Debug" "Release"))
         (project (gethash (fsharp-ac--buffer-truename) fsharp-ac--project-files))
         (projdata (when project (gethash project fsharp-ac--project-data)))
         (args jsrn-fsharp-command-args)
         (outputfile (if projdata
                         (replace-regexp-in-string "\\(Release\\|Debug\\)"
                                                   config (gethash "Output"
                                                                   projdata))
                       (error "No project data")))
         ;; (default (if (and outputfile
         ;;                   (s-equals? "exe"
         ;;                              (downcase (file-name-extension outputfile))))
         ;;              (if fsharp-ac-using-mono
         ;;                  (s-concat "mono " outputfile)
         ;;                outputfile)
         ;;            ""))
         ;; (cmd (read-from-minibuffer "Run: "
         ;;                            default
         ;;                            nil
         ;;                            nil
         ;;                            'fsharp-run-executable-file-history))
         (bufname "*F# output*")
         )
    (when (get-buffer bufname)
        (kill-buffer bufname))
    (apply 'start-process
           (append (list "fsharp-process" bufname "mono" outputfile) args))
    ;; (start-process-shell-command cmd bufname)
    (show-buffer (next-window) bufname)
    (with-current-buffer bufname (compilation-mode))
    ))


(message "Loaded fsharp_setup.el")
(provide 'fsharp_setup)

