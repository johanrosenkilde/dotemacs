;; The following was my CEDET setup, but it never came to work satisfactorily
;; There remains the following issues:
;; 
;; * DEALBREAKER: All files outside the current directory were not really
;; understood. It seemed that forward declarations confused it.
;;
;; * I never really found an elegant way to call the functions. Most important
;; help is coming from the functions with key bindings as below, but others
;; include e.g. semantic-ia-describe-class. They often seemed to have no or
;; little information. And the extra buffer didn't go away with 'q' which was
;; annoying.
;;
;; * Some of the submodes below should be disabled (highlight-func-mode), while
;; the Decorate Tags (in Development menu) didn't get activated by default (I
;; think)
;;
;; * The speedbar was pretty useless; showed classes in my local project, but
;; again not external files. And speedbar font size is ridicolous, but thats
;; general for my current emacs setup.
;;
;; * The version that worked was loaded as first part of init.el, so it was
;; loaded for all files always. Obviously, this should be fixed, e.g. by
;; creating a function which needs to be called to activate everything
;; "globally", and then rely on desktop-clear to shift it out when doing
;; something else.
;;
;; eassist and the key bindings below was pretty cool -- does this work without cedet?

(setq cedet-root-path (file-name-as-directory "~/.emacs.d/cedet_snapshot8463/"))
(load-file (concat cedet-root-path "cedet-devel-load.el"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))

;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-syntax-mode) 
(setq semantic-idle-work-parse-neighboring-files-flag t)

;; Activate semantic
(semantic-mode 1)
(add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))

;; load contrib library
(require 'eassist)

;; customisation of modes
(defun alexott/cedet-hook ()
(local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
(local-set-key "\C-c?" 'semantic-ia-complete-symbol)
;;
(local-set-key "\C-c>" 'semantic-complete-analyze-inline)
(local-set-key "\C-c=" 'semantic-decoration-include-visit)
 
(local-set-key "\C-cj" 'semantic-ia-fast-jump)
(local-set-key "\C-cq" 'semantic-ia-show-doc)
(local-set-key "\C-cs" 'semantic-ia-show-summary)
(local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
)

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
(local-set-key "\C-ct" 'eassist-switch-h-cpp)
(local-set-key "\C-xt" 'eassist-switch-h-cpp)
(local-set-key "\C-ce" 'eassist-list-methods)
(local-set-key "\C-c\C-r" 'semantic-symref)
)

(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(when (cedet-ectag-version-check t)
(semantic-load-enable-primary-ectags-support))

;; SRecode
(global-srecode-minor-mode 1)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)
