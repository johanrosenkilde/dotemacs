(setq semantic-default-submodes '(
      'global-semanticdb-minor-mode
      'global-cedet-m3-minor-mode
      'global-semantic-highlight-func-mode
      'global-semantic-idle-scheduler-mode
      'global-semantic-idle-completions-mode
      'global-semantic-idle-summary-mode
))
(defun jsrn-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'jsrn-semantic-hook)
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)

(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)

;TODO: Make this somehow load only when doing Ogre projects.
;(semantic-add-system-include "~/local/ogre/OgreMain/include" 'c++-mode)
;(semantic-add-system-include "~/local/ogre/Components/Paging/include" 'c++-mode)
;(semantic-add-system-include "~/local/ogre/Components/Property/include" 'c++-mode)
;(semantic-add-system-include "~/local/ogre/Components/RTShaderSystem/include" 'c++-mode)
;(semantic-add-system-include "~/local/ogre/Components/Terrain/include" 'c++-mode)

(defun jsrn/c-mode-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  ;(local-set-key "\C-c+" 'semantic-tag-folding-show-block)
  ;(local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
  (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'jsrn/c-mode-cedet-hook)
;(global-semantic-tag-folding-mode 1)
;(global-semantic-idle-tag-highlight-mode 1)


;; gnu global support
(require 'cedet-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;;; CEDET projects
(ede-cpp-root-project"horrorville"
   :file "~/code/horrorville/trunk/CMakeLists.txt"
   :include-path '("/src"
		   "/src/Horrorville"
		   "/src/RapidXML")
   :system-include-path
   '("/home/jsrn/local/ogre/OgreMain/include"
     "/home/jsrn/local/ogre/Components/Paging/include"
     "/home/jsrn/local/ogre/Components/Property/include"
     "/home/jsrn/local/ogre/Components/RTShaderSystem/include"
     "/home/jsrn/local/ogre/Components/Terrain/include/"))
