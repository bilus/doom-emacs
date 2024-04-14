;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
                                        ;(unpin! t)

;; ...but to unpin a single package:
                                        ;(unpin! pinned-package)
;; Use it to unpin multiple packages
                                        ;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;;
;; General
;;
;; (package! super-save)       ;; Save buffer when focus lost.
(package! evil-iedit-state)


;;
;; Clojure
;;
(package! evil-cleverparens) ;; paredit alternative
(package! ivy-cider
  :recipe (:host github :repo "rschmukler/ivy-cider"))

;;
;; Kubernetes
;;
;; (package! kubel)
;; (package! kubel-evil)

;;
;; Blogging
;;
;; (package! prodigy)

;;
;; Go
;;
;; (package! lsp-mode)
;; (package! lsp-ui)
(package! company-lsp)

;;
;; Ruby
;;
;; (package! rvm)


;;
;; Python
;;
(package! yapfify)

;;
;; Experimental/workarounds
;;
(package! yaml-mode)  ;; yaml module is broken
(package! feature-mode)
(package! evil-iedit-state)
(package! ox-hugo)


(package! ob-http)
(package! ob-mermaid)

;; (package! nix-haskell-mode)
;; (package! org-roam-server)

(package! org-reveal)

(package! format-all)

(package! org-superstar)

;; Needed by (editor +onsave).
(package! evil-escape)

(package! flx-ido)

;; Haskell
(package! direnv)


;; org-encode-time issue: https://github.com/doomemacs/doomemacs/issues/6491
(package! org
  :recipe (:host github
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           :build t
           :pre-build
           (with-temp-file "org-version.el"
             (let ((version
                    (with-temp-buffer
                      (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                      (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                          (match-string-no-properties 1)
                        "Unknown"))))
               (insert (format "(defun org-release () %S)\n" version)
                       (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                               version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                       "(provide 'org-version)\n"))))
  :pin "971eb6885ec996c923e955730df3bafbdc244e54")

(package! chatgpt
  :recipe (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el")))

;; (package! elixir-ts-mode)
(package! go-errcheck)
(package! flymake-go-staticcheck)


(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! org-modern)

(package! sqlite3)

(package! vertico-prescient)
(package! prescient)

(package! noccur)
