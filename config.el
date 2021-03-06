;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Marcin Bilski"
      user-mail-address "gyamtso@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-font (font-spec :family "Hack" :size 13))

(unless (find-font doom-font)
  (setq doom-font (font-spec :family "Fira Code" :size 13)))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dracula)
(setq doom-theme 'doom-nord-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/git/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)

;; Load env variables. Refresh using ~/.emacs.d/bin/doom env.
(doom-load-envvars-file "~/.emacs.d/.local/env")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;
;; General
;;

;; Map SPC w w to (dvorak-friendly) ace window selection.
(map! :leader
      :desc "ace-window" "w w" #'ace-window
      :desc "Search Project" "/" #'+default/search-project)

(after! ace-window
  (setq aw-keys '(?a ?o ?e ?u ?i ?1 ?2 ?3 ?4 ?5)))

;; Start in full screen.
;; (setq ns-use-native-fullscreen t)
;; (toggle-frame-fullscreen)

;; Do not show line numbers.
(setq display-line-numbers-type nil)

;; Trim trailing newlines and lines on save.
(defun bilus/cleanup-whitespaces ()
  (delete-trailing-whitespace)
  (doom/delete-trailing-newlines))

(add-hook 'before-save-hook 'bilus/cleanup-whitespaces)

;; Auto-save buffers when focus lost
;; TODO: Seems to not work when switching windows/buffers with/without ace.
;; (use-package! super-save
;;   :config (progn
;;             (super-save-mode +1)
;;             (add-to-list 'super-save-triggers 'ace-window)))

;; Map SPC f / to autocomplete file name at point
(map! :leader
      :desc "Autocomplete path" "f /" #'comint-dynamic-complete-filename)

(map! :leader
      :desc "Widen" "b w" #'widen)

;; I don’t use evil-escape-mode, so I may as well turn it off, I’ve heard it
;; contributes a typing delay. I’m not sure it’s much, but it is an extra
;; pre-command-hook that I don’t benefit from, so…
(after! evil (evil-escape-mode nil))

;;
;; Org mode
;;
;;


;; SPC m x - execute BEGIN_SRC code block
(map! :localleader
      :map org-mode-map
      :desc "Exec src block" "x" #'org-babel-execute-src-block)

(defun bilus/org-insert-clipboard-image (&optional file)
  (interactive "F")
  (shell-command (concat "pngpaste " file))
  (insert (concat "[[" file "]]"))
  (org-display-inline-images))

;; SPC m I - insert image from clipboard
;; (map! :localleader
;;       :map org-mode-map
;;       :desc "Insert clipboard PNG" "P" #'bilus/org-insert-clipboard-image)

(after! org-roam
  (progn
    ;; Enable org-roam minor mode.
    (add-hook 'after-init-hook 'org-roam-mode)
    ;; Support org-roam note capture from within Chrome.
    (require 'org-roam-protocol)))

;; Searching notes.
;; Haven't been using it.
;; (after! deft
;;   (setq deft-directory "/Users/martinb/git/org/roam"))

;; Open roam graphs in Chrome.
(setq org-roam-graph-viewer "/usr/bin/open")

(setq org-agenda-files '("~/git/org" "~/git/org/roam"))

;; Haven't been using it.
;; (use-package org-roam-server
;;   :ensure t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8077
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))
;;
;; Terminal/shell
;;

(defun bilus/fish ()
  (interactive)
  (if-let (fish (get-buffer "*ansi-term*"))
      (cond ((eq fish (window-buffer (selected-window)))
             (message "Fish shell already focused"))
            ((get-buffer-window fish)
             (select-window (get-buffer-window fish)))
            (t
             (switch-to-buffer fish)))
    (ansi-term "/usr/local/bin/fish")))

(map! :leader
      :desc "Fish term" "§" #'bilus/fish)

;; Open eshell vertically.
(set-popup-rule! "^\\*doom:eshell" :side 'right :size 0.2)

;;
;; Typescript
;;

;; Auto-format using tide.
(add-hook 'before-save-hook 'tide-format-before-save)


;;
;; Clojure
;;

(use-package! evil-cleverparens)
(after! evil-cleverparens
  (add-hook! emacs-lisp-mode #'evil-cleverparens-mode)
  (add-hook! clojure-mode #'evil-cleverparens-mode)
  (add-hook! clojurescript-mode #'evil-cleverparens-mode))

(map!
 (:after clojure-mode
  (:map clojure-mode-map
   :leader
   :n "\\" #'ivy-cider-apropos
   :n "DEL" #'ivy-cider-browse-ns)))

(setq cider-enhanced-cljs-completion-p nil)  ;; https://github.com/clojure-emacs/cider/issues/2714


;; (after! cider
;;   (defun clj-format ()
;;     (save-excursion
;;       (cider-format-buffer)))

;;   (defun add-clj-format-before-save ()
;;     (interactive)
;;     (add-hook 'before-save-hook
;;               'clj-format
;;               t
;;               t))

;;   (add-hook 'clojure-mode-hook 'add-clj-format-before-save)
;;   (add-hook 'clojurescript-mode-hook 'add-clj-format-before-save))


;;
;; Golang
;;
;; (setq gofmt-command "goimports")

;; (add-hook 'before-save-hook 'gofmt-before-save)
;; (bilus-setup-go-lsp)
(setq gofmt-command "goimports")

(add-hook 'before-save-hook 'gofmt-before-save)
(use-package! company-lsp
  :after (push 'company-lsp company-backends))


;;
;; Kubernetes
;;
;; (use-package! kubel)
;; (use-package! kubel-evil)
;; (map! :leader
;;       :desc "kubel" "o k" #'kubel)


;;
;; Blog
;;
;; (setq org-publish-project-alist
;;       '(("posts"
;;          :base-directory "posts/"
;;          :base-extension "org"
;;          :publishing-directory "public/"
;;          :recursive t
;;          :postamble nil
;;          :publishing-function org-twbs-publish-to-html
;;          ;; :publishing-function org-html-publish-to-html
;;          :auto-sitemap t)
;;         ("all" :components ("posts"))))

;; (prodigy-define-service
;;   :name "bilus.dev@localhost"
;;   :command "python2"
;;   :args '("-m" "SimpleHTTPServer" "8123")
;;   :cwd "~/dev/blog"
;;   :tags '(file-server)
;;   :stop-signal 'sigkill
;;   :kill-process-buffer-on-stop t)

;;
;; Magit
;;
;;
(bilus/setup-smerge-hydra)
(setq auth-sources '("~/.authinfo"))  ;; Have forge use unencrypted file.

;;
;; Ruby
;;
(map! :localleader
      :map ruby-mode-map
      "v" #'rvm-use)


;;
;; Python
;;
(after! yapfify
  (add-hook 'python-mode-hook 'yapf-mode))


;;
;; Org mode
;;
(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/bin/plantuml.jar"))

;;
;; Experimental
;;
(map! :leader
      :desc "Connect Bluetooth device" "o h" #'ar/ivy-bluetooth-connect)

(defun bilus/flycheck-prioritize-govet ()
  (delete 'go-vet flycheck-checkers)
  (add-to-list 'flycheck-checkers 'go-vet))

;; Ensure go-vet is run before golangci-lint to avoid "Can't run linter goanalysis_metalinter",
;; underscoring "package" instead of showing the location of the error, for any non-compilable Go source code,
;; caused by golangci-lint expecting code to compile.
(after! go-mode
  (advice-add 'flycheck-golangci-lint-setup :after #'bilus/flycheck-prioritize-govet))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (direnv projectile-direnv ob-http exec-path-from-shell rvm org-roam-server keycast selectric-mode ox-hugo prodigy org-alert json-mode gherkin-mode evil-iedit-state))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(after! ob-mermaid
  (setq ob-mermaid-cli-path "/usr/local/bin/mmdc"))

;; Blogging
(after! ox
  (use-package! ox-hugo))

(after! org-reveal
  (setq org-reveal-root "https://revealjs.com/")
  (setq org-reveal-title-slide nil))

(after! elm-mode
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))
