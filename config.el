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
;; (setq doom-font (font-spec :family "Hack" :size 12))
(setq doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 12))

;; (setq doom-font (font-spec :family "Iosevka Aile" :size 12))

(setq doom-font (font-spec :family "Iosevka Term" :size 12))

(unless (find-font doom-font)
  (setq doom-font (font-spec :family "Fira Code" :size 12)))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-nord-light)
(load-theme 'modus-operandi)

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
;; (after! evil (evil-escape-mode nil))

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

;; (after! org-roam
;;   (progn
;;     ;; Enable org-roam minor mode.
;;     (add-hook 'after-init-hook 'org-roam-mode)
;;     ;; Support org-roam note capture from within Chrome.
;;     (require 'org-roam-protocol)))

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

(setq shell-file-name (executable-find "zsh"))


(map! :leader
      :desc "Fish term" "§" #'bilus/fish)

;; Open eshell vertically.
(set-popup-rule! "^\\*doom:eshell" :side 'right :size 0.2)

;;
;; Typescript
;;

;; Auto-format using tide.
;; (add-hook 'before-save-hook 'tide-format-before-save)


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
;; First install the package:
(use-package flycheck-clj-kondo
  :ensure t)

;; then install the checker as soon as `clojure-mode' is loaded
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

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

(add-hook 'before-save-hook 'gofmt-before-save)

;; (bilus-setup-go-lsp)
(setq-hook! 'go-mode-hook +format-with-lsp nil)

(setq gofmt-command "goimports")
;; (setq gofmt-command "gofumpt")



;; (add-hook 'before-save-hook 'gofmt-before-save)

;; (use-package! flymake-go-staticcheck
;;   :after (add-hook 'go-mode-hook #'flymake-go-staticcheck-enable))

(use-package flycheck
  :defer t
  :hook (go-mode . flycheck-mode)
  :config
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get))

(use-package go-mode
  :defer t
  :hook (go-mode . lsp-deferred)
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (flycheck-define-checker golangci-lint
    "A Go syntax checker using golangci-lint that's 5x faster than gometalinter

See URL `https://github.com/golangci/golangci-lint'."
    :command ("golangci-lint" "run" "--out-format=checkstyle" "--deadline=1m" ".")
    :error-parser flycheck-parse-checkstyle
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end)
     (error line-start (file-name) ":" line ":" (message) line-end))
    :modes go-mode
    :predicate flycheck-buffer-saved-p)
  (add-to-list 'flycheck-checkers 'golangci-lint)

  (defun dsh/flycheck-golangci-lint-setup ()
    (setq flycheck-local-checkers
          '((lsp . ((next-checkers . ((warning . golangci-lint))))))))
  (add-hook 'go-mode-hook #'dsh/flycheck-golangci-lint-setup))

;; debugger support
;; https://emacs-lsp.github.io/dap-mode/page/configuration/#go
;; (require 'dap-dlv-go) -- needs treesitter

;; (remove-hook 'before-save-hook 'gofmt-before-save)

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
;; (bilus/setup-smerge-hydra)
(setq auth-sources '("~/.authinfo"))  ;; Have forge use unencrypted file.

;; Show recent branches in magit.
(defvar exp-feat/recent-branches (make-hash-table :test 'equal))

(defcustom exp-feat/recent-branches-limits 5
  "Limits" :type 'integer :risky t)

(defun exp-feat/magit-insert-recent-branches nil
  "Insert recent branches"
  (let* ((dir (magit-toplevel))
         (curr-branch (magit-get-current-branch))
         (prev-branch (magit-get-previous-branch))
         (rbs (--> (gethash dir exp-feat/recent-branches)
                   (nconc (list prev-branch curr-branch) it)
                   (-distinct it)
                   (-filter (lambda (a) (and a (not (equal a curr-branch)))) it))))
    (when rbs
      (when (> (length rbs) exp-feat/recent-branches-limits)
        (--> (1- exp-feat/recent-branches-limits)
             (nthcdr it rbs)
             (setcdr it nil)))
      (puthash dir rbs exp-feat/recent-branches)
      (magit-insert-section (rb "rb")
        (magit-insert-heading "Recent branches")
        (dolist (it-branch rbs)
          (let ((output (magit-rev-format "%h %s" it-branch)))
            (string-match "^\\([^ ]+\\) \\(.*\\)" output)
            (magit-bind-match-strings (commit summary) output
              (when (and t (equal summary ""))
                (setq summary "(no commit message)"))
              (magit-insert-section (branch it-branch)
                (insert (propertize commit
                                    'font-lock-face 'magit-hash) ?\s)
                (insert (propertize it-branch
                                    'font-lock-face 'magit-branch-local) ?\s)
                (insert (funcall magit-log-format-message-function
                                 it-branch summary) ?\n)))))))))

(after! magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'exp-feat/magit-insert-recent-branches
                          'magit-insert-stashes
                          'append)  )

;;
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
;; (setq plantuml-jar-path (expand-file-name "~/.emacs.d/bin/plantuml.jar"))
;; (setq plantuml-default-exec-mode 'jar)

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

;; Show vterm popup on the right (SPC o t)
(after! vterm
  (set-popup-rule! "*doom:vterm-popup:*" :size 0.25 :vslot -4 :select t :quit nil :ttl t :side 'right))

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(after! flx-ido
  (flx-ido-mode)
  (flx-ido-mode)
  (flx-ido-mode)
  )

;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (org-roam-mode 0)
;;             ))

;; Haskell
;; (after! direnv
;;   (use-package direnv
;;     :ensure t
;;     :config
;;     (direnv-mode)))

(use-package lsp-mode
  :ensure t
  :hook ((haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-haskell
  :ensure t)

(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "floskell"))

;; (setq haskell-stylish-on-save t)
(setq haskell-mode-stylish-haskell-path "brittany")

(defun haskell-stylish-on-save ()
  "Function formats haskell buffer with brittany on save."
  (when (eq major-mode 'haskell-mode)
    (haskell-mode-stylish-buffer)
    ;;(shell-command-to-string (format "brittany --write-mode inplace %s" buffer-file-name))
    ;;(revert-buffer :ignore-auto :noconfirm)
    ))

(add-hook 'after-save-hook 'haskell-stylish-on-save)

(add-hook 'haskell-mode-hook (progn (remove-hook 'before-save-hook #'lsp-format-buffer t)))



(setq dired-listing-switches "-alh --group-directories-first")

(setq org-mobile-directory "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents")

(after! chatgpt
  (require 'python))

(map! :leader
      :desc "Chatgpt" "b c" #'chatgpt-query)

;; Disable persistent undo history. At least temporarily due to "can not recover" errors.
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

;; Use when getting Too many files open error.
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))


;; https://github.com/doomemacs/doomemacs/issues/5317
(setq org-clock-auto-clock-resolution nil)

;; (use-package! elixir-ts-mode
;;   :mode "\\.heex\\'")

;; (after! elixir-ts-mode
;;     (add-hook! elixir-mode #'elixir-ts-mode))


(defun bilus-dts-projects-todo ()
  (let ((dts-dir "/Users/martinb/dev/Tooploox/DTS/"))
    (mapcar
     (lambda (proj) (concat dts-dir proj "/todo.org"))
     (list "conrad" "mmd"))))

(defun bilus-todos ()
  (append (bilus-dts-projects-todo)
          (list  "/Users/martinb/git/org/todo.org")))


;; Hiding DONE org tasks
(defun bilus/org-get-folded-state ()
  (cond
   ((not (or (org-at-item-p) (org-at-heading-p)))
    'not-at-node)
   ((org-before-first-heading-p)
    'not-at-node)
   (t
    (let (eoh eol eos has-children children-skipped struct)
      ;; First, determine end of headline (EOH), end of subtree or item
      ;; (EOS), and if item or heading has children (HAS-CHILDREN).
      (save-excursion
        (if (org-at-item-p)
            (progn
              (beginning-of-line)
              (setq struct (org-list-struct))
              (setq eoh (point-at-eol))
              (setq eos (org-list-get-item-end-before-blank (point) struct))
              (setq has-children (org-list-has-child-p (point) struct)))
          (org-back-to-heading)
          (setq eoh (save-excursion (outline-end-of-heading) (point)))
          (setq eos (save-excursion (org-end-of-subtree t t)
                                    (when (bolp) (backward-char)) (point)))
          (setq has-children
                (or (save-excursion
                      (let ((level (funcall outline-level)))
                        (outline-next-heading)
                        (and (org-at-heading-p t)
                             (> (funcall outline-level) level))))
                    (save-excursion
                      (org-list-search-forward (org-item-beginning-re) eos t)))))
        ;; Determine end invisible part of buffer (EOL)
        (beginning-of-line 2)
        (while (and (not (eobp)) ;; this is like `next-line'
                    (get-char-property (1- (point)) 'invisible))
          (goto-char (next-single-char-property-change (point) 'invisible))
          (and (eolp) (beginning-of-line 2)))
        (setq eol (point)))
      (cond
       ((= eos eoh)
        'empty-node)
       ((or (>= eol eos)
            (not (string-match "\\S-" (buffer-substring eol eos))))
        'folded)
       (t
        'not-folded))))))

(defun bilus/org-tree-can-fold-p ()
  (not (member (bilus/org-get-folded-state) (list 'folded 'empty-node))))

(defun bilus/org-cycle-until-folded ()
  (while (bilus/org-tree-can-fold-p)
    (org-cycle)))

(defun bilus/org-hide-done-entries-in-range (start end)
  (save-excursion
    (goto-char end)
    (while (and (outline-previous-heading) (> (point) start))
      (when (org-entry-is-done-p)
        (bilus/org-cycle-until-folded)))))

(defun bilus/org-hide-done-entries-in-region (start end)
  (interactive "r")
  (bilus/org-hide-done-entries-in-range start end))

(defun bilus/org-hide-done-entries-in-buffer ()
  (interactive)
  (bilus/org-hide-done-entries-in-range (point-min) (point-max)))


;; (evil-collection-define-key 'normal 'smerge-mode-map
;;   "n" 'smerge-next
;;   "p" 'smerge-prev
;;   "k" 'smerge-prev
;;   "j" 'evil-next-line
;;   "k" 'evil-previous-line
;;   "a" 'smerge-keep-all
;;   "b" 'smerge-keep-base
;;   "m" 'smerge-keep-mine
;;   "o" 'smerge-keep-other
;;   "c" 'smerge-keep-current
;;   "C" 'smerge-combine-with-next
;;   "R" 'smerge-refine
;;   "u" 'undo-tree-undo)
(general-define-key
 :states 'normal
 :modes 'smerge-mode
 :prefix ", d"
 "n" 'smerge-next
 "p" 'smerge-prev
 "l" 'smerge-keep-lower
 "u" 'smerge-keep-upper
 "a" 'smerge-keep-all
 "X" 'smerge-keep-base
 "x" 'smerge-swap
 "r" 'smerge-resolve)




(add-to-list 'auto-mode-alist '("\\.templ\\'" . web-mode))


;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<right>" . 'copilot-accept-completion)
                                        ;("<tab>" . 'copilot-accept-completion)
                                        ;("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("M-n" . 'copilot-next-completion)
              ("M-p" . 'copilot-previous-completion)))

;; https://github.com/zerolfx/copilot.el/issues/193
(after! (evil copilot)
  ;; Define the custom function that either accepts the completion or does the default behavior
  (defun my/copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

  ;; Bind the custom function to <tab> in Evil's insert state
  (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))

(after! copilot
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent)))


;; Styling for org mode.
(progn
  ;; Minimal UI
  (package-initialize)
  (menu-bar-mode -1)

  (tool-bar-mode -1)
  (scroll-bar-mode -1)


  ;; Choose some fonts
  ;; (set-face-attribute 'default nil :family "Iosevka")
  ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 20)
     (internal-border-width . 20)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  (global-org-modern-mode))

(defun tvaughan/untabify ()
  "Preserve initial tab when 'makefile-mode."
  (interactive)
  (save-excursion
    (if (derived-mode-p 'makefile-mode)
        (progn
          (goto-char (point-min))
          (while (not (eobp))
            (skip-chars-forward "\t")
            (untabify (point) (line-end-position))
            (forward-line 1)))
      (untabify (point-min) (point-max)))))

(add-hook 'before-save-hook 'tvaughan/untabify)

(defun bilus-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-horizontally)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          ;; (shrink-window (/ (frame-width) 4))
          )))))
(add-hook 'compilation-mode-hook 'bilus-compilation-hook)

(after! treemacs
  (treemacs-follow-mode))

;; (after! centaur-tabs
;;   (centaur-tabs-group-by-projectile-project)
;;   (setq
;;    centaur-tabs-style "bar"
;;    centaur-tabs-height 15)
;;   (centaur-tabs-change-fonts (face-attribute 'default :font) 110))

(after! noccur
  (map!
   :leader "sn" #'noccur-dired)
  (map!
   :leader "pn" #'noccur-project))
