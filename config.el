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
(setq doom-font (font-spec :family "Fira Code" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/git/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)


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
      :desc "ace-window" "w w" #'ace-window)

(after! ace-window
  (setq aw-keys '(?a ?o ?e ?u ?i ?1 ?2 ?3 ?4 ?5)))


;; Start in full screen.
(toggle-frame-fullscreen)

;; Auto-save buffers when focus lost
;; TODO: Seems to not work when switching windows/buffers with/without ace.
(def-package! super-save
  :config (progn
            (super-save-mode +1)
            (add-to-list 'super-save-triggers 'ace-window)))

;;
;; Org mode
;;
;;

;; t --> org-todo (choose TODO > DONE etc.)
(map! :after evil-org
      :map evil-org-mode-map
      :m "t" #'org-todo)

;;
;; Terminal
;;

(defun bilus-fish ()
  (interactive)
  (if-let (fish (get-buffer "*ansi-term*"))
      (cond ((eq fish (window-buffer (selected-window)))
             (message "Visible and focused"))
            ((get-buffer-window fish)
             (select-window (get-buffer-window fish)))
            (t
             (switch-to-buffer fish)))
    (ansi-term "/usr/local/bin/fish")))

(map! :leader
      :desc "Fish term" "§" #'bilus-fish)

;;
;; Typescript
;;

;; Auto-format using tide.
(add-hook 'before-save-hook 'tide-format-before-save)


;;
;; Experimental
;;
