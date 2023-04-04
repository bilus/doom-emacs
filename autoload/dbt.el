;;; ~/.doom.d/autoload/bigquery.el -*- lexical-binding: t; -*-
;;;
;;; TODO: Minor dbt-mode modifying bindings when .sql inside dbt project.
;;; TODO: Move bq query to bigquery-mode.


;;; Formatting of tables in dbt-unit-testing test cases
;;;
(defun bilus-format-table ()
  (interactive)
  (let ((start (bilus--find-indentation-start))
            (end (bilus--find-indentation-end)))
        (bilus--dbt-to-org-table start end)
        (org-table-align)
        (let ((start (bilus--find-indentation-start))
              (end (bilus--find-indentation-end)))
          (bilus--org-table-to-dbt start end))))

(defun bilus--dbt-to-org-table (start end)
  (replace-regexp-in-region "^\\([[:space:]]+\\)" "\\1| " start end))

(defun bilus--org-table-to-dbt (start end)
  (replace-regexp-in-region "^\\([[:space:]]+\\)| " "\\1" start end)
  (replace-regexp-in-region "|$" "" start end))

(defun bilus--find-indentation-end ()
  (save-excursion
    (let ((start-indent (current-indentation)))
      (while (= (current-indentation) start-indent)
        (forward-line 1)))
    (point)))

(defun bilus--find-indentation-start ()
  (save-excursion
    (let ((start-indent (current-indentation)))
      (while (= (current-indentation) start-indent)
        (forward-line -1)))
    (forward-line 1)
    (point)))


;; Running dbt

(defun dbt--yaml-read-from-file (path)
  (let ((json-object-type 'hash-table))
    ;; TODO Drop yq dep, it's silly.
    (json-read-from-string (shell-command-to-string (concat "yq r - -j <" path)))))

(defun dbt--escape-quotes (str-val)
  "Return STR-VAL with every double-quote escaped with backslash."
  (save-match-data
    (replace-regexp-in-string "'" "\\\\'" str-val)))

(defun dbt--escape-percent (str-val)
  (save-match-data
    (replace-regexp-in-string "%" "%%" str-val)))

(defun dbt--escape (str-val)
  (dbt--escape-percent (dbt--escape-quotes str-val)))

(defun with-sentinel-message (fun msg &rest args)
  "Inhibit messages in all sentinels started by fun."
  (cl-letf* ((old-set-process-sentinel (symbol-function 'set-process-sentinel))
         ((symbol-function 'set-process-sentinel)
          (lambda (process sentinel)
        (funcall
         old-set-process-sentinel
         process
         `(lambda (&rest args)
            (message (quote ,msg))
            (cl-letf (((symbol-function 'message) #'ignore))
              (apply (quote ,sentinel) args)))))))
        (message "")
        (apply fun args)))

;; Simplified version of save-some-buffers, acting on a list of buffers.
(defun dbt--save-some-buffers (buffers)
  ;; code based on extract from files.el in XEmacs 21.4.14
  (map-y-or-n-p
   (lambda (buffer)
     (if
	 (and (buffer-modified-p buffer)
	      (not (buffer-base-buffer buffer))
	      (buffer-file-name buffer))
	 ;; we deliberately don't switch to show the buffer;
	 ;; let's assume user can see it or knows what's in it.
	 (format "Save file %s? "
		 (buffer-file-name buffer))))
   (lambda (buffer)
     (set-buffer buffer)
     (condition-case ()
	 (save-buffer)
       (error nil)))
   buffers))

;; See: https://emacs.stackexchange.com/questions/47142/multiple-async-shell-command-commands-in-sequence
(defun dbt--run-commands (&rest cmds)
  (let ((out-buf (dbt--compile-out-buf))
        (root-dir (dbt--project-dir)))
    (dbt--save-some-buffers (list (current-buffer)))
    (with-current-buffer out-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (newline)
        (setq default-directory root-dir)
        ;; (with-sentinel-message #'async-shell-command "Finished." (string-join cmds " && ") out-buf)
        (compilation-start (string-join cmds " && ") 'dbt-compilation-mode (lambda (m) (buffer-name)))
        ;; (setq buffer-read-only t)  ;; async-shell-command seems to reset it
        (pop-to-buffer out-buf)))))

(defun dbt--bq-query (sql opts)
  (let (escaped-sql (dbt--escape sql))
    (message escaped-sql)
    (concat "bq query --use_legacy_sql=false --max_rows=50 " opts " '" (dbt--escape sql) "' || true")))

(defun dbt--project-yml-path ()
  (let* ((path (buffer-file-name))
         (project-yml "dbt_project.yml")
         (project-dir (and path (locate-dominating-file path project-yml))))
    (and project-dir
         (concat project-dir project-yml))))

(defun dbt--project-dir ()
  (file-name-directory (dbt--project-yml-path)))

(defun dbt--project-get (key)
  (gethash key (dbt--yaml-read-from-file (dbt--project-yml-path))))

(defun dbt--rel-sql-path* (dir)
  (let* ((path (buffer-file-name))
         (project-name (dbt--project-get "name")))
    (when (string-match (concat "/" dir) path)
      (file-relative-name path (concat (dbt--project-dir) dir)))))

(defun dbt--rel-sql-path ()
  (or (dbt--rel-sql-path* "analysis/") (dbt--rel-sql-path* "models/")))

(defun dbt--is-dbt-project ()
  (not (string= "" (dbt--project-yml-path))))

(defun dbt--compiled-sql-path ()
  (let ((project-name (dbt--project-get "name")))
    (concat (dbt--project-dir) "target/compiled/" project-name "/" (dbt--rel-sql-path))))

(defun dbt--current-model ()
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

;;;### autoload
(defun dbt-show-compiled-sql ()
  (interactive)
  (pop-to-buffer (find-file-noselect (dbt--compiled-sql-path) nil nil nil))
  (bigquery-mode))

(defun dbt--compile-out-buf ()
  (let ((buf (get-buffer-create "** DBT **")))
    ;; (with-current-buffer buf
    ;;   (setq buffer-read-only t)
    buf))

(defun dbt--clear-compile-out-buf ()
  (with-current-buffer (dbt--compile-out-buf)
    (erase-buffer)))


;;;###autoload
(defun bigquery-eval-sql ()
  (interactive)
  (dbt--run-commands (dbt--bq-query (buffer-string) "")))

;;;###autoload
(defun dbt-pop-compile-output ()
  (interactive)
  (pop-to-buffer (dbt--compile-out-buf)))

;;;###autoload
(defun dbt-compile ()
  (interactive)
  (dbt--run-commands (concat "dbt compile --models " (dbt--current-model))))

;;;###autoload
(defun dbt-run ()
  (interactive)
  (dbt--run-commands (concat "dbt run --models " (dbt--current-model))))

;;;###autoload
(defun dbt-run-full-refresh ()
  (interactive)
  (dbt--run-commands (concat "dbt run --models " (dbt--current-model) " --full-refresh")))

;;;###autoload
(defun dbt-eval-sql ()
  (interactive)
  (let* ((path (dbt--compiled-sql-path))
         (buf (find-file-noselect path nil nil nil))
         (sql (with-current-buffer buf
                (buffer-string))))
    (dbt--run-commands
     (concat "dbt compile --models " (dbt--current-model) " --full-refresh") ;; full refresh to remove incremental conditional parts
     (dbt--bq-query sql ""))))

;;;###autoload
(define-derived-mode dbt-mode sql-mode "dbt")

;; (defun dbt-mode-hook ()
;;   (message "dbt-mode-hook!")
;;   (when (and
;;          (not (eq major-mode 'dbt-mode))
;;          (dbt--is-dbt-project))
;;     (dbt-mode)))

;; (add-hook 'sql-mode-hook 'dbt-mode-hook)


;; TODO: Move to config
(map! :localleader
      :map dbt-mode-map
      :desc "bigquery-eval-sql" "x" #'bigquery-eval-sql)
(map! :localleader
      :map dbt-mode-map
      :desc "dbt-compile" "c" #'dbt-compile)
(map! :localleader
      :map dbt-mode-map
      :desc "dbt-run" "r" #'dbt-run)
(map! :localleader
      :map dbt-mode-map
      :desc "dbt-run-full-refresh" "R" #'dbt-run)
(map! :localleader
      :map dbt-mode-map
      :desc "dbt-eval-sql" "e" #'dbt-eval-sql)

(defun dbt--compilation-filter ()
  "Filter function for compilation output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode dbt-compilation-mode "dbt"
  "dbt compilation mode."
  (progn
    (add-hook 'compilation-filter-hook 'dbt--compilation-filter nil t)
    ))

;; (add-hook 'dbt-compilation-mode-hook
          ;; (add-to-list 'compilation-error-regexp-alist '("Database Error.*\\n.*at \\[([^:]+):.*\\n\\s+compiled SQL at\\s(.+)\\n" 1 2)))


;; (with-current-buffer (dbt--compile-out-buf)
;;   (re-search-forward "Database Error.*\n.*at \\[([^:]+):.*\n\\s+compiled SQL at\\s(.+)\n"))

;; (with-current-buffer (dbt--compile-out-buf)
;;   (pop-to-buffer (current-buffer))
;;   (goto-char (point-min))
;;   (re-search-forward "Database Error.*at")
;;   ;; (re-search-forward "Database Error.*\\n.*at \\[([^:]+):.*\\n\\s+compiled SQL at\\s(.+)\\n")
;;   ;; (re-search-forward "Database Error.*\\n")

;;   )
