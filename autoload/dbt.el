;;; ~/.doom.d/autoload/bigquery.el -*- lexical-binding: t; -*-
;;;
;;; TODO: Minor dbt-mode modifying bindings when .sql inside dbt project.
;;; TODO: Move bq query to bigquery-mode.


(defun dbt--yaml-read-from-file (path)
  (let ((json-object-type 'hash-table))
    ;; TODO Drop yq dep, it's silly.
    (json-read-from-string (shell-command-to-string (concat "yq r - -j <" path)))))

(defun dbt--escape-quotes (str-val)
  "Return STR-VAL with every double-quote escaped with backslash."
  (save-match-data
    (replace-regexp-in-string "'" "\\\\'" str-val)))

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
  (let ((out-buf (dbt--compile-out-buf)))
    (dbt--save-some-buffers (list (current-buffer)))
    (with-current-buffer out-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (newline)
        ;; (with-sentinel-message #'async-shell-command "Finished." (string-join cmds " && ") out-buf)
        (compilation-start test-command-to-run 'mocha-compilation-mode (lambda (m) (buffer-name)))
        (setq buffer-read-only t)  ;; async-shell-command seems to reset it
        (pop-to-buffer out-buf)))))

(defun dbt--bq-query (sql opts)
  (concat "bq query --use_legacy_sql=false --max_rows=50 " opts " '" (dbt--escape-quotes sql) "' || true"))

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
    (with-current-buffer buf
      (setq buffer-read-only t))
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

;; (with-current-buffer (find-file-noselect "/Users/martinb/dev/Tooploox/DTS/conrad-analytics/data/analytics/models/genres.sql")
;;   (dbt-eval-sql))
