;;; ~/.doom.d/autoload/bigquery.el -*- lexical-binding: t; -*-

;; (defun dbt--root-dir ()
;;   (and (buffer-file-name)
;;        (or (and (fboundp 'magit-get-top-dir) (magit-get-top-dir))
;;            (and (fboundp 'vc-root-dir) (vc-root-dir))
;;            (locate-dominating-file (buffer-file-name) ".git")
;;            (locate-dominating-file (buffer-file-name) dir-locals-file))))


(defun dbt--project-yml-path ()
  (let* ((path (buffer-file-name))
         (project-yml "dbt_project.yml")
         (project-dir (locate-dominating-file path project-yml)))
    (concat project-dir project-yml)))

(defun dbt--project-dir ()
  (file-name-directory (dbt--project-yml)))

(defun dbt--project-get (key)
  (gethash key (bilus-yaml-read-from-file (dbt--project-yml))))

(defun dbt--rel-sql-path* (dir)
  (let* ((path (buffer-file-name))
         (project-name (dbt--project-get "name")))
    (when (locate-dominating-file path (concat project-name "/" dir))
        (file-relative-name path (concat (dbt--project-dir) dir)))))

(defun dbt--rel-sql-path ()
  (dbt--rel-sql-path* "models/"))

(defun dbt--compiled-sql-path ()
  (let* ((path (buffer-file-name))
         (project-name (dbt--project-get "name")))
    (concat (dbt--project-dir) "target/compiled/" project-name "/" (dbt--rel-sql-path))))

(with-current-buffer (find-file-noselect "/Users/martinb/dev/Tooploox/DTS/conrad-analytics/data/analytics/models/genres.sql")
  (dbt--compiled-sql-path))

;;;###autoload
(defun dbt-compile-sql ()
  (interactive)
  (let* ((path (buffer-file-name))
        (model (file-name-sans-extension (file-name-nondirectory path)))
        (cmd (concat "dbt compile --models " model))
        (out-buf (get-buffer-create "** DBT **"))
        (out-path (dbt--compiled-sql-path)))
    (with-current-buffer out-buf
      (erase-buffer)
      (insert "$ ")
      (insert cmd)
      (newline))
    (call-process-shell-command cmd nil out-buf)
    (pop-to-buffer out-buf nil nil)
    (pop-to-buffer (find-file-noselect out-path nil nil nil))
    (bigquery-mode)))

(defun bilus-yaml-read-from-file (path)
  (let ((json-object-type 'hash-table))
    (json-read-from-string (shell-command-to-string (concat "yq r - -j <" path)))))


(dbt-compile-sql)
(dbt--detect-project-yml "/Users/martinb/dev/Tooploox/DTS/conrad-analytics/data/analytics/target/")

(let ((path (dbt--detect-project-yml "/Users/martinb/dev/Tooploox/DTS/conrad-analytics/data/analytics/target/")))
  (yaml-read-from-file path))


(let ((path "/Users/martinb/dev/Tooploox/DTS/conrad-analytics/data/analytics/target/something.sql"))
   path)

(file-relative-name "/Users/martinb/dev/Tooploox/DTS/conrad-analytics/data/analytics/target/something.sql" "/Users/martinb/dev/Tooploox/DTS/conrad-analytics/data/analytics/")
