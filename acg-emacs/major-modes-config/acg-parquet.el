


(defun parquet-tools-show-file (&optional filepath)
  (interactive)
  (let* ((filepath (or filepath (buffer-file-name (current-buffer))))
         (filepath (expand-file-name filepath))
         (filename (file-name-nondirectory filepath))
         (output-buffer-name (concat filename ".txt"))
         (output-buffer (generate-new-buffer output-buffer-name)))
    (call-process "parquet-tools" nil output-buffer nil "show" filepath)
    (switch-to-buffer output-buffer)))
;; (parquet-tools-show-file "~/Projects/fit-file-catalog/blob-data-unzipped-output/userId=4f42c1ee-193d-4437-a82f-2e1f9543a885/history/2021-06-02_084729_garmin/")
