;;; tools/jira/lisp/ui.el -*- lexical-binding: t; -*-

(require '+jira-core)
(require '+jira-formatters)

(defun +jira--display-result (data)
  "Display the API response DATA in a buffer."
  (+jira--log "Displaying result, data type: %s" (type-of data))
  (let* ((buf (cond
               ;; Handle issue list from search
               ((and (alist-get 'issues data)
                     (alist-get 'total data))
                (+jira--format-issue-list data))
               ;; Use existing display logic for other cases
               (t
                (let ((buf (get-buffer-create "*Jira Result*")))
                  (with-current-buffer buf
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert
                       (cond
                        ((and (alist-get 'displayName data)
                              (alist-get 'emailAddress data))
                         (+jira--format-user-info data))
                        ((and (alist-get 'key data)
                              (alist-get 'fields data))
                         (+jira--format-issue data))
                        (t (pp-to-string data))))

                      ;; Set up org-mode and view-mode
                      (org-mode)
                      (view-mode -1)  ; Disable view-mode temporarily

                      ;; Create a new keymap that inherits from org-mode-map
                      (let ((map (make-composed-keymap
                                  (make-sparse-keymap)
                                  org-mode-map)))
                        (define-key map (kbd "q") #'+jira--quit-and-kill-buffer)
                        (use-local-map map))

                      ;; Re-enable view-mode
                      (view-mode +1)
                      (goto-char (point-min))))
                  buf))))
         (display-buffer-alist
          `(("\\*Sprint.*\\*"
             (display-buffer-below-selected)
             (window-height . 0.3)
             (preserve-size . (nil . t))))))
    ;; Display buffer using our rules
    (select-window (display-buffer buf))))

(provide '+jira-ui)

;;; ui.el ends here
