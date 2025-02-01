;;; tools/jira/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jira/toggle-debug ()
  "Toggle Jira debug logging."
  (interactive)
  (setq +jira-debug (not +jira-debug))
  (message "Jira debug logging %s" (if +jira-debug "enabled" "disabled")))

;;;###autoload
(defun +jira/issue (issue-key)
  "Get information about a specific Jira issue.
ISSUE-KEY is the Jira issue key (e.g., PROJ-123)"
  (interactive "sEnter Issue Key: ")
  (+jira--log "Fetching issue: %s" issue-key)
  (if (string-match-p "^[A-Z]+-[0-9]+$" issue-key)
      (+jira--make-request
       (format "/issue/%s?expand=renderedFields,names,schema,transitions,editmeta,changelog,comments"
               issue-key)
       "GET")
    (user-error "Invalid issue key format. Should be like PROJ-123")))

;;;###autoload
(defun +jira/open-issue-from-branch ()
  "Extract Jira issue number from current git branch and open it."
  (interactive)
  (when-let* ((branch (magit-get-current-branch))
              (issue-key (and branch
                              (string-match "\\(IX-[0-9]+\\)" branch)
                              (match-string 1 branch))))
    (+jira/issue issue-key)))

;;;###autoload
(defun +jira/sprint-issues ()
  "Get issues assigned to me in the current sprint."
  (interactive)
  (+jira--log "Fetching my sprint issues")
  (let* ((points-field-id (+jira--get-custom-field-id "Story Points"))
         (jql "assignee = currentUser() AND sprint in openSprints() ORDER BY priority DESC, created"))
    (+jira--make-request
     (format "/search?jql=%s&fields=summary,status,priority,issuetype,%s"
             (url-encode-url jql)
             points-field-id)  ; Include the points field ID in the request
     "GET")))

;;;###autoload
(defun +jira/search-issues ()
  "Search for Jira issues."
  (interactive)
  (let* ((query (read-string "Search Jira: "))
         (jql (format "text ~ \"%s\" ORDER BY updated DESC"
                      (replace-regexp-in-string "\"" "\\\"" query))))
    (+jira--search-request jql)))

;; Branching
(defun +jira--find-existing-branch (issue-key)
  "Find existing branch containing ISSUE-KEY."
  (let ((branches (split-string (shell-command-to-string "git branch -a") "\n" t)))
    (seq-find (lambda (branch)
                (string-match-p issue-key (string-trim branch)))
              branches)))

(defun +jira--create-branch-name (issue-key issue-type summary)
  "Create a branch name from ISSUE-KEY, ISSUE-TYPE, and SUMMARY."
  (let* ((prefix (if (string= issue-type "Bug") "fix" "feat"))
         (cleaned-summary (replace-regexp-in-string "[^a-zA-Z0-9]+" "-"
                                                    (downcase (string-trim summary))))
         (branch-name (format "%s/%s-%s" prefix issue-key cleaned-summary)))
    ;; Truncate to 50 characters if needed
    (if (> (length branch-name) 50)
        (substring branch-name 0 50)
      branch-name)))

(defun +jira/create-branch-from-issue ()
  "Create a git branch from the current issue or switch to it if it exists."
  (interactive)
  (when-let* ((issue-key (tabulated-list-get-id))
              (issue-data (car (seq-filter
                                (lambda (entry)
                                  (string= (car entry) issue-key))
                                tabulated-list-entries)))
              (entry-vector (cadr issue-data))
              (issue-type (aref entry-vector 1))
              (summary (aref entry-vector 5)))
    (if-let ((existing-branch (+jira--find-existing-branch issue-key)))
        (let ((branch-name (string-trim existing-branch "^[* ]*" "[ \r\n]*$")))
          (message "Switching to existing branch: %s" branch-name)
          (shell-command (format "git checkout %s" branch-name)))
      (let ((new-branch (+jira--create-branch-name issue-key issue-type summary)))
        (message "Creating new branch: %s" new-branch)
        (create-branch-from-remote "dev" new-branch)))))
