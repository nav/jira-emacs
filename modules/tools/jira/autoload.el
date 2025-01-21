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
