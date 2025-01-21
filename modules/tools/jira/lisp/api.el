;;; tools/jira/lisp/api.el -*- lexical-binding: t; -*-

(require '+jira-core)
(require '+jira-auth)

(defun +jira--handle-error (response)
  "Handle error responses from Jira API."
  (let* ((status (or (request-response-status-code response) 500))
         (error-data (request-response-data response))
         (error-msg
          (cond
           ((stringp error-data)
            (condition-case nil
                (let ((json-data (json-read-from-string error-data)))
                  (or (cdr (assoc 'message json-data)) "Unknown error"))
              (error error-data)))
           ((null error-data) "No response data")
           (t "Unknown error"))))
    (+jira--log "Error response: Status=%s, Message=%s" status error-msg)
    (pcase status
      (400 (message "Bad Request: %s" error-msg))
      (401 (message "Authentication failed. Please check your credentials"))
      (403 (message "Forbidden: You don't have permission for this action"))
      (404 (message "Not Found: The requested resource doesn't exist"))
      (429 (message "Too Many Requests: API rate limit exceeded"))
      (500 (message "Internal Server Error: %s" error-msg))
      (_ (message "Request failed with status %s: %s" status error-msg)))))


(defun +jira--make-request (endpoint method &optional data)
  "Make a request to the Jira API."

  (+jira--validate-host)

  (let* ((url (concat +jira-host "/rest/api/3" endpoint))
         (auth-header (+jira--make-auth-header))
         (headers `(("Authorization" . ,auth-header)
                    ("Content-Type" . "application/json"))))
    (+jira--log "Making %s request to %s" method url)
    (request url
      :type method
      :headers headers
      :parser 'json-read
      :data (when data (json-encode data))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (+jira--log "Request successful with data type: %s" (type-of data))
                  (+jira--display-result data)))
      :error (cl-function
              (lambda (&key response &allow-other-keys)
                (+jira--handle-error response))))))


(defun +jira--search-request (jql)
  "Make a search request to Jira using JQL."
  (+jira--log "Making search request with JQL: %s" jql)
  (let ((points-field-id (+jira--get-custom-field-id "Story Points")))
    (+jira--make-request
     (format "/search?jql=%s&fields=summary,status,priority,issuetype,%s,labels"
             (url-encode-url jql)
             points-field-id)
     "GET")))


(provide '+jira-api)
