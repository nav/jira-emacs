;;; tools/jira/lisp/core.el -*- lexical-binding: t; -*-

(defgroup +jira nil
  "A Jira API client for Doom Emacs."
  :group 'tools
  :prefix "+jira-")

(defcustom +jira-host nil
  "Jira host URL."
  :type 'string
  :group '+jira)

(defcustom +jira-debug nil
  "Enable debug logging for Jira operations."
  :type 'boolean
  :group '+jira)

;; Add the cache variable here
(defvar +jira--search-cache (make-hash-table :test 'equal)
  "Cache for Jira search results.")

(defvar +jira--custom-fields-cache nil
  "Cache for Jira custom fields mapping.")

(defun +jira--log (format-string &rest args)
  "Log debug messages when +jira-debug is enabled."
  (when +jira-debug
    (apply #'message (concat "[JIRA] " format-string) args)))

(defun +jira--get-host-domain ()
  "Extract domain from Jira host URL."
  (when-let* ((host +jira-host)
              (url (url-generic-parse-url host))
              (domain (url-host url)))
    domain))

(defun +jira--validate-host ()
  "Validate Jira host configuration."
  (unless +jira-host
    (user-error "Jira host is not configured. Set +jira-host in your config"))
  (unless (string-match-p "^https?://" +jira-host)
    (user-error "Invalid Jira host URL. Must start with http:// or https://"))
  (setq +jira-host (replace-regexp-in-string "/$" "" +jira-host))
  t)

(defun +jira--fetch-custom-fields ()
  "Fetch custom fields from Jira API and store them in cache."
  (unless +jira--custom-fields-cache
    (+jira--log "Fetching custom fields")
    (let ((url (concat +jira-host "/rest/api/3/field"))
          (auth-header (+jira--make-auth-header)))
      (request url
        :type "GET"
        :headers `(("Authorization" . ,auth-header)
                   ("Content-Type" . "application/json"))
        :parser 'json-read
        :sync t  ; Synchronous request to ensure we have fields
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (+jira--log "Successfully fetched custom fields")
                    (setq +jira--custom-fields-cache
                          (cl-loop for field across data
                                   when (eq t (alist-get 'custom field))
                                   collect (cons (alist-get 'id field)
                                                 (alist-get 'name field))))))
        :error (cl-function
                (lambda (&key response &allow-other-keys)
                  (+jira--handle-error response)))))))

(defun +jira--get-custom-field-name (field-id)
  "Get custom field name from FIELD-ID."
  (+jira--fetch-custom-fields)
  (or (cdr (assoc field-id +jira--custom-fields-cache))
      field-id))

(defun +jira--get-custom-field-id (field-name)
  "Get custom field ID from FIELD-NAME."
  (+jira--fetch-custom-fields)
  (car (rassoc field-name +jira--custom-fields-cache)))


(defun +jira--quit-and-kill-buffer ()
  "Kill the current buffer and delete its window."
  (interactive)
  (let ((buf (current-buffer)))
    (if (> (length (window-list)) 1)
        (quit-window t)  ; Kill buffer and window if there are multiple windows
      (kill-buffer))))  ; Just kill buffer if it's the only window


(provide '+jira-core)
