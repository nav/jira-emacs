;;; tools/jira/lisp/auth.el -*- lexical-binding: t; -*-

(require '+jira-core)

(after! auth-source
  (setq auth-sources '("~/.authinfo")))

(defun +jira--get-credentials ()
  "Get Jira credentials from auth-source."

  (+jira--validate-host)

  (let ((domain (+jira--get-host-domain)))
    (+jira--log "Retrieving credentials for host: %s" domain)
    (condition-case err
        (if-let ((auth (car (auth-source-search
                             :host domain
                             :require '(:user :secret)))))
            (let ((user (plist-get auth :user))
                  (token (if (functionp (plist-get auth :secret))
                             (funcall (plist-get auth :secret))
                           (plist-get auth :secret))))
              (if (and user token)
                  (progn
                    (+jira--log "Found credentials for user: %s" user)
                    (list :email user :token token))
                (user-error "Missing or invalid credentials in .authinfo")))
          (user-error "No credentials found in .authinfo for %s" domain))
      (error
       (user-error "Failed to retrieve credentials: %s" (error-message-string err))))))


(defun +jira--make-auth-header ()
  "Create the basic auth header for Jira API requests."
  (+jira--log "Creating authentication header")
  (if-let* ((creds (+jira--get-credentials))
            (email (plist-get creds :email))
            (token (plist-get creds :token)))
      (progn
        (+jira--log "Created auth header for user: %s" email)
        (concat "Basic "
                (base64-encode-string (concat email ":" token) t)))
    (user-error "Failed to create authentication header")))

(provide '+jira-auth)
