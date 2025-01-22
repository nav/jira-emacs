;;; tools/jira/lisp/formatters.el -*- lexical-binding: t; -*-

(require 'org)
(require '+jira-core)


(defun +jira--format-adf-content (content &optional indent-level)
  "Convert Jira ADF (Atlassian Document Format) CONTENT to plain text.
INDENT-LEVEL is used for nested lists indentation."
  (let ((indent-level (or indent-level 0)))
    (cond
     ;; Handle null or empty content
     ((or (null content) (seq-empty-p content))
      "")

     ;; Handle array of content
     ((arrayp content)
      (mapconcat (lambda (item) (+jira--format-adf-content item indent-level))
                 content
                 ""))

     ;; Handle content nodes
     ((and (listp content) (alist-get 'type content))
      (pcase (alist-get 'type content)
        ;; Text node
        ("text" (or (alist-get 'text content) ""))

        ;; Mention node
        ("mention"
         (let* ((attrs (alist-get 'attrs content))
                (id (alist-get 'id attrs))
                (text (or (alist-get 'text content)
                          (alist-get 'displayName attrs)
                          (alist-get 'text attrs)
                          id)))
           (propertize (concat "[" text "]")
                       'face '(:foreground "deep sky blue" :weight bold)
                       'jira-mention-id id)))

        ;; Paragraph
        ("paragraph"
         (let ((parent-type (alist-get 'type (car (alist-get 'parent content)))))
           (if (member parent-type '("listItem" "tableCell"))
               ;; Don't add newline for list items or table cells
               (+jira--format-adf-content (alist-get 'content content) indent-level)
             ;; Add newline for regular paragraphs
             (concat (+jira--format-adf-content (alist-get 'content content) indent-level) "\n"))))

        ;; Bullet list
        ("bulletList"
         (let ((items (append (alist-get 'content content) nil)))
           (concat
            (if (zerop indent-level) "\n" "")
            (mapconcat
             (lambda (item)
               (concat
                (make-string indent-level ?\s) "- "
                (string-trim
                 (+jira--format-adf-content item (+ indent-level 2)))))
             items
             "\n")
            (if (zerop indent-level) "\n" ""))))

        ;; Ordered list
        ("orderedList"
         (let ((items (append (alist-get 'content content) nil))
               (counter 1))
           (concat
            (if (zerop indent-level) "\n" "")
            (mapconcat
             (lambda (item)
               (prog1
                   (concat
                    (make-string indent-level ?\s)
                    (number-to-string counter) ". "
                    (string-trim
                     (+jira--format-adf-content item (+ indent-level 2))))
                 (setq counter (1+ counter))))
             items
             "\n")
            (if (zerop indent-level) "\n" ""))))

        ;; List item
        ("listItem"
         (+jira--format-adf-content (alist-get 'content content) indent-level))

        ;; Headings
        ((or "heading" "h1" "h2" "h3" "h4" "h5" "h6")
         (let ((level (string-to-number (substring (alist-get 'type content) -1))))
           (concat "\n" (make-string level ?*) " "
                   (+jira--format-adf-content (alist-get 'content content) indent-level)
                   "\n")))

        ;; Code block
        ("codeBlock"
         (concat "\n#+BEGIN_SRC "
                 (or (alist-get 'language content) "")
                 "\n"
                 (+jira--format-adf-content (alist-get 'content content) indent-level)
                 "#+END_SRC\n"))

        ;; Inline code
        ("inlineCode"
         (concat "~" (+jira--format-adf-content (alist-get 'content content) indent-level) "~"))

        ;; Blockquote
        ("blockquote"
         (concat "\n#+BEGIN_QUOTE\n"
                 (+jira--format-adf-content (alist-get 'content content) indent-level)
                 "#+END_QUOTE\n"))

        ;; Links
        ("link"
         (let ((href (alist-get 'href (alist-get 'attrs content))))
           (format "[[%s][%s]]"
                   href
                   (+jira--format-adf-content (alist-get 'content content) indent-level))))

        ;; Emphasis (bold, italic, etc.)
        ("strong" (concat "*" (+jira--format-adf-content (alist-get 'content content) indent-level) "*"))
        ("em" (concat "/" (+jira--format-adf-content (alist-get 'content content) indent-level) "/"))
        ("strike" (concat "+" (+jira--format-adf-content (alist-get 'content content) indent-level) "+"))

        ;; Default case for unknown types
        (_ (+jira--format-adf-content (alist-get 'content content) indent-level))))

     ;; Return empty string for unknown content
     (t ""))))


(defun +jira--format-description (description)
  "Format a Jira issue DESCRIPTION from ADF to Org format."
  (if description
      (let ((content (if (stringp description)
                         description
                       (+jira--format-adf-content description))))
        (string-trim content))
    "No description provided."))


(defun +jira--format-user-info (data)
  "Format user information DATA into a readable format."
  (let ((buf (generate-new-buffer "*Jira User Info*")))
    (with-current-buffer buf
      (insert "# Jira User Information\n\n")
      (insert (format "Display Name: %s\n" (alist-get 'displayName data)))
      (insert (format "Email: %s\n" (alist-get 'emailAddress data)))
      (insert (format "Active: %s\n" (if (alist-get 'active data) "Yes" "No")))

      (org-mode)  ; Use org-mode for better formatting
      (goto-char (point-min))
      (buffer-string))))


(defun +jira--format-custom-field-value (value)
  "Format custom field VALUE based on its type."
  (cond
   ;; Handle null value
   ((null value) "")

   ;; Handle array/list values
   ((arrayp value)
    (string-join
     (mapcar #'+jira--format-custom-field-value value)
     ", "))

   ;; Handle objects with name field (like users)
   ((and (listp value) (alist-get 'name value))
    (alist-get 'name value))

   ;; Handle objects with value field (like select fields)
   ((and (listp value) (alist-get 'value value))
    (alist-get 'value value))

   ;; Handle ADF content
   ((and (listp value) (alist-get 'content value))
    (+jira--format-adf-content value))

   ;; Default to string conversion
   (t (format "%s" value))))

(defun +jira--format-comment (comment)
  "Format a single COMMENT into org format."
  (let* ((author (alist-get 'displayName (alist-get 'author comment)))
         (created (alist-get 'created comment))
         (updated (alist-get 'updated comment))
         (body (alist-get 'body comment))
         (is-edited (not (string= created updated))))
    (concat
     (format "\n*** %s (%s%s)\n\n"
             author
             (format-time-string "%Y-%m-%d %H:%M"
                                 (date-to-time created))
             (if is-edited
                 (format " - edited %s"
                         (format-time-string "%Y-%m-%d %H:%M"
                                             (date-to-time updated)))
               ""))
     (+jira--format-description body)
     "\n")))


(defun +jira--format-issue-fields (fields)
  "Format issue FIELDS into a readable format."
  (with-temp-buffer
    (insert (format "* %s\n" (alist-get 'summary fields)))

    ;; Standard fields
    (insert (format "- Type: %s\n" (alist-get 'name (alist-get 'issuetype fields))))
    (insert (format "- Status: %s\n" (alist-get 'name (alist-get 'status fields))))

    ;; Add Points
    (let* ((points-field-id (+jira--get-custom-field-id "Story Points"))
           (points (when points-field-id
                     (alist-get (intern points-field-id) fields))))
      (insert (format "- Points: %s\n"
                      (if points
                          (number-to-string (round (string-to-number (format "%s" points))))
                        "Not estimated"))))

    (insert (format "- Priority: %s\n" (alist-get 'name (alist-get 'priority fields))))
    (insert (format "- Created: %s\n" (alist-get 'created fields)))
    (insert (format "- Updated: %s\n" (alist-get 'updated fields)))

    ;; Assignee and Reporter
    (let ((assignee (alist-get 'assignee fields))
          (reporter (alist-get 'reporter fields)))
      (insert (format "- Assignee: %s\n"
                      (if assignee (alist-get 'displayName assignee) "Unassigned")))
      (insert (format "- Reporter: %s\n"
                      (if reporter (alist-get 'displayName reporter) "Unknown"))))

    ;; Description
    (insert "\n** Description\n")
    (insert (+jira--format-description (alist-get 'description fields)))

    ;; Acceptance Criteria
    (let* ((acceptance-criteria-id (+jira--get-custom-field-id "Acceptance Criteria"))
           (acceptance-criteria (when acceptance-criteria-id
                                  (alist-get (intern acceptance-criteria-id) fields))))
      (when acceptance-criteria
        (insert "\n\n** Acceptance Criteria\n")
        (insert (+jira--format-description acceptance-criteria))))

    ;; Test Criteria
    (let* ((test-criteria-id (+jira--get-custom-field-id "Test Criteria"))
           (test-criteria (when test-criteria-id
                            (alist-get (intern test-criteria-id) fields))))
      (when test-criteria
        (insert "\n\n** Test Criteria\n")
        (insert (+jira--format-description test-criteria))))

    ;; Comments
    (when-let* ((comment-data (alist-get 'comment fields))
                (comments (alist-get 'comments comment-data))
                (total (alist-get 'total comment-data)))
      (insert (format "\n\n** Comments (%d)\n" total))
      (if (> (length comments) 0)
          ;; Sort comments by created date in descending order
          (seq-do (lambda (comment)
                    (insert (+jira--format-comment comment)))
                  (sort (append comments nil) ; Convert vector to list
                        (lambda (a b)
                          (string> (alist-get 'created a)
                                   (alist-get 'created b)))))
        (insert "No comments yet.\n")))

    (buffer-string)))

(defun +jira--format-issue (data)
  "Format issue DATA into a readable format."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Issue Key and URL
      (insert (format "[[%s/browse/%s][%s]] | [[%s][%s]]\n\n"
                      (string-trim-right +jira-host "/")
                      (alist-get 'key data)
                      (alist-get 'key data)
                      (alist-get 'self data)
                      (alist-get 'id data)))

      ;; Format fields
      (insert (+jira--format-issue-fields (alist-get 'fields data)))
      (buffer-string))))


(defun jira--format-key (key)
  "Format issue KEY with bold face and primary color."
  (propertize key 'face `(:inherit bold :foreground ,(doom-color 'highlight))))


(defun jira--get-priority-face (priority)
  "Get face for PRIORITY using Doom theme colors."
  (pcase priority
    ("Highest" `(:foreground ,(doom-color 'error) :weight bold))
    ("High" `(:foreground ,(doom-color 'error)))
    ("Medium" `(:foreground ,(doom-color 'warning)))
    ("Low" `(:foreground ,(doom-color 'success)))
    ("Lowest" `(:foreground ,(doom-color 'grey)))
    (_ nil)))


(defun jira--format-priority (priority)
  "Format PRIORITY with appropriate face using Doom theme colors."
  (if-let ((face (jira--get-priority-face priority)))
      (propertize priority 'face face)
    priority))


(defvar jira-priority-order
  '("Highest" "High" "Medium" "Low" "Lowest")
  "List of priorities in descending order.")

(defun jira-priority-value (priority)
  "Convert priority string to numeric value for sorting."
  (- (length jira-priority-order)
     (or (cl-position priority jira-priority-order :test 'string-equal) 0)))

(define-derived-mode jira-sprint-issues-mode tabulated-list-mode "Jira Sprint Issues"
  "Major mode for displaying sprint issues."
  (setq tabulated-list-format
        [("Key" 12 t)
         ("Type" 10 t)
         ("Priority" 10 (lambda (a b)
                          (> (jira-priority-value (aref (cadr a) 2))
                             (jira-priority-value (aref (cadr b) 2)))))
         ("Points" 8 (lambda (a b)
                       (let ((points-a (string-to-number (or (aref (cadr a) 3) "0")))
                             (points-b (string-to-number (or (aref (cadr b) 3) "0"))))
                         (> points-a points-b))))
         ("Status" 20 t)
         ("Summary" 50 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Priority" nil))
  (tabulated-list-init-header))

(defun +jira--format-issue-list (data)
  "Format a list of issues from search results DATA."
  (let ((buf (get-buffer-create "*Sprint Issues*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Enable the custom mode
        (jira-sprint-issues-mode)

        ;; Create entries for tabulated list
        (setq tabulated-list-entries
              (mapcar
               (lambda (issue)
                 (let* ((key (alist-get 'key issue))
                        (fields (alist-get 'fields issue))
                        (summary (alist-get 'summary fields))
                        (status (alist-get 'name (alist-get 'status fields)))
                        (priority (alist-get 'name (alist-get 'priority fields)))
                        (type (alist-get 'name (alist-get 'issuetype fields)))
                        (points-field-id (+jira--get-custom-field-id "Story Points"))
                        (points (when points-field-id
                                  (alist-get (intern points-field-id) fields)))
                        (points-display (if points
                                            (number-to-string
                                             (round (string-to-number (format "%s" points))))
                                          "-")))
                   (list key  ; Store issue key as ID
                         (vector (jira--format-key key)
                                 type
                                 (jira--format-priority priority)
                                 points-display  ; Add points column
                                 status
                                 summary))))
               (alist-get 'issues data)))

        ;; Add key binding for RET
        (define-key jira-sprint-issues-mode-map (kbd "RET")
                    (lambda ()
                      (interactive)
                      (when-let ((issue-key (tabulated-list-get-id)))
                        (+jira/issue issue-key))))

        ;; Print the table with headers
        (tabulated-list-print t)

        ;; Calculate and add total points
        (let ((total-points (cl-reduce #'+
                                       (mapcar (lambda (entry)
                                                 (string-to-number
                                                  (or (aref (cadr entry) 3) "0")))
                                               tabulated-list-entries))))
          (goto-char (point-max))
          (insert (format "\nTotal Issues: %d   Total Points: %d"
                          (alist-get 'total data)
                          total-points)))


        ;; Set up the quit key binding
        (use-local-map (copy-keymap jira-sprint-issues-mode-map))
        (local-set-key (kbd "q") #'+jira--quit-and-kill-buffer)


        (goto-char (point-min))))
    buf))


(provide '+jira-formatters)
