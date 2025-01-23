;;; tools/jira/config.el -*- lexical-binding: t; -*-

(load! "lisp/core")
(load! "lisp/auth")
(load! "lisp/formatters")  ;; Load formatters before ui
(load! "lisp/api")
(load! "lisp/ui")

;; Keybindings
(map! :leader
      (:prefix-map ("j" . "jira")
       :desc "Get issue" "i" #'+jira/issue
       :desc "Open current issue" "o" #'+jira/open-issue-from-branch
       :desc "Sprint Issues" "s" #'+jira/sprint-issues
       :desc "Search Issues" "f" #'+jira/search-issues
       :desc "Toggle debug" "d" #'+jira/toggle-debug))
