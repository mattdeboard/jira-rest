;;; jira-rest.el -- Interact with JIRA REST API.

;; Copyright (C) 2012 Matt DeBoard

;; This work is incorporates many concepts & code from, and is heavily
;; influenced by jira.el, the work of Brian Zwahr and Dave Benjamin:
;; http://emacswiki.org/emacs/JiraMode

;;; Code:
(require 'cl)
(require 'json)

;; ********************************
;; JIRA REST Mode - By Matt DeBoard
;; ********************************


(defgroup jira-rest nil
  "JIRA customization group."
  :group 'applications)

(defgroup jira-rest-faces nil
  "Faces for displaying JIRA information."
  :group 'jira)

(defcustom jira-rest-endpoint ""
  "The URL of the REST API endpoint for user's JIRA
 installation."
  :group 'jira-rest
  :type 'string
  :initialize 'custom-initialize-set)

(defface jira-rest-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'jira-rest-faces)

(defface jira-rest-issue-info-header-face
  '((t (:bold t :inherit 'jira-rest-issue-info-face)))
  "Base face for issue headers."
  :group 'jira-rest-faces)

(defface jira-rest-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'jira-rest-faces)

(defface jira-rest-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'jira-rest-faces)

(defface jira-rest-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'jira-rest-faces)

(defface jira-rest-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'jira-rest-faces)

(defface jira-rest-link-project-face
  '((t (:underline t)))
  "Face for linked projects"
  :group 'jira-rest-faces)

(defface jira-rest-link-filter-face
  '((t (:underline t)))
  "Face for linked filters"
  :group 'jira-rest-faces)

(defvar jira-rest-mode-hook nil)

(defvar jira-rest-mode-map nil)
  
(if jira-rest-mode-map
    nil
  (progn
    (setq jira-rest-mode-map (make-sparse-keymap))
    (define-key jira-rest-mode-map "li" 'jira-rest-list-issues)
    (define-key jira-rest-mode-map "lp" 'jira-rest-list-projects)
    (define-key jira-rest-mode-map "lf" 'jira-rest-list-filters)
    (define-key jira-rest-mode-map "si" 'jira-rest-search-issues)
    (define-key jira-rest-mode-map "sp" 'jira-rest-search-project-issues)
    (define-key jira-rest-mode-map "i" 'jira-rest-show-issue)
    (define-key jira-rest-mode-map "c" 'jira-rest-create-ticket)
    (define-key jira-rest-mode-map "o" 'jira-rest-comment-ticket)
    (define-key jira-rest-mode-map "r" 'jira-rest-refresh-ticket)
    (define-key jira-rest-mode-map "a" 'jira-rest-assign-ticket)
    (define-key jira-rest-mode-map "n" 'jira-rest-next-comment)
    (define-key jira-rest-mode-map "p" 'jira-rest-previous-comment)
    (define-key jira-rest-mode-map "jl" 'jira-rest-login)
    (define-key jira-rest-mode-map "jL" 'jira-rest-logout)
    (define-key jira-rest-mode-map "Q" 'jira-rest-mode-quit)
    (define-key jira-rest-mode-map [return] 'jira-rest-return)))

(defun jira-rest-mode ()
  "A mode for working with JIRA's JSON REST API. The full
specification for the API can be found at URL
https://developer.atlassian.com/display/JIRADEV/JIRA+REST+APIs

Requires JIRA 5.0 or greater.

\\{jira-rest-mode-map}"
  (interactive)
  (if (or (equal jira-rest-endpoint nil)
          (equal jira-rest-endpoint ""))
      (message "jira-rest-endpoint not set! Please use 'M-x\
 customize-variable RET jira-rest-endpoint RET'!")
    (progn
      (switch-to-buffer "*JIRA-REST*")
      (kill-all-local-variables)
      (setq major-mode 'jira-rest-mode)
      (setq mode-name "JIRA-REST")
      (use-local-map jira-rest-mode-map)
      (run-hooks 'jira-rest-mode-hook)
      (jira-rest-store-projects)
      (jira-rest-store-priorities)
      (jira-rest-store-statuses)
      (jira-rest-store-types)
      (insert "Welcome to jira-rest-mode!")
      (message "jira rest mode loaded!"))))

(defvar jira-rest-current-issue nil
  "This holds the currently selected issue.")

(defvar jira-rest-projects-list nil
  "This holds a list of projects and their details.")

(defvar jira-rest-types nil
  "This holds a list of issues types.")

(defvar jira-rest-statuses nil
  "This holds a list of statuses.")

(defvar jira-rest-priorities nil
  "This holds a list of priorities.")

(defvar jira-rest-user-fullnames nil
  "This holds a list of user fullnames.")

(defun jira-rest-mode-quit ()
  (interactive)
  (jira-rest-logout)
  (kill-buffer "*JIRA-REST*"))
(json-encode '(:1 2))

(defun jira-rest-create-ticket (project summary description issuetype)
  "File a new ticket with JIRA."
  (interactive (list (read-string "Project Key: ")
                     (read-string "Summary: ")
                     (read-string "Description: ")
                     (read-string "Issue Type: ")))
  (if (or (equal project "")
          (equal summary "")
          (equal description "")
          (equal issuetype ""))
      (message "Must provide all information!")
    ;; Create the JSON string that will be passed to create the ticket.
    (progn
      (setq ticket-alist (cons :fields
                               (list (cons :project (cons :key project))
                                     (cons :summary summary)
                                     (cons :description description)
                                     (cons :issuetype
                                           (cond
                                            ((equal 0 (string-to-number
                                                       issuetype))
                                             (cons :id issuetype))
                                            (t (cons :name issuetype))))))))))
  
