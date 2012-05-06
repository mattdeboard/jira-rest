;;; jira-rest.el -- Interact with JIRA REST API.

;; Copyright (C) 2012 Matt DeBoard

;; This work is incorporates many concepts & code from, and is heavily
;; influenced by jira.el, the work of Brian Zwahr and Dave Benjamin:
;; http://emacswiki.org/emacs/JiraMode

;; Documentation of JIRA REST API can be found at URL:
;; https://developer.atlassian.com/display/JIRADEV/JIRA+REST+APIs

;;; Code:
(require 'cl)
(require 'json)
(require 'url)

;; ********************************
;; JIRA REST Mode - By Matt DeBoard
;; ********************************

(defgroup jira-rest nil
  "JIRA customization group."
  :group 'applications)

(defgroup jira-rest-faces nil
  "Faces for displaying JIRA information."
  :group 'jira)

(defvar jira-rest-auth-info nil
  "The auth header used to authenticate each request. Please
see URL https://developer.atlassian.com/display/JIRADEV/JIRA+REST+API+Example+-+Basic+AuthenticationConsists for more information.")

(defun load-auth-info ()
  (let ((jira-pwd-file (expand-file-name "~/.jira-auth-info.el")))
    (when (file-regular-p jira-pwd-file)
      (load jira-pwd-file))))
    
(defun jira-rest-login ()
  (progn
    (load-auth-info)
    (let ((enc (base64-encode-string (concat jira-username ":" jira-password))))
      (setq jira-rest-auth-info (concat "Basic " enc)))
    nil))

(defun jira-rest-logout ()
  "Logs the user out of JIRA."
  (interactive)
  (setq jira-rest-auth-info nil))

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
      ;; (jira-rest-store-projects)
      ;; (jira-rest-store-priorities)
      ;; (jira-rest-store-statuses)
      ;; (jira-rest-store-types)
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

(defun url-post (data)
  (if (not jira-rest-auth-info)
      (message "You must login first, 'M-x jira-rest-login'.")
    (let ((url-request-method "POST")
          (url-request-extra-headers
           `(("Content-Type" . "application/json")
             ("Authorization" . ,jira-rest-auth-info)))
          (url-request-data data))
      (url-retrieve jira-rest-endpoint 'my-switch-to-url-buffer))))

(defun my-switch-to-url-buffer (status)
  (switch-to-buffer (current-buffer)))

(defun jira-rest-mode-quit ()
  (interactive)
  (jira-rest-logout)
  (kill-buffer "*JIRA-REST*"))

(defun id-or (s)
  "Return ':id' if 's' is a numeric string. Otherwise, return
nil. The idea here is that the JIRA REST API spec allows the 'project'
and 'issuetype' keys to be either 'id' or some other value (in the
case of 'project', the other is 'key'; for 'issuetype', 'name'). This fn
enables us to allow either type of user input."
  (if (not (equal 0 (string-to-number s)))
      "id"))

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
    (let ((field-hash (make-hash-table :test 'equal))
          (issue-hash (make-hash-table :test 'equal))
          (project-hash (make-hash-table :test 'equal))
          (issuetype-hash (make-hash-table :test 'equal)))
      ;; Create the JSON string that will be passed to create the ticket.
      (progn
        ;; Populate our hashes, from bottom to top. The format for these
        ;; nested hash tables follow the format outlined in the JIRA REST
        ;; API documentation.
        (puthash (or (id-or project) "key") project project-hash)
        (puthash (or (id-or issuetype) "name") issuetype issuetype-hash)
        (puthash "project" project-hash issue-hash)
        (puthash "issuetype" issuetype-hash issue-hash)
        (puthash "summary" summary issue-hash)
        (puthash "description" description issue-hash)
        (puthash "fields" issue-hash field-hash)
        ;; Return the JSON-encoded hash map.
        (url-post (json-encode field-hash))))))

