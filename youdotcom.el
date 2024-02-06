;;; youdotcom.el --- You.com search package -*- lexical-binding: t; -*-

;; Author: Samuel Michael Vanié <samuelmichaelvanie@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1
;; Package-requires: ((emacs "25.1"))
;; Keywords: ai, tools
;; URL: https://github.com/SamuelVanie/youdotcom.el

;;; Commentary:

;; This package provides an interactive interface to You.com's search API

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

(defgroup youdotcom nil
  "A package to make quick searches on you.com."
  :group 'applications)

(defcustom youdotcom-search-api-key ""
  "Your secret API key for Search endpoint on api.you.com."
  :type 'string
  :group 'youdotcom)

(defcustom youdotcom-rag-api-key ""
  "Your secret API key for rag endpoint on api.you.com."
  :type 'string
  :group 'youdotcom)

(defun youdotcom-get-api-key (type)
  "Get the API key for the given TYPE."
  (if (string= type "search")
      youdotcom-search-api-key
    youdotcom-rag-api-key))

(defvar youdotcom-buffer-name "*Youdotcom*"
  "The name of the buffer for the Youdotcom session.")

(defcustom youdotcom-number-of-results 1
  "The number of results that the api should return."
  :type 'natnum
  :group 'youdotcom)


(defconst youdotcom-base-api-endpoint "https://api.ydc-index.io/"
  "The base url of the you.com api for the search functionalities.")

(defun youdotcom-build-url (query type)
  "Build a URL for the You.com's API with the given QUERY and TYPE."
  (if (string= type "search")
      (format "%ssearch?query=%s&num_web_results=%d"
              youdotcom-base-api-endpoint
              (url-hexify-string query)
              youdotcom-number-of-results)
    (if (string= type "rag")
    (format "%srag?query=%s"
            youdotcom-base-api-endpoint
            (url-hexify-string query)))))

(defun youdotcom-verify-payload ()
  "Verify that the payload is valid."
  (unless (and (not (string-empty-p youdotcom-search-api-key))
               (not (string-empty-p youdotcom-base-api-endpoint))
               (> youdotcom-number-of-results 0))
    (error "Invalid arguments or global variables")))

(defun youdotcom-send-request (query type callback)
  "Send a request to the You.com's API with the given QUERY and CALLBACK."
  (youdotcom-verify-payload)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-API-Key" . ,(youdotcom-get-api-key type))
           ("Content-Type" . "application/json")))
        (url-request-data nil))
    (url-retrieve (youdotcom-build-url query type)
                  callback (list query type))))

(defun youdotcom-format-message (message)
  "Format a MESSAGE as a string for display."
  (let ((role (cdr (assoc "role" message)))
        (content (cdr (assoc "content" message))))
    (format "%s: %s\n" role content)))

(defun youdotcom-display-messages (messages)
  "Display the MESSAGES in the chat buffer."
  (with-current-buffer (get-buffer-create youdotcom-buffer-name)
    (goto-char (point-max))
    (let ((current-point (point)))
        (insert (youdotcom-format-message (pop messages)))
        (add-face-text-property current-point (point-max) '(:foreground "red")))
    (dolist (message messages)
      (insert (youdotcom-format-message message)))
    (goto-char (point-min))))

(defun youdotcom-parse-response (json type)
  "Parse JSON response from the API for the given TYPE."
  (if (string= type "search")
      (youdotcom-parse-search-response json)
    (if (string= type "rag")
        (youdotcom-parse-rag-response json))))

(defun youdotcom-parse-search-response (json)
  "Parse JSON response if called from the search endpoint."
  (let* ((hits (alist-get 'hits json))
         (response ""))
    (dolist (hit hits)
      (let ((description (alist-get 'description hit))
            (snippets (alist-get 'snippets hit))
            (title (alist-get 'title hit))
            (url (alist-get 'url hit)))
        (setq response (concat response "\n\n# Title: "
                               (format "%s" title) "\n\n"
                               (format "## Description : %s" description)
                               "\n\n" (format "%s" (mapconcat #'identity snippets "\n"))
                               "\n\n" (format "%s" url) "\n"))))
    response))

(defun youdotcom-parse-rag-response (json)
  "Parse JSON response if called from the rag endpoint."
  (let* ((answer (alist-get 'answer json))
         (response ""))
    (setq response answer)
    response))


(defun youdotcom-format-answer (query response)
  "Format user's QUERY and the API's RESPONSE for easy display."
  (youdotcom-display-messages
   `((("role" . "user")
      ("content" . ,query))
     (("role" . "assistant")
      ("content" . ,response)))))

(defun youdotcom-handle-response (status content type)
  "Extract CONTENT from the response and change it to elisp list.STATUS, ignored."
  (ignore status)
  (goto-char (point-min))
  (re-search-forward "^$")
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (json (json-read))
         (response (youdotcom-parse-response json type)))
    ;; REVIEW: This info extractions
    ;; is based on how the answers of the API are structured
    ;; it should be changed if the response changes
    (youdotcom-format-answer content response)))

(defun youdotcom-send-message (content type)
  "Send a message with CONTENT and display the response."
  (youdotcom-send-request content type #'youdotcom-handle-response))


(defvar youdotcom-session-started nil
  "Variable to track whether the Youdotcom session has started.")

(defun youdotcom-enter ()
  "Start a search session."
  (interactive)
  (let ((buf (get-buffer youdotcom-buffer-name)))
    (if buf
        (switch-to-buffer buf)
      (setq buf (get-buffer-create youdotcom-buffer-name))
      (split-window-sensibly)
      (switch-to-buffer buf)
      (youdotcom-mode))
    (setq youdotcom-session-started t)
    (youdotcom-start)))

(define-derived-mode youdotcom-mode fundamental-mode "Youdotcom"
  "A major mode for searching on the web with the You.com/search API."
  (read-only-mode -1)
  (youdotcom-enter))

(defun youdotcom-start ()
  "Enter the command followed by the query."
  (if youdotcom-session-started
      (let ((input (read-string "> ")))
        (let ((command (car (split-string input)))
              (query (mapconcat #'identity (cdr (split-string input)) " ")))
          (cond ((string-equal command "/quit")
                 (kill-buffer youdotcom-buffer-name)
                 (setq youdotcom-session-started nil))
                ((string-equal command "/clear")
                 (erase-buffer))
                ((string-equal command "/help")
                 (message "Commands: /quit, /clear, /help, /search, /rag"))
                ((string-equal command "/search")
                 (if (string-empty-p query)
                     (message "Please provide a query")
                   (youdotcom-send-message query "search")))
                ((string-equal command "/rag")
                 (if (string-empty-p query)
                     (message "Please provide a query")
                   (youdotcom-send-message query "rag")))
                (t
                 (message "Invalid command. type /help for available commands.")))))
    (message "Youdotcom session not started.")))

(provide 'youdotcom)
;;; youdotcom.el ends here
