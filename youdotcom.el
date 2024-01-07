;;; youdotcom.el --- You.com search package -*- lexical-binding: t; -*-

;; Author: Samuel Michael Vanié <samuelmichaelvanie@gmail.com>
;; Version: 0.1
;; Package-requires: ((emacs "25.1"))
;; Keywords: ai, tools
;; URL: https://github.com/SamuelVanie/youdotcom.el

;;; Commentary:

;; This package provides an interactive interface to You.com's search API

;;; Code:
;; user facing functionality prefixed youdotcom-, developer oriented API prefixed youdotcom-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'json)
(require 'url)
(require 'cl-lib)

(defgroup youdotcom nil
  "A package to make quick searches on you.com."
  :group 'applications)

(defcustom youdotcom-api-key "1b674f83-5400-44cf-96eb-81a3e9944cb2<__>1OHyzyETU8N2v5f4A8MSTZoG"
  "Your secret API key for You.com."
  :type 'string
  :group 'youdotcom)

(defvar youdotcom-buffer-name "*Youdotcom*"
  "The name of the buffer for the Youdotcom session.")

(defcustom youdotcom-number-of-results "1"
  "The number of results that the api should return."
  :type 'string
  :group 'Youdotcom)


(defcustom youdotcom-base-api-endpoint "https://api.ydc-index.io/search"
  "The base url of the you.com api for the search functionalities."
  :type 'string
  :group 'youdotcom)

(defun youdotcom-send-request (query callback)
  "Send a request to the You.com's API with the given QUERY and CALLBACK."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-API-Key" . ,youdotcom-api-key)))
        (url-request-data nil))
    (url-retrieve (format "%s?query=%s&num_web_results=%s"
                          youdotcom-base-api-endpoint
                          query
                          youdotcom-number-of-results)
                  callback)))

(defun youdotcom-format-message (message)
  "Format a MESSAGE as a string for display."
  (let ((role (cdr (assoc "role" message)))
        (content (cdr (assoc "content" message))))
    (format "%s: %s\n" role content)))

(defun youdotcom-display-messages (messages)
  "Display the MESSAGES in the chat buffer."
  (with-current-buffer (get-buffer-create youdotcom-buffer-name)
    (goto-char (point-max))
    (dolist (message messages)
      (insert (youdotcom-format-message message)))
    (goto-char (point-min))))

(defun youdotcom-send-message (content)
  "Send a message with CONTENT to the You.com's API model and display the response."
  (youdotcom-send-request content
    (lambda ()
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol)
                (json (json-read))
                (hits (alist-get 'hits json))
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
        ;; REVIEW: this info extractions
        ;; is based on how the answer is given by the
        ;; api, it should be fixed if the response
        ;; change
            (youdotcom-display-messages
                `((("role" . "user")
                ("content" . ,content))
                (("role" . "assistant")
                ("content" . ,response))))))))

(defvar youdotcom-session-started nil
  "Variable to track whether the Youdotcom session has started.")

(defun youdotcom-start ()
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
    (youdotcom-enter)))


(define-derived-mode youdotcom-mode fundamental-mode "Youdotcom"
  "A major mode for searching on the web with the You.com/search API."
  (read-only-mode -1)
  (local-set-key (kbd "RET") 'youdotcom-enter)
  (youdotcom-enter))

(defun youdotcom-enter ()
  "Enter a message or a command."
  (interactive)
  (if youdotcom-session-started
  (let ((input (read-string "> ")))
    (cond ((string-equal input "/quit")
           (kill-buffer youdotcom-buffer-name)
	   (setq youdotcom-session-started nil))
          ((string-equal input "/clear")
           (erase-buffer))
          ((string-equal input "/help")
           (message "Available commands: /quit, /clear, /help"))
          (t
	   (switch-to-buffer (get-buffer youdotcom-buffer-name))
           (youdotcom-send-message input))))
  (message "Youdotcom session not started. Call `youdotcom-start` first.")))

(provide 'youdotcom)
;;; youdotcom.el ends here
