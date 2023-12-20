;; -*- lexical-binding: t; -*-

(require 'json)
(require 'url)
(require 'cl-lib)

(defgroup youchat nil
  "A package to make quick searches on you.com"
  :group 'applications)

(defcustom youchat-api-key ""
  "Your secret API key for You.com."
  :type 'string
  :group 'youchat)

(defvar youchat-buffer-name "*YouChat*"
  "The name of the buffer for the YouChat session.")

(defcustom youchat-number-of-results "1"
  "The number of results that the api should return"
  :type 'string
  :group 'youchat)


(defcustom youchat-base-api-endpoint "https://api.ydc-index.io/search"
  "The base url of the you.com api for the search functionalities"
  :type 'string
  :group 'youchat)

(defun youchat-send-request (query callback)
  "Send a request to the You.com's API with the given QUERY and CALLBACK."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-API-Key" . ,youchat-api-key)))
        (url-request-data nil))
    (url-retrieve (format "%s?query=%s&num_web_results=%s" youchat-base-api-endpoint query youchat-number-of-results) callback)))

(defun youchat-format-message (message)
  "Format a MESSAGE as a string for display."
  (let ((role (cdr (assoc "role" message)))
        (content (cdr (assoc "content" message))))
    (message "The content is: %s\n" content)
    (format "%s: %s\n" role content)))

(defun youchat-display-messages (messages)
  "Display the MESSAGES in the chat buffer."
  (with-current-buffer (get-buffer-create youchat-buffer-name)
    (goto-char (point-max))
    (dolist
      (insert (youchat-format-message message)))
    (goto-char (point-max))))

(defun youchat-send-message (content)
  "Send a message with the given CONTENT to the You.com's API model and display the response."
  (youchat-send-request content
                        (lambda (status)
                          (goto-char (point-min))
                          (re-search-forward "^$")
                          (let* ((json-object-type 'alist) ;; convert the json object to lisp's lists
                                 (json-array-type 'list)
                                 (json-key-type 'symbol)
                                 (json (json-read))
                                 (hits (alist-get 'hits json))
                                 (response "")) ;; empty string that will collect all the results
			    (dolist (hit hits)
			      (let ((description (alist-get 'description hit)) ;; get each hit and the different field and concatenate them in a readable format :
				    ;; - The World's Greatest Search Engine
				    ;; Search on YDC
				    ;; I'm an AI assistant that helps you get more done. What can I help you with ?
				    ;; https://you.com
				    ;;
				    ;; - You.com - Wikipedia
				    ;; You.com is a search engine and artificial intelligence platform that aims to provide users with personalized and relevant information
				    ;; and services. It was founded in 2020 by a team of former Google engineers and researchers.
				    ;; https://en.wikipedia.org/wiki/You.com
				    (snippets (alist-get 'snippets hit))
				    (title (alist-get 'title hit))
				    (url (alist-get 'url hit)))
				(setq response (concat response  "\n# Title: " (format "%s" title) "\n\n" (format "## Description : %s" description) "\n\n" (format "%s" (mapconcat 'identity snippets "\n")) "\n\n" (format "%s" url) "\n")))) ;; this format info extractions is based on how the answer is given in the api, it should be fixed if the response change
                            (youchat-display-messages `((("role" . "user") ("content" . ,content))
                                                        (("role" . "assistant") ("content" . ,response))))))))

(defvar youchat-session-started nil
  "Variable to track whether the YouChat session has started")

(defun youchat-start ()
  "Start a search session"
  (interactive)
  (let ((buf (get-buffer youchat-buffer-name)))
    (if buf
        (switch-to-buffer buf)
      (setq buf (get-buffer-create youchat-buffer-name))
      (split-window-sensibly)
      (switch-to-buffer buf)
      (youchat-mode)
      (with-current-buffer youchat-buffer-name (markdown-mode)))
    (setq youchat-session-started t)
    (youchat-enter)))


(define-derived-mode youchat-mode fundamental-mode "YouChat"
  "A major mode for searching on the web with the You.com/search API"
  (read-only-mode -1)
  (local-set-key (kbd "RET") 'youchat-enter)
  (youchat-enter))

(defun youchat-enter ()
  "Enter a message or a command."
  (interactive)
  (if youchat-session-started
  (let ((input (read-string "> ")))
    (cond ((string-equal input "/quit")
           (kill-buffer youchat-buffer-name)
	   (setq youchat-session-started nil))
          ((string-equal input "/clear")
           (erase-buffer))
          ((string-equal input "/help")
           (message "Available commands: /quit, /clear, /help"))
          (t
	   (switch-to-buffer (get-buffer youchat-buffer-name))
           (youchat-send-message input))))
  (message "YouChat session not started. Call `youchat-start` first.")))
