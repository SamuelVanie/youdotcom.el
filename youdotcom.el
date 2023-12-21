;; -*- lexical-binding: t; -*-

(require 'json)
(require 'url)
(require 'cl-lib)

(defgroup youdotcom nil
  "A package to make quick searches on you.com"
  :group 'applications)

(defcustom youdotcom-api-key ""
  "Your secret API key for You.com."
  :type 'string
  :group 'youdotcom)

(defvar youdotcom-buffer-name "*Youdotcom*"
  "The name of the buffer for the Youdotcom session.")

(defcustom youdotcom-number-of-results "1"
  "The number of results that the api should return"
  :type 'string
  :group 'Youdotcom)


(defcustom youdotcom-base-api-endpoint "https://api.ydc-index.io/search"
  "The base url of the you.com api for the search functionalities"
  :type 'string
  :group 'youdotcom)

(defun youdotcom-send-request (query callback)
  "Send a request to the You.com's API with the given QUERY and CALLBACK."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-API-Key" . ,youdotcom-api-key)))
        (url-request-data nil))
    (url-retrieve (format "%s?query=%s&num_web_results=%s" youdotcom-base-api-endpoint query youdotcom-number-of-results) callback)))

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
  "Send a message with the given CONTENT to the You.com's API model and display the response."
  (youdotcom-send-request content
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
				(setq response (concat response  "\n\n# Title: " (format "%s" title) "\n\n" (format "## Description : %s" description) "\n\n" (format "%s" (mapconcat 'identity snippets "\n")) "\n\n" (format "%s" url) "\n")))) ;; this format info extractions is based on how the answer is given in the api, it should be fixed if the response change
                            (youdotcom-display-messages `((("role" . "user") ("content" . ,content))
                                                        (("role" . "assistant") ("content" . ,response))))))))

(defvar youdotcom-session-started nil
  "Variable to track whether the Youdotcom session has started")

(defun youdotcom-start ()
  "Start a search session"
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
  "A major mode for searching on the web with the You.com/search API"
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
