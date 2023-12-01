;; -*- lexical-binding: t; -*-

(require 'json)
(require 'url)
(require 'cl-lib)

(defgroup youchat nil
  "A package for chatting with the You.com/chat AI model inside Emacs."
  :group 'applications)

(defcustom youchat-api-key ""
  "Your secret API key for You.com."
  :type 'string
  :group 'youchat)

(defvar youchat-buffer-name "*YouChat*"
  "The name of the buffer for chatting with the You.com/chat AI model.")

;; (defcustom youchat-api-endpoint "https://api.ydc-index.io/search?query=%s"
;;   "Default API endpoint for the Youdotcom chat api"
;;   :type 'string
;;   :group 'youchat)

(defun youchat-send-request (query callback)
  "Send a request to the You.com/chat AI model with the given QUERY and CALLBACK."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-API-Key" . ,youchat-api-key)))
        (url-request-data nil))
    (url-retrieve (format "https://api.ydc-index.io/search?query=%s" query) callback)))

(defun youchat-format-message (message)
  "Format a MESSAGE as a string for display."
  (let ((role (alist-get 'role message))
        (content (alist-get 'content message)))
    (format "%s: %s\n" (capitalize role) content)))

(defun youchat-display-messages (messages)
  "Display the MESSAGES in the chat buffer."
  (message messages)
  (with-current-buffer (get-buffer-create youchat-buffer-name)
    (erase-buffer)
    (dolist (message messages)
      (insert (youchat-format-message message))) ;; call the format function for each message
    (goto-char (point-max))))

(defun youchat-send-message (content)
  "Send a message with the given CONTENT to the You.com/chat AI model and display the response."
  (youchat-send-request content
                        (lambda (status)
                          (goto-char (point-min))
                          (re-search-forward "^$")
                          (let* ((json-object-type 'alist)
                                 (json-array-type 'list)
                                 (json-key-type 'symbol)
                                 (json (json-read))
                                 (hits (alist-get 'hits json))
                                 (message (car hits)))
                            (youchat-display-messages `((("role" . "user") ("content" . ,content))
                                                        (("role" . "assistant") ("content" . ,(alist-get 'snippets message)))))))))

(defun youchat-start ()
  "Start a chat session with the You.com/chat AI model."
  (interactive)
  (switch-to-buffer (get-buffer-create youchat-buffer-name))
  (youchat-mode))

(define-derived-mode youchat-mode fundamental-mode "YouChat"
  "A major mode for chatting with the You.com/chat AI model."
  (read-only-mode 1)
  (local-set-key (kbd "RET") 'youchat-enter))

(defun youchat-enter ()
  "Enter a message or a command."
  (interactive)
  (let ((input (read-string "> ")))
    (cond ((string-equal input "/quit")
           (kill-buffer youchat-buffer-name))
          ((string-equal input "/clear")
           (erase-buffer))
          ((string-equal input "/help")
           (message "Available commands: /quit, /clear, /help"))
          (t
           (youchat-send-message input)))))
