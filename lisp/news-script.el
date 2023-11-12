(add-to-list 'exec-path "~/.emacs.d/easyscripts/")

(setq news-data "~/.emacs.d/easyscripts/news.data")
(setq news-buffer-name "news-search")

(defun read-news-file ()  
  "Return a list of lines from FILE."
  (with-temp-buffer
    (insert-file-contents news-data)
    (split-string (buffer-string) "\n" t)))

(defun open-stories-url (link)
  "Function to be executed when a link is clicked."
  (interactive)
  (browse-url link))

(defun insert-news-stories (title date link)
  "Insert a clickable link with LABEL."
  (insert-button (format " %s\t%s\n" date title link)
                 'action `(lambda (_) (open-stories-url ,link))
                 'follow-link t
		 'face `(:foreground ,"gray65" :height ,1.3)
		 ))

(defun get-news-stories (topic)
  ;; Create a New Buffer
  (generate-new-buffer news-buffer-name)
  (switch-to-buffer news-buffer-name)
  (hl-line-mode 1) ;  Enable highlight mode
  ;; (read-only-mode 1); Enable Read Only Mode
  (insert (format "Topic: %s \n\n" topic))  

  ; List of all stories found
  (setq all-news-stories (read-news-file))
  (setq list-length (length all-news-stories))  ; Length of the list
  ; Loop over Data and Insert Stories to Buffer
  (dotimes (i list-length)
    (setq news-list (split-string (nth i all-news-stories) "||"))    ; Split to get Separate Data Points
    ; Title, Time, Url
    (insert-news-stories (nth 0 news-list)
			 (nth 2 news-list)
			 (nth 3 news-list))))

(defun news-script ()
  (interactive)
  (setq TOPIC (read-string "Enter a Topic: "))
  (message " Loading ... ")
  (set-process-sentinel
   (start-process "News" nil "news" TOPIC)
   (lambda (p e)
     (when (= 0 (process-exit-status p))
       (message " Finished ")
       (get-news-stories TOPIC)
       (goto-char (point-min))
       ))))

