(require 'shadchen)
(require 'iso8601)
(require 'parse-csv)
(require 'org)

(defun clocker--parse-csv-event-line (line)
  (match (parse-csv-string line ?, ?\")
    ((list event-type time task-name comment)
     (list (clocker--trim event-type)
           (string-to-number (clocker--trim time))
           (clocker--trim task-name)
           (clocker--trim comment)))))

(defun clocker--iso8601-parse-duration (str)
  (clocker--normalize-duration (iso8601-parse-duration str)))

(defun clocker--seconds-to-iso8601-duration (s)
  (clocker--iso8601-parse-duration (format "PT%SS" s)))

(defun clocker--all-zero? (lst)
    (cond ((eq lst '()) t)
          ((eq (car lst) 0)
           (clocker--all-zero? (cdr lst)))
          (:else nil)))
(defun clocker--mapconcat (f &rest args)
  (apply #'concat (apply #'cl-mapcar (cons f args))))
(defun clocker--encode-part (numbers sigils)
  (clocker--mapconcat (lambda (n s)
               (cond ((= n 0) "")
                     (:else (concat (format "%S" n) s))))
             numbers sigils))

(defun clocker--iso8601-duration->string (parsed-duration)  
  (match parsed-duration
         ((list seconds minutes hours days months years nil nil nil)
          (let ((before-t-part (clocker--encode-part (list years months days) (list "Y" "M" "D")))
                (after-t-part (clocker--encode-part (list hours minutes seconds) (list "H" "M" "S"))))
            (cond ((and (equal before-t-part "")
                        (equal after-t-part ""))
                   "PT0S")
                  ((and (equal before-t-part "")
                        (not (equal after-t-part "")))
                   (concat "PT" after-t-part))
                  ((and (not (equal before-t-part ""))
                        (equal after-t-part ""))
                   (concat "P" before-t-part))
                  ((and (not (equal before-t-part ""))
                        (not (equal after-t-part "")))
                   (concat "P" before-t-part "T" after-t-part)))))))

;(iso8601-duration->string '(1 0 0 0 0 5 nil nil nil))

(cl-defmacro clocker--chain (name body0 &body body)
  `(let ((,name ,body0))
     ,@(mapcar (lambda (term) `(setq ,name ,term)) body)
     ,name))

(defun clocker--parse-task-line (str)
  (match (split-string str)
         ((or (list (and (or "-" "*") active-flag) task-name (funcall #'clocker--iso8601-parse-duration total-time))
              (and (list (and (or "-" "*") active-flag) task-name)
                   (let 
                     (total-time (clocker--iso8601-parse-duration "PT0S")))))
          (list 'task-line active-flag task-name total-time))))

(defpattern clocker--task-line (flag task-name total-time)
  `(list (and ,flag (or "-" "*"))
	 ,task-name
	 ,total-time))

(defun clocker--parsed-task-line? (parsed)
  (eq (car parsed) 'task-line))

(defun clocker--parsed-task-line-task-name (parsed)
  (elt parsed 2))

(defun clocker--parsed-line->string (parsed)
  (format "%s %s %s" (elt parsed 1)
          (elt parsed 2)
          (clocker--iso8601-duration->string (elt parsed 3))))

(defun clocker--activate-task (parsed)
  (let ((parsed (copy-list parsed)))
    (setf (elt parsed 1) "*")
    parsed))

(defun clocker--deactivate-task (parsed)
  (let ((parsed (copy-list parsed)))
    (setf (elt parsed 1) "-")
    parsed))
 
(defun clocker--task-active? (parsed)
  (equal (elt parsed 1) "*"))

(defun clocker--task-inactive? (parsed)
  (equal (elt parsed 1) "-"))

(defun clocker--task-name (parsed)
  (elt parsed 2))

(defun clocker--task-duration (parsed)
  (elt parsed 3))

(defun clocker--set-task-duration (parsed new-dur)
  (let ((parsed (copy-list parsed)))
    (setf (elt parsed 3) new-dur)
    parsed))

(defun clocker--add-to-task-duration (parsed to-add)
  (let ((parsed (copy-list parsed)))
    (setf (elt parsed 3) (clocker--add-durations
                          (elt parsed 3)
                          new-dur))))

(defun clocker--normalize-duration (parsed-duration)
  (match-let ((remaining parsed-duration)
              (maximums (list 60 60 24 30 12))
              (acc (list)))
             (match maximums
               ((list) (append (reverse acc) remaining))
               ((list* m rest-of-maximums)
                (let* ((x (car remaining))
                       (n-carry (floor (/ x m)))
                       (r (- x (* n-carry m)))
                       (rest-remaining (cdr remaining)))
                  (recur (cons (+ n-carry (car rest-remaining)) (cdr rest-remaining))
                         rest-of-maximums
                         (cons r acc)))))))

(defun clocker--add-durations (d1 d2)
  (clocker--normalize-duration
   (cl-mapcar (lambda (a b)
             (cond ((and a b) (+ a b))
                   ((and a (not b)) a)
                   ((and (not a) b) b)
                   (:else nil)))
           d1 d2)))

(defun clocker--add-duration-strings (s1 s2)
  (clocker--iso8601-duration->string
   (clocker--add-durations (clocker--iso8601-parse-duration s1)
                           (clocker--iso8601-parse-duration s2))))

(defun clocker--trim (s)
  (clocker--chain $ (replace-regexp-in-string "^[[:space:]]*" "" s)
                  (replace-regexp-in-string "[[:space:]]*$" "" $)))

(defun clocker--task-line? (s)
  (clocker--chain $ (clocker--trim s)
                  (and (not (equal $ ""))
                       (or (eq (elt $ 0) ?-)
                           (eq (elt $ 0) ?*)))))

(defun clocker--comment-line? (s)
  (not (clocker--task-line? s)))

(defun clocker--comment? (s)
  (not (eq )))

(defun clocker--cons->list (s)
  (list (car s) (cdr s)))

(defun clocker--current-line-task? ()
  (let ((line (clocker--trim (apply #'buffer-substring-no-properties
                                    (clocker--cons->list (bounds-of-thing-at-point 'line))))))
    (clocker--task-line? line)))

(defun clocker--parse-buffer-string (s)
  (let* ((active-line-number nil)
         (i 0)
         (parsed-lines (cl-mapcar (lambda (line)
                       (prog1
                           (cond
                            ((clocker--task-line? line)
                             (let ((parsed (clocker--parse-task-line line)))
                               (when (clocker--task-active? parsed)
                                 (setq active-line-number i))
                               parsed))
                            ((clocker--comment-line? line)
                             (list 'comment line)))
                         (setq i (+ i 1))))
                     (split-string s (format "\n")))))
    (list 'buffer active-line-number parsed-lines)))

(defun clocker--parsed-buffer-active-task (parsed-rep)
  (elt parsed-rep 1))

(defun clocker--parse-current-buffer ()
  (clocker--parse-buffer-string
   (save-excursion (buffer-substring-no-properties (point-min) (point-max)))))

(defun clocker--parsed-buffer-active-task? (pb)
  (elt pb 1))

(defun clocker--ensure-directory (dir)
  (when (not (file-directory-p dir))
    (mkdir dir)))

(defun clocker--parsed-buffer-lines (parsed-rep)
  (copy-list (elt parsed-rep 2)))


(defun clocker--task-line-name (ln)
  (elt ln 2))

 (defun clocker--clock-out (parsed-rep ln)
   (assert (clocker--last-event))
   (assert (clocker--parsed-buffer-active-task parsed-rep))
   (let* ((parsed-rep (copy-list parsed-rep))
          (parsed-lines (clocker--parsed-buffer-lines parsed-rep))
          (now (time-convert (current-time) 'integer))
          (last-time (clocker--last-event-time))
          (elapsed (clocker--iso8601-parse-duration (format "PT%sS" (- now last-time))))
	  (active-line (clocker--parsed-buffer-active-task parsed-rep))
	  (comment (read-string (format "Comment (out %s):" (clocker--task-line-name (elt parsed-lines active-line))))))
     (match (elt parsed-lines active-line)
       ((list 'task-line state name current-duration)
	(with-temp-buffer
	  (insert (format "\"out\", %d, %S, %S" now name comment))
	  (write-region (point-min) (point-max) (format "events/%d.csv" now)))
	(setf (elt parsed-lines active-line)
	      (list 'task-line "-" name (clocker--add-durations elapsed current-duration)))))
     (setf (elt parsed-rep 2) parsed-lines)
     parsed-rep))

(defun clocker--clock-in (parsed-rep ln)
  (let* ((line (elt (elt parsed-rep 2) ln))
         (now (time-convert (current-time) 'integer))
         (comment (read-string (format "Comment (in %s): " (clocker--task-line-name line))))
         (filename (format "events/%d.csv" now)))
    (clocker--ensure-directory "events")
    (with-temp-buffer
      (insert (format "\"in\", %d, %S, %S" now (clocker--task-name line) comment))
      (write-region (point-min) (point-max) filename))
    (clocker--buffer-transform-line parsed-rep ln #'clocker--activate-task)))

(defun clocker--last-event ()
  (let ((files (reverse (directory-files "events"))))
    (car files)))

(defun clocker--last-event-time ()
  (let ((event-file-name (clocker--last-event)))
    (clocker--chain $ (split-string event-file-name "[./]+")
                    (reverse $)
                    (cadr $)
                    (string-to-number $))))

(defun clocker--rewrite-buffer (parsed)
  (delete-region (point-min) (point-max))
  (cl-loop for line in (elt parsed 2) do
           (message (format "Processing %S" line))
           (cond            
             ((eq 'comment (car line)) (insert (cadr line)) (insert (format "\n")))
             ((eq 'task-line (car line)) (insert (clocker--parsed-line->string line)) (insert (format "\n")))
             (:else (message (format "Unrecognized line %S" line))))))

(defun clocker--buffer-lines (parsed)
  (copy-list (elt parsed 2)))

(defun clocker--buffer-transform-line (parsed ln fun)
  (let* ((parsed (copy-list parsed))
         (lines (clocker--buffer-lines parsed)))
    (setf (elt lines ln) (funcall fun (elt lines ln)))
    (setf (elt parsed 2) lines)
    parsed))

(defun clocker--go ()
  (interactive)
  (let* ((ln (- (line-number-at-pos) 1))
         (parsed (clocker--parse-current-buffer))
         (parsed-lines (elt parsed 2))
         (active (clocker--parsed-buffer-active-task? parsed))
         (_ignore          (if active 
                               (message (format "A task is active: %S" (clocker--task-name (elt parsed-lines active))))
                             (message (format "No task is active. (line %d)" ln))))
         (updated (cond
                   ((eq active ln)
                    (message (format "Clocking out of %S" (clocker--task-name (elt parsed-lines active))))
                    (clocker--clock-out parsed ln))
                   ((and active
                         (not (eq active ln)))
                    (message (format "Clocking out of %S" (clocker--task-name (elt parsed-lines active))))
                    (message (format "Clocking in to %S" (clocker--task-name (elt parsed-lines ln))))
                    (setq parsed (clocker--clock-out parsed active))
		    (setq parsed-lines (clocker--parsed-buffer-lines parsed))
                    (when (clocker--parsed-task-line? (elt parsed-lines ln))
                      (clocker--clock-in parsed ln)))
                   ((and (not active)
                         (clocker--parsed-task-line? (elt parsed-lines ln)))
                    (message (format "Clocking in to %S" (clocker--task-name (elt parsed-lines ln))))
                    (clocker--clock-in parsed ln))
                   (:else
                    (message (format "Can't do anything on this line. %S" (elt parsed-lines ln)))
                    parsed))))
    (clocker--rewrite-buffer updated)
    (goto-line (+ ln 1))))

(cl-defmacro clocker--dont (&body terms)
  nil)

(defun clocker--current-directory ()
  (replace-regexp-in-string "^Directory " "" (pwd)))

(defun clocker--list-event-files ()
  (cl-loop for file in (directory-files "events" nil "[0-9]+\.csv")
           collect (concat "events/" file)))

(defun clocker--put-events-into-buffer-as-org-table (buffer)
  (let ((wd (clocker--current-directory))
        (files (clocker--list-event-files)))
    (with-current-buffer buffer
      (cd wd)
      (delete-region (point-min) (point-max))
      (insert (format "event-type, timestamp, task-name, comment\n"))
      ;; (cl-loop for file in files do
      ;;          (insert-file file)
      ;;          (goto-char (point-max))
      ;;          (insert (format "\n")))
      (cl-loop for file in files do
               (match (clocker--chain $ (clocker--slurp-file file)
                                      (clocker--parse-csv-event-line $))
                 ((list event-type
                        (funcall #'clocker--epoch-to-readable-time time-string)
                        task-name
                        comment)
                  (insert (format "%S, %S, %S, %S\n" event-type time-string task-name comment)))))
      (goto-char (point-min))
      (org-mode)
      (org-table-convert-region (point-min) (point-max)))
    buffer))

(defun clocker--edit-events-as-org-table (&optional buffer)
  (interactive)
  (pop-to-buffer
   (clocker--put-events-into-buffer-as-org-table (get-buffer-create "*clocker-events*"))))

(defun clocker--delete-all-events ()
  (let ((r (y-or-n-p "This will delete all event logs. Hope you have a git repo!")))
    (if r
        (progn
          (cl-loop for file in (clocker--list-event-files) do
                   (delete-file file))
          t)
      nil)))

(defun clocker--write-org-table-events-out ()
  (interactive)
  (cond ((not (clocker--delete-all-events))
         (message "Didn't write org table out to events."))
        (:else
         (with-current-buffer (get-buffer "*clocker-events*")
           (let ((lines (cdr (split-string (buffer-substring-no-properties (point-min) (point-max))
                                           (format "\n")))))
             (cl-loop for line in lines do
                      (match (split-string line (regexp-quote "|") t "[[:space:]]*")
                        ((list event-type (funcall #'clocker--parse-readable-time timestamp)
                               task-name comment)
                         (with-temp-buffer
                           (insert (format "%S, %d, %S, %S"
                                           event-type
                                           timestamp
                                           task-name
                                           comment))
                           (write-region (point-min) (point-max)
                                         (format "events/%d.csv" timestamp))))
                        (anything-else (message "Ignoring %s" anything-else)))))))))

(define-minor-mode clocker-mode
  "Toggle clocker-mode
Clocker mode is a simple time tracker. It tracks total time on each task 
and creates a log of clock in and out events."
 ;; The initial value.
 :init-value nil
 ;; The indicator for the mode line.
 :lighter " clocker"
 ;; The minor mode bindings.
 :keymap
 '(([C-c C-c] . clocker--go)))

(defun clocker--parsed-buffer-get-tasks (pb)
  (if (eq pb nil)
      (clocker--parsed-buffer-get-tasks (clocker--parse-current-buffer)))
  (cl-loop for line in (clocker--parsed-buffer-lines pb)
           when (clocker--parsed-task-line? line)
           collect line))

(defun clocker--parsed-buffer-get-task (pb task-name)
  (if (eq pb nil)
      (clocker--parsed-buffer-get-task (clocker--parse-current-buffer)))
  (let ((out (cl-loop for line in (clocker--parsed-buffer-lines pb)
                      when (and (clocker--parsed-task-line? line)
                                (equal (clocker--task-line-name
                                        line) task-name))
                      collect line)))
    (cond
     ((eq out '()) nil)
     ((eq (cdr out) '()) (car out))
     (:else
      (error (format "This buffer has duplicate tasks by the name of %S." task-name))))))

(defun clocker--slurp-file (file)
  (with-temp-buffer
    (insert-file file)
    (goto-char (point-min))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun clocker--get-events-as-list ()
  (cl-loop for file in (clocker--list-event-files)
           collect (clocker--parse-csv-event-line
                    (clocker--slurp-file file))))

(defun clocker--parsed-event-task-name (pe)
  (elt pe 2))

(defun clocker--parsed-event-get-time (pe)
  (elt pe 1))

(defun clocker--parsed-event-sorter (a b)
  (< (clocker--parsed-event-get-time a)
     (clocker--parsed-event-get-time b)))

(defun clocker--get-events-in-task-groups ()
  (let ((events (clocker--get-events-as-list))
        (output (make-hash-table :test 'equal)))
    (cl-labels ((keycons (key value)
                         (let ((cv (gethash key output nil)))
                           (puthash key (cons value cv) output))))
      (cl-loop for event in events
               do
               (keycons (clocker--parsed-event-task-name event)
                        event)))
    (cl-loop for key in (hash-table-keys output)
             collect (cons key (sort (gethash key output)
                                     #'clocker--parsed-event-sorter)))))

(defun clocker--get-task-durations-from-events ()
  (let ((events-data (clocker--get-events-in-task-groups)))
    (cl-loop for event-data in events-data collect
             (cons (car event-data)
                   (clocker--seconds-to-iso8601-duration
                    (match-let ((events (cdr event-data))
                               (acc 0))
                     (match events
                       ((list) acc)
                       ((list (list* "out" time-out _))
                        acc)
                       ((list (list* "in" time-in _))
                        acc)
                       ((list* (list* "out" time-out _)
                               rest)
                        (recur rest acc))
                       ((list* (and first-in (list* "in" time-in rest-event))
                               (list* "in" other-in _) rest)
                        (recur (cons first-in rest) acc))
                       ((list* (list* "in" time-in _)
                               (list* "out" time-out _)
                               rest)
                        (recur rest
                               (+ acc (- time-out time-in)))))))))))

(gv-define-setter clocker--parsed-buffer-lines (pb new-value)
  `(progn (setf (elt ,pb 2) ,new-value) ,new-value))

;; YYYY-MM-DDTHH:MM:SS
(defun clocker--epoch-to-readable-time (ep)
  (format-time-string "%Y-%m-%dT%I:%M:%S %p" ep))

(defun clocker--parse-readable-time (string)
  (cl-labels ((->n (a) (string-to-number a)))
    (match (split-string string "[-T: ]")
      ((list y mo d h12 mi s am/pm)
       (clocker--chain $
                       (parse-iso8601-time-string
                        (format "%0.4d-%0.2d-%0.2dT%0.2d:%0.2d:%0.2d"
                                (->n y)
                                (->n mo)
                                (->n d)
                                (+ (->n h12) (if (equal (downcase am/pm) "am") 0 12))
                                (->n mi)
                                (->n s)))
                       (time-convert $ 'integer))))))

(defun clocker--recalculate-durations ()
  (interactive)
  (let* ((event-data (clocker--get-task-durations-from-events))
         (pb (clocker--parse-current-buffer))
         (pbl (clocker--parsed-buffer-lines pb)))
    (setf (elt pb 2)
          (cl-loop for line in pbl
                   collect
                   (match line
                     ((list 'task-line status task-name duration)
                      (message "found task line")
                      (cond ((assoc task-name event-data)
                             (message "found new duration")
                             (list 'task-line status task-name (cdr (assoc task-name event-data))))
                            (:else
                             (message (format "didn't find duration. %S %S" task-name event-data))
                             line)))
                     (everything-else (message (format "Something else %S" everything-else)) everything-else))))
    (clocker--rewrite-buffer pb)))

(define-key clocker-mode-map (kbd "C-c C-c") 'clocker--go)

(clocker--dont
 (with-current-buffer (find-file "./test/tasks.txt")
   (goto-line 1)
   (clocker--go))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--last-event))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--parsed-buffer-get-tasks (clocker--parse-current-buffer)))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--parsed-buffer-get-task (clocker--parse-current-buffer) "work-on-clocker")) 
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--last-event-time))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--get-events-in-task-groups))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--get-task-durations-from-events))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--recalculate-durations))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (let ((files (directory-files "events" nil "[0-9]+\.csv")))
     (with-temp-buffer
       (insert (format "event-type, timestamp, task-name, comment\n"))
       (cl-loop for file in files do
                (insert-file (concat "events/" file))
                (goto-char (point-max))
                (insert (format "\n")))
       (goto-char (point-min))
       (org-table-convert-region (point-min) (point-max))
       (write-region (point-min) (point-max) "/tmp/test.org"))))
 (with-current-buffer (get-buffer "*clocker-events*")
   (clocker--write-org-table-events-out)))

(provide 'clocker)

