(require 'shadchen)
(require 'iso8601)

(defun clocker--iso8601-parse-duration (str)
  (clocker--normalize-duration (iso8601-parse-duration str)))

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
	  (write-region (format "events/%d.csv" now) (point-min) (point-max)))
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

(clocker--dont
 (with-current-buffer (find-file "./test/tasks.txt")
   (goto-line 1)
   (clocker--go))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--last-event))
 (with-current-buffer (find-file-noselect "./test/tasks.txt")
   (clocker--last-event-time)))
