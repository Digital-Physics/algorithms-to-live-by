(defun find-min-time (load-pairs)
  "Find the load with the minimum wash or dry time in a list of wash-dry pairs.
   Returns (min-index is-wash), where is-wash is T if the smallest time is a wash time, NIL otherwise.

   Notes: 
   do is a general-purpose loop construct in Common Lisp. It allows iteration with variables that can change their values during the loop.
   The loop with two simultaneous for expression ensures we process each load in load-pairs while tracking its index.
   when is like an if statement without an else branch
   setf is used to assign values to variables or data structures. It's more powerful than setq, allowing updates to complex data structures like lists, arrays, or hash tables.
   values returns multiple values while return exits a loop with one value.
   Without values, you'd need to return a list, tuple, or other structure, which adds unnecessary complexity when the outputs are logically separate.
"
  (let ((min-time nil)
        (min-index nil)
        (is-wash nil))
    (loop for index from 0
          for (wash-time dry-time) in load-pairs
          do
            (when (or (null min-time) (< wash-time min-time))
              (setf min-time wash-time
                    min-index index
                    is-wash t))
            (when (< dry-time min-time)
              (setf min-time dry-time
                    min-index index
                    is-wash nil)))
    (values min-index is-wash)))

(defun calculate-cumulative-time (load-pairs)
  "Calculate cumulative times for washing and drying.
  
  Notes: 
  dolist macro iterates over each element of a list, executing a block of code for each element. load here is the var name given to each element.
  let* binds variables sequentially so later variables can depend on previous variables, while let binds them in parrallel, or at least without consideration.
  push adds to the front in O(1) (but is destructive and renames variable); it is shorthand for:
        (setf cumulative-times (cons new-element cumulative-times))
        It is ok to use push here instead of cons because we are iteratively updating it without doing multiple function calls recursively.
  while append traverses entire list to add link to linked list tail (without a pointer) in O(n). (this is why we push and nreverse instead of append)
  "
  (let ((cumulative-times nil)
        (current-wash-end 0)
        (current-dry-end 0))
    (dolist (load load-pairs)
      (let* ((wash-time (first load))
             (dry-time (second load))
             (wash-end (+ current-wash-end wash-time))
             (dry-start (max wash-end current-dry-end))
             (dry-end (+ dry-start dry-time)))
        ;; Debug output
        ; (format t "Cumulative Times:~A ~%" cumulative-times)
        ; (format t "Processing load: Wash=~A, Dry=~A~%" wash-time dry-time)
        ; (format t "  Wash start=~A, Wash end=~A, Dry start=~A, Dry end=~A~%" current-wash-end wash-end dry-start dry-end)
        (push (list current-wash-end wash-end dry-start dry-end) cumulative-times)
        (setf current-wash-end wash-end
              current-dry-end dry-end)))
    ;; Debug output
    ; (format t "Cumulative times: ~A~%" (reverse cumulative-times))
    (nreverse cumulative-times)))

(defun print-gantt-chart (load-pairs)
  "Print a Gantt chart for wash-dry schedules."
  (format t "~%==========================================~%")
  (format t "           Gantt Chart (Info)~%")
  (format t "==========================================~%")

  ;; Calculate cumulative times
  (let ((cumulative-times (calculate-cumulative-time load-pairs)))
    ;; Washer schedule
    (format t "~%WASHER SCHEDULE:~%------------------------------------------~%")
    (loop for i from 0
          for (wash-start wash-end dry-start dry-end) in cumulative-times
          do (format t "Load ~A: |~A-~A| (~A minutes)~%" (1+ i) wash-start wash-end (- wash-end wash-start)))

    ;; Dryer schedule
    (format t "~%~%DRYER SCHEDULE:~%------------------------------------------~%")
    (loop for i from 0
          for (wash-start wash-end dry-start dry-end) in cumulative-times
          do (format t "Load ~A: |~A-~A| (~A minutes)~%" (1+ i) dry-start dry-end (- dry-end dry-start)))))

(defun optimize-laundry (loads &optional (start-order '()) (end-order '()))
  "Recursively optimize laundry order by scheduling loads based on the smallest time (wash or dry).
  
  Notes:
  We use cons instead of push to put on element on front of list because we use recurssion here. 
  push mutates a variable. cons in functional and returns new linked list.
  progn groups multiple expressions into a single block, evaluating them sequentially.
  multiple-value-bind handles multiple (two) pieces of information returned by find-min-time, the index and the boolean.
  
  "
  (if (null loads)
      ;; Base case: Print Gantt chart when all loads are scheduled
      (progn
        ; (format t "Final scheduling order: Start-order=~A, End-order=~A~%" start-order end-order) ;; Debug output
        (print-gantt-chart (append start-order end-order)))
      ;; Recursive case
      (multiple-value-bind (min-index is-wash) (find-min-time loads)
        (let ((selected-load (nth min-index loads))
              (remaining-loads (remove-if-index min-index loads)))
          ;; Debug output
        ;   (format t "Selected load: ~A, Is-Wash=~A~%" selected-load is-wash)
        ;   (format t "Remaining loads: ~A~%" remaining-loads)
          (if is-wash
              (optimize-laundry remaining-loads (append start-order (list selected-load)) end-order)
              (optimize-laundry remaining-loads start-order (cons selected-load end-order)))))))

(defun remove-if-index (index load_list)
  "Remove the element at INDEX from LIST.
  Notes: 
  unless is like the opposite of an if.
  we don't need to initialize a list with collect and we can construct the new linked list dynamically.
  "
  (loop for i from 0
        for elem in load_list
        unless (= i index) 
        collect elem))

(defun test-laundry-scheduler ()
  "Test the laundry scheduler with sample data."
  (let ((loads '((30 25) (2 15) (45 40))))
    (format t "Input loads (wash_time, dry_time):~%")
    (dolist (pair loads)
      (format t "Wash: ~A, Dry: ~A~%" (first pair) (second pair)))
    (format t "~%Scheduling and generating Gantt chart...~%")
    (optimize-laundry loads)))

(test-laundry-scheduler)
