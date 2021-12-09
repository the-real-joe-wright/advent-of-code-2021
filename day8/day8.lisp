(ql:quickload 'str)

(defun string-to-charlist (str)
  "Convert a string to a list of characters."
  (loop for ch across str collect ch))

(defun find-1-and-4-strings (input-line)
  (let ((strings (str:split " " input-line :omit-nulls t))
        (str1 nil)
        (str4 nil))
    (loop for str in strings
          do (when (= (length str) 2) (setf str1 str))
          (when ( = (length str) 4) (setf str4 str)))
    (values str1 str4)))

(defun process-line (line)
  "Reads an input line and returns the integer value of its output section."
  (let* ((file-line-splits (str:split "|" line :omit-nulls t))
         (file-line-input (first file-line-splits))
         (file-line-output (second file-line-splits))
         (output-str (make-array 4 :element-type 'character :fill-pointer 0 :adjustable t)))

    (multiple-value-bind (str1 str4)
      (find-1-and-4-strings line)
      ; read the output and determine the digits
      (let* ((digit1 (string-to-charlist str1))
             (digit4 (string-to-charlist str4))
             ; calculate the 4-1 set
             (digit4-1 (set-difference digit4 digit1)))
        (loop for s in (str:split " " file-line-output :omit-nulls t) 
              do (vector-push-extend (coerce  (calculate-digit s digit1 digit4 digit4-1) 'character) output-str)
              )))
    (parse-integer output-str)))

(defun calculate-digit (digitstr digit1 digit4 digit4-1)
  "Input: 
       digitstr: string representing a digit;
       digit1: list of characters for the '1' digit;
       digit4: list of characters for the '4' digit;
       digit4-1: set-difference betwen the '4' and '1' digits;
   Output: single digit (string) represented by the input string"
  (let ((result 
          (case (length digitstr)
            (2 "1")
            (3 "7")
            (4 "4")
            (7 "8")
            (5 (calculate-5len digitstr digit1 digit4 digit4-1))
            (6 (calculate-6len digitstr digit1 digit4 digit4-1))
            )))
    result))

(defun calculate-5len (digitstr digit1 digit4 digit4-1)
  (let ((digitset (string-to-charlist digitstr)))
    (cond
      ((subsetp digit1 digitset) "3" )
      ((subsetp digit4-1 digitset) "5")
      (t "2"))))

(defun calculate-6len (digitstr digit1 digit4 digit4-1)
  (let ((digitset (string-to-charlist digitstr)))
    (cond
      ((not (subsetp digit1 digitset)) "6" )
      ((subsetp digit4-1 digitset) "9")
      (t "0"))))

(defun read-file-data (filename)
  "Read data from filename."
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun process-data-lines (data)
  "Process the lines from the file."
  (let ((result 0))
    (loop for line in data
          do (incf result (process-line line)))
    result))

(defun star2 (filename)
  (let ((filedata (read-file-data filename))
        (result 0))
    (setf result (process-data-lines filedata))
    result))

(star2 "/Users/joe/advent-of-code-2021/8.txt")
