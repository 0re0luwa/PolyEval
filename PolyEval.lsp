(defun split-string (string delimiter)
  "Splits a string into a list of substrings"
  (let ((start 0)
        (result '()))
    (loop for i from 0 to (1- (length string))
          do (if (char= (elt string i) delimiter)
                 (progn
                   (push (subseq string start i) result)
                   (setf start (1+ i))
                 )
          )
    )
    (push (subseq string start) result)
    (nreverse result)
  )
)

(defun split-and-parse-integers (string delimiter)
  "Splits a string by the delimiter and parses the substrings into integers."
  (mapcar #'parse-integer (split-string string delimiter))
)

(defun read-file (file-name)
  "Reads coefficients from a file and returns them as a list of integers."
  (with-open-file (stream file-name)
    (loop for line = (read-line stream nil)
          while line
          append (split-and-parse-integers line #\,)
    )
  )
)
          
(defun string-join (string separator)
   "Joins a list of string with a specified separator"
   (apply #'concatenate 'string
   		(loop for str in string	
   		   for first = t then nil
   		   append (if first (list str)(list separator str))
   		)
   )
)


(defun display-polynomial(coefficients)
   "Displays the polynomial as a string"
   (let ((terms '()))
   	 (loop for coef in coefficients
   	   for index from 0
   	   unless (= coef 0)
   	   do (push (cond
   	   			 ((= index 0)(princ-to-string coef))  ; constant term
   	   			 ((= index 1)(concatenate 'string (princ-to-string coef) "x")) ;x^1 term
   	   			 (t (concatenate 'string (princ-to-string coef) "x^" (princ-to-string index))) ;matches the index as the exponents
   	   			)
   	   	   terms
   	   	  )
   	 )
   	 (string-join (reverse terms) " + ")
   )
)

(defun evaluate-polynomial (coefficients x-value)
   "Evaluates the polynomial at a given value of x"
   (loop for coef in coefficients
   		for index from 0
   		sum(* coef (expt x-value index))
   )
)

(defun main ()
   "Main function to display polynomial, prompt for the x-value, and print result"
   (format t "Enter the file name fo the polynomial coefficients: ")
   (finish-output)
   (let* ((file-name (read-line))
   		(coefficients (read-file file-name)))
   
      (format t "Your polynomial is: ~a~%" (display-polynomial coefficients))
      (format t "Enter a value for x: ")
      (finish-output)
      (let ((x-value (parse-integer (read-line))))
   	     (format t "Result: ~a~%" (evaluate-polynomial coefficients x-value))
      )
   )
   "DONE"
)      
