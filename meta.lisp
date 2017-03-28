;;;; -*- lisp -*- Copyright 2004, 2013-2017 Lars Brinkhoff

;;; Meta compiler to C target.
;;
;; Usage: (compile-forth "nucleus.fth" <optionally more sources> "kernel.fth")
;;
;; The output is a C file containing a dictionary with a linked list
;; of "struct word" as defined by forth.h.  CODE words are emitted as
;; C functions, and colon definitions generate indirect threaded code.

;;; Words (partially) supported by this meta compiler:
;;
;; ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED] INCLUDE
;; : ; IMMEDIATE DOES> DEFER CODE END-CODE
;; VARIABLE VALUE CREATE ALLOT HERE , ' CELLS INVERT RSHIFT CHAR > = + -
;; [CHAR] ['] [ ] LITERAL POSTPONE COMPILE [COMPILE] 'DODOES TO IS ." S"
;; ABORT" AHEAD IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT
;; UNTIL CELL NAME_LENGTH TO_NEXT TO_CODE TO_DOES TO_BODY
;; .CS

;;; Restrictions and special features:
;;
;; Many words are immediate and only work in compilation mode,
;; i.e. they always append code to the current definition.
;;
;; CODE may be followed by a \ comment which specifies the generated C
;; function declaration.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :meta
  (:use :cl))

(in-package :meta)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Retreive some information passed from C.
(load "params.lisp")

(defvar *this-word*)
(defvar *previous-word*)
(defvar *vocabulary* nil)
(defvar *control-stack* nil)
(defvar *state* 'interpret-word)
(defvar *input*)
(defvar *output*)
(defvar *here* 0)
(defvar *finished-p* t)
(defvar *dictionary* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *deferred* nil)

(defun trivial-quit ()
  #+sbcl
  (#.(or (find-symbol "EXIT" "SB-EXT") (find-symbol "QUIT" "SB-EXT")))
  #+clisp
  (ext:quit)
  #+ecl
  (si:quit)
  #+ccl
  (ccl:quit)
  #+gcl
  (lisp:bye)
  #-(or sbcl clisp ecl ccl gcl)
  (implementation-dependent-quit))

#-ecl
(declaim (ftype function interpret-file compile-word mangle-char mangle-word
		output output-line output-name output-finish quoted read-word
		whitespacep))

(defun output-extern (name)
  (output "extern struct word ~A_word;" (mangle-word name)))

(defun cl-user::compile-forth (&rest input-files
			       &aux
			       (output-file (output-name input-files)))
  (with-open-file (*output* output-file :direction :output
			                :if-exists :supersede)
    (output-line "#include \"forth.h\"")
    (output-extern "docol,")
    (let ((*previous-word* "0"))
      (dolist (file input-files)
	(interpret-file file))
      (output-finish)))
  (trivial-quit))

(defun interpret-file (file)
  (loop for path in '("" "src/")
     for file-path = (concatenate 'string path file)
     when (probe-file file-path) do
       (with-open-file (*input* file-path)
	 (do ((word (read-word) (read-word)))
	     ((null word))
	   (funcall *state* word)))))

(defun emit (string)
  (unless (stringp string)
    (setq string (format nil "~A" string)))
  (vector-push-extend string *dictionary*)
  (incf *here* *cell-size*)
  (values))

(defun trunc-word (word)
  (subseq word 0 (min (length word) (1- *name-size*))))

(defun tick (word)
  (format nil "&~A_word" (mangle-word word)))

(defun word-body (word &optional (n 0))
  (format nil "~A.param[~D]" (tick word) n))

(defun branch-target (dest)
  (word-body *this-word* (floor dest *cell-size*)))

(defun emit-word (word)
  (emit (tick word)))

(defun emit-literal (x)
  (emit-word "(literal)")
  (if (and (integerp x) (plusp x))
      (emit (format nil "~DU" x))
      (emit x)))

(defun emit-branch (word dest)
  (emit-word word)
  (if (eq dest :unresolved)
      (push *here* *control-stack*)
      (setq dest (branch-target dest)))
  (emit dest))

(defun resolve-branch (&optional (orig (pop *control-stack*)))
  (setf orig (floor orig *cell-size*))
  (setf (aref *dictionary* orig) (branch-target *here*))
  (values))

(defun output (format &rest args)
  (output-line (apply #'format nil format args)))

(defun output-line (line)
  (fresh-line *output*)
  (write-line line *output*)
  (values))

(defun output-name (files)
  (let* ((file (car (last files)))
	 (pos (position #\/ file)))
    (when pos
      (setq file (subseq file (1+ pos))))
    (merge-pathnames (make-pathname :type "c") file)))

(defun output-finish ()
  (when *deferred*
    (output (shiftf *deferred* nil)))
  (unless *finished-p* ;(string= *previous-word* "0")
    (output-line "} };"))
  (setq *finished-p* t))

(defvar *peeked-word* nil)

(defun read-word (&optional (delimiter nil delimp))
  (or (shiftf *peeked-word* nil)
      (let ((word-end-p
	     (if delimp
		 (lambda (char) (eql char delimiter))
		 (progn
		   (peek-char t *input* nil)
		   #'whitespacep))))
	(do ((word nil)
	     (char (read-char *input* nil) (read-char *input* nil)))
	    ((or (null char)
		 (funcall word-end-p char))
	     word)
	  (setq word (concatenate 'string word (string char)))))))

(defun peek-word (&rest args)
  (setq *peeked-word* (apply #'read-word args)))

(defun whitespacep (char)
  (or (char= char #\Tab)
      (char= char #\Space)
      (char= char #\Newline)))

(defpackage :interpreted
  (:use)
  (:export ":" "?:" "FORWARD:" "DEFER" "VALUE" "CODE" "ALLOT" "," "'" ".(" "CR"
	   "CELLS" "CREATE" "HERE" "+" "-" "1+" "*" "CHAR" "VARIABLE" "CELL" "IS"
	   "]" "INVERT" "RSHIFT" "=" ">" "INCLUDE" "CONSTANT" "LSHIFT"))

(defpackage :immediate
  (:use)
  (:export ";" "IS" "TO" "DOES>" "(" "\\" "LITERAL" "POSTPONE" "'DODOES"
	   "COMPILE" "[COMPILE]" ".\"" "S\"" "ABORT\"" "AHEAD" "IF" "ELSE"
	   "THEN" "DO" "LEAVE" "LOOP" "+LOOP" "BEGIN" "AGAIN" "WHILE"
	   "REPEAT" "UNTIL" "[CHAR]" "[']" "CELL" "NAME_LENGTH" "TO_NEXT"
	   "TO_CODE" "TO_DOES" "TO_BODY" "[" "[DEFINED]" "[UNDEFINED]"
	   "[IF]" "[ELSE]" "[THEN]" ".CS"))

(defmacro defword (name lambda-list &body body)
  `(progn
     (setf (get ',name 'args) ',lambda-list)
     (defun ,name ,(remove '&parse lambda-list) ,@body)))

(defun find-word (word package)
  (find-symbol (string-upcase word) package))

(defun immediate-word (word)
  (find-word word "IMMEDIATE"))

(defun interpreted-word (word)
  (find-word word "INTERPRETED"))

(defun to-integer (x)
  (etypecase x
    (number	x)
    (string	(parse-integer x))))

(defun execute (word)
  (do* ((args nil)
	(lambda-list (get word 'args) (rest lambda-list))
	(arg (first lambda-list) (first lambda-list)))
       ((null lambda-list)
	(let ((value (apply word args)))
	  (and value (push value *control-stack*))))
    (if (eq arg '&parse)
	(setq args (nconc args (list (read-word)))
	      lambda-list (rest lambda-list))
	(let ((datum (pop *control-stack*))
	      (type (char (symbol-name arg) 0)))
	  (when (member type '(#\N #\U))
	    (setq datum (to-integer datum)))
	  (push datum args)))))

(defun compile-word (word)
  (cond
    ((immediate-word word)
     (execute (immediate-word word)))
    ((multiple-value-bind (i p) (parse-integer word :junk-allowed t)
       (when (and i (= p (length word)))
	 (emit-literal i)
	 t)))
    (t
     (emit-word word))))

(defun interpret-word (word)
  (cond
    ((interpreted-word word)
     (execute (interpreted-word word)))
    ((immediate-word word)
     (execute (immediate-word word)))
    (t
     (push word *control-stack*))))

(defun output-header (name code does &optional immediatep)
  (push name *vocabulary*)
  (let* ((mangled (mangle-word name))
	 (latestxt (tick name))
	 (name (trunc-word name))
	 (len (length name)))
    (when immediatep
      (setq len (- len)))
    (output-finish)
    (output "struct word ~A_word = { ~D, \"~A\", ~A, ~A, ~A, {"
	    mangled len (quoted name) *previous-word* does code)
    (setq *previous-word* latestxt)
    (setq *finished-p* nil)))

(defun mangle-word (name)
  (cond
    ((and (char= (char name 0) #\()
	  (char= (char name (1- (length name))) #\)))
     (concatenate 'string "do" (mangle-word (string-trim "()" name))))
    ((char= (char name (1- (length name))) #\0)
				name)
    ((equal name "0branch")	"zbranch")
    ((equal name ">r")		"to_r")
    ((equal name "2>r")		"two_to_r")
    ((equal name "r>")		"r_from")
    ((equal name "2r>")		"two_r_from")
    ((equal name "0=")		"zero_equals")
    ((equal name "0<")		"zero_less")
    ((equal name "0>")		"zero_greater")
    (t				(apply #'concatenate 'string
				       (map 'list #'mangle-char name)))))

(defun mangle-char (char)
  (case char
    (#\!	"store")
    (#\"	"quote")
    (#\#	"number")
    (#\'	"tick")
    (#\(	"paren")
    (#\*	"star")
    (#\+	"plus")
    (#\,	"comma")
    (#\-	"minus")
    (#\.	"dot")
    (#\/	"slash")
    (#\0	"zero")
    (#\1	"one")
    (#\2	"two")
    (#\3	"three")
    (#\4	"four")
    (#\:	"colon")
    (#\;	"semicolon")
    (#\<	"lt")
    (#\=	"eq")
    (#\>	"gt")
    (#\?	"query")
    (#\@	"fetch")
    (#\[	"lbracket")
    (#\]	"rbracket")
    (#\\	"backslash")
    (t		(string char))))

(defun quoted (name)
  (concatenate 'string
    (loop for char across name
	  when (or (eql char #\") (eql char #\\))
	    collect #\\
	  collect char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun immediatep ()
  (and (equal (peek-word) "immediate")
       (read-word)))

;;;definterpreted end-code

(defun roll (n list)
  (let ((tail (nthcdr n list)))
    (append (list (first tail)) (ldiff list tail) (rest tail))))

(defun cs-roll (n)
  (setq *control-stack* (roll n *control-stack*))
  (values))

(defvar *leave*)

(defun emit-loop (word)
  (emit-word word)
  (emit-branch "0branch" (pop *control-stack*))
  (dolist (dest *leave*)
    (resolve-branch dest))
  (setq *leave* nil)
  (emit-word "unloop"))
  
(defun ends-with-p (string1 string2)
  (let ((n (- (length string1) (length string2))))
    (and (>= n 0) (string= string1 string2 :start1 n))))

(defun mask-word (x)
  (logand x (1- (ash 1 (* 8 *cell-size*)))))

(defun word-found-p (word vocabulary)
  (member word vocabulary :test #'string-equal))

(defun skip-until (&rest words)
  (do ((word (read-word) (read-word)))
      ((word-found-p word words))
    (when (string-equal word "[if]")
      (skip-until "[then]"))))
      
(load "lisp/words.lisp")
