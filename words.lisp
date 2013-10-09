(defword interpreted:|:| ()
  (setf (fill-pointer *code*) 0)
  (setq *ip* 0)
  (setq *this-word* (read-word))
  (setq *state* 'compile-word))

(defword immediate:|;| ()
  (emit-word "exit")
  (output-header *this-word* "dodoes_code" (word-body ":" 13) (immediatep))
  (do ((end (fill-pointer *code*))
       (i 0 (1+ i)))
      ((= i end))
    (output "  (cell)~A~:[~;,~]" (aref *code* i) (/= (1+ i) end)))
  (setq *state* 'interpret-word))

;;; (defword interpreted:immediate ()
;;;   ...)

(defword interpreted:|FORWARD:| ()
  (output-finish)
  (output "struct word ~A_word;" (mangle-word (read-word))))

(defword interpreted:defer ()
  (output-header (read-word) "dodoes_code" (word-body "perform"))
  (output "  (cell)~A" (tick "abort")))

(defword immediate:is ()
  (emit-literal (word-body (read-word)))
  (emit-word "!"))

(defword interpreted:value (x)
  (output-header (read-word) "dodoes_code" (word-body "here" 1))
  (output "  (cell)~A," x))

(defword immediate:to ()
  (emit-literal (word-body (read-word)))
  (emit-word "!"))

(defword immediate:does> ()
  (emit-word "(does>)"))

(defword interpreted:code ()
  (let* ((name (read-word))
	 (mangled (format nil "~A_code" (mangle-word name)))
	 (special-code-p nil))
    (output-finish)
    (cond
      ((equal (read-word) "\\")
       (let ((ret-type (read-word)))
	 (setq special-code-p t
	       mangled (read-word))
	 (output "~A ~A ~A" ret-type mangled (read-line *input*))))
      (t
       (read-line *input*)
       (output "xt_t * REGPARM ~A (xt_t *IP, struct word *word)" mangled)))
    (output-line "{")
    (do ((line (read-line *input*) (read-line *input*)))
	((equalp (string-trim " " line) "end-code"))
      (output-line line))
    (unless special-code-p
      (output-line "    return IP;"))
    (output-line "}")
    (output-header name (format nil "(code_t *)~A" mangled) "0" nil)))

;;;definterpreted end-code

(defword interpreted:allot (u)
  (loop repeat (ceiling (to-integer u) *cell-size*)
        do (output "  (cell)0,")))

(defword interpreted:|,| (x)
  (output "  (cell)~A," x))

(defword interpreted:|'| ()
  (push (tick (read-word)) *control-stack*))

(defword interpreted:cells (u)
  (push (cells (to-integer u)) *control-stack*))

(defword interpreted:create ()
  (output-header (read-word) "dodoes_code" (word-body "nop" 0)))

(defword immediate:|(| ()
  (do ()
      ((eql (read-char *input*) #\)))))

(defword immediate:\\ ()
  (do ()
      ((eql (read-char *input*) #\Newline))))

(defword immediate:literal (x)
  (emit-literal x))

(defword immediate:postpone ()
  (let ((word (read-word)))
    (if (immediate-word word)
	(emit-word word)
	(progn
	  (emit-literal (tick word))
	  (emit-word ",")))))

(defword immediate:postcode ()
  (emit-literal (format nil "~A_code" (mangle-word (read-word))))
  (emit-word ","))

(defword immediate:|."| ()
  (let ((string (read-word #\")))
    (emit-literal (concatenate 'string "\"" (quoted string) "\""))
    (emit-literal (length string))
    (emit-word "type")))

(defword immediate:|S"| ()
  (let ((string (read-word #\")))
    (emit-literal (concatenate 'string "\"" (quoted string) "\""))
    (emit-literal (length string))))

(defword immediate:|ABORT"| ()
  (emit-branch "0branch" :unresolved)
  (let ((string (read-word #\")))
    (emit-literal (concatenate 'string "\"" (quoted string) "\""))
    (emit-literal (length string)))
  (emit-word "cr")
  (emit-word "type")
  (emit-word "cr")
  (emit-word "abort")
  (resolve-branch))

(defword immediate:if ()
  (emit-branch "0branch" :unresolved))

(defword immediate:else ()
  (emit-branch "branch" :unresolved)
  (cs-roll 1)
  (resolve-branch))

(defword immediate:then ()
  (resolve-branch))

(defword interpreted:here ()
  (push *ip* *control-stack*))

(defvar *leave*)

(defword immediate:do ()
  (emit-word "2>r")
  (setq *leave* nil)
  (push *ip* *control-stack*))

(defword immediate:leave ()
  (emit-word "branch")
  (setq *leave* *ip*)
  (emit :unresolved))

(defword immediate:loop ()
  (emit-literal "1")
  (emit-loop "(+loop)"))

(defword immediate:+loop ()
  (emit-loop "(+loop)"))

(defword immediate:begin ()
  (push *ip* *control-stack*))

(defword immediate:again (x)
  (emit-branch "branch" x))

(defword immediate:while ()
  (emit-branch "0branch" :unresolved)
  (cs-roll 1))

(defword immediate:repeat (x)
  (emit-branch "branch" x)
  (resolve-branch))

(defword immediate:until (x)
  (emit-branch "0branch" x))

(defword interpreted:+ (n1 n2)
  ;; HUGE UGLY HACK ALERT!
  (push (+ n1 (floor n2 *cell-size*)) *control-stack*))

(defword interpreted:- (n1 n2)
  (push (- n1 n2) *control-stack*))

(defword interpreted:char ()
  (push (char-code (char (read-word) 0)) *control-stack*))

(defword immediate:[char] ()
  (let ((char (char (read-word) 0)))
    (emit-literal (cond
		    ((char= char #\') "'\\''")
		    ((char= char #\\) "'\\\\'")
		    (t (format nil "'~A'" char))))))

(defword immediate:|[']| ()
  (emit-literal (tick (read-word))))

(defword interpreted:variable ()
  (output-header (read-word) "dodoes_code" (word-body "nop" 0))
  (output-line "  0"))

(defword interpreted:cell ()
  (push *cell-size* *control-stack*))

(defword immediate:cell ()
  (emit-literal *cell-size*))

(defword interpreted:jmp_buf ()
  (push *jmp_buf-size* *control-stack*))

(defword immediate:name_length ()
  (emit-literal *name-size*))

(defword immediate:to_next ()
  (emit-literal *next-offset*))

(defword immediate:to_code ()
  (emit-literal *code-offset*))

(defword immediate:to_does ()
  (emit-literal *does-offset*))

(defword immediate:to_body ()
  (emit-literal *body-offset*))

(defword immediate:|[| ()
  (setq *state* 'interpret-word))

(defword interpreted:|]| ()
  (setq *state* 'compile-word))

(defword interpreted:invert (u)
  (push (mask-word (lognot (to-integer u))) *control-stack*))

(defword interpreted:rshift (u n)
  (push (ash (to-integer u) (- (to-integer n))) *control-stack*))

(defword interpreted:= (x1 x2)
  (push (if (= (to-integer x1) (to-integer x2)) -1 0) *control-stack*))

(defword interpreted:> (n1 n2)
  (push (if (> (to-integer n1) (to-integer n2)) -1 0) *control-stack*))

(defword immediate:[defined] ()
  (push (defined (read-word)) *control-stack*))

(defword immediate:[undefined] ()
  (immediate:[defined])
  (push (lognot (pop *control-stack*)) *control-stack*))

(defword interpreted:include ()
  (interpret-file (read-word)))
      
(defword immediate:[if] (x)
  (when (zerop (to-integer x))
    (skip-until "[then]" "[else]")))

(defword immediate:[else] ()
  (skip-until "[then]"))

(defword immediate:[then] ()
  nil)

;;; Print control stack.
(defword immediate:.cs ()
  (format *trace-output* "<~D> " (length *control-stack*))
  (dolist (x (reverse *control-stack*))
    (format *trace-output* "~A " x)))
