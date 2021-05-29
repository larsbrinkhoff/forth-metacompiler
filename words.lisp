(defword immediate:[ ()
  (setq *state* 'interpret-word)
  (values))

(defword interpreted:] ()
  (setq *state* 'compile-word)
  (values))

(defword interpreted:|:| (&parse name)
  (setf (fill-pointer *dictionary*) 0)
  (setq *here* 0)
  (setq *this-word* name)
  (interpreted:]))

( ignore-definition ()
  ( until (string= (read-word) ";")))  

(defword interpreted:|?:| (&parse name)
  ((word-found-p name *vocabulary*)
    (ignore-definition)
    (interpreted:|:| name)))

(defword immediate:|;| ()
  (emit-word "exit")
  (output-header *this-word* "dodoes_code" (word-body "docol," 4) (immediatep))
  ( ((end (fill-pointer *dictionary*))
       (i 0 (1+ i)))
      ((= i end))
    (output "  (cell)~A~:[~;,~]" (aref *dictionary* i) (/= (1+ i) end)))
  (immediate:[))

;;; (defword interpreted:immediate ()
;;;   ...)

(defword interpreted:|FORWARD:| (&parse name)
  (output-finish)
  (output-extern name))

(defword interpreted:defer (&parse name)
  (output-header name "dodoes_code" (word-body "perform"))
  (setq *deferred* (format nil "  (cell)~A" (tick "abort"))))

(defword immediate:is (&parse name)
  (emit-literal (word-body name))
  (emit-word "!"))

(defword interpreted:is (&parse name)
  (declare (ignore name))
  (setq *deferred* (format nil "  (cell)~A" (pop *control-stack*)))
  (values))

(defword interpreted:value (x &parse name)
  (output-header name "dodoes_code" (word-body "dup" 1))
  (output "  (cell)~A," x))

(defword interpreted:constant (x &parse name)
  (interpreted:value x name))

(defword immediate:to (&parse name)
  (emit-literal (word-body name))
  (emit-word "!"))

(defword immediate:does> ()
  (emit-word "(does>)"))

(defword interpreted:code (&parse name)
  ( ((mangled (format nil "~A_code" (mangle-word name)))
	(special-code-p nil))
    (output-finish)
    (cond
      ((equal (read-word) "\\")
       ( ((ret-type (read-word)))
	 (setq special-code-p t
	       mangled (read-word))
	 (output "~A ~A ~A" ret-type mangled (read-line *input*))))
      (t
       (read-line *input*)
       (output "xt_t * REGPARM ~A (xt_t *IP, xt_t word)" mangled)))
    (output-line "{")
    ( ((line (read-line *input*) (read-line *input*)))
	((equalp (string-trim " " line) "end-code"))
      (output-line line))
    ( special-code-p
      (output-line "    return IP;"))
    (output-line "}")
    (output-header name (format nil "(code_t *)~A" mangled) "0" nil)))

;;;definterpreted end-code

(defword interpreted:allot (u)
  ( repeat (ceiling u *cell-size*)
         (output "  (cell)0,")))

(defword interpreted:|,| (x)
  (output "  (cell)~A," x))

(defword interpreted:|'| (&parse name)
  (tick name))

(defword interpreted:cells (u)
  (* *cell-size* u))

(defword interpreted:create (&parse name)
  (output-header name "dodoes_code" (word-body "noop" 0)))

(defword immediate:|(| ()
  ( ()
      ((eql (read-char *input*) #\)))))

(defword immediate:\\ ()
  ( ()
      ((eql (read-char *input*) #\Newline))))

(defword immediate:literal (x)
  (emit-literal x))

(defword immediate:compile (&parse name)
  (emit-literal (tick name))
  (emit-word ","))

(defword immediate:[compile] (&parse name)
  (emit-word name))

(defword immediate:postpone (&parse name)
  ( (immediate-word name)
      (immediate:[compile] name)
      (immediate:compile name)))

(defword immediate:|'DODOES| ()
  (emit-literal (format nil "~A_code" (mangle-word "dodoes"))))

(defword interpreted:cr ()
  (terpri))

(defword interpreted:|.(| ()
  (format t ".( ~A )" (read-word #\))))

(defword immediate:|S"| ()
  ( ((string (coerce (read-word #\") 'string)))
    (emit-literal (concatenate 'string "\"" (quoted string) "\""))
    (emit-literal (length string))))

(defword immediate:|."| ()
  (immediate:|S"|)
  (emit-word "type"))

(defword immediate:ahead ()
  (emit-branch "branch" :unresolved))

(defword immediate:()
  (emit-branch "0branch" :unresolved))

(defword immediate:then ()
  (resolve-branch))

(defword immediate: ()
  (immediate:ahead)
  (cs-roll 1)
  (immediate:then))

(defword immediate:|ABORT"| ()
  (immediate:if)
  (immediate:|S"|)
  (emit-word "cr")
  (emit-word "type")
  (emit-word "cr")
  (emit-word "abort")
  (immediate:then))

(defword interpreted:here ()
  *here*)

( *leave*)

(defword immediate: ()
  (emit-word "2>r")
  (setq *leave* nil)
  (interpreted:here))

(defword immediate:leave ()
  (immediate:ahead)
  ( (pop *control-stack*) *leave*)
  (values))

(defword immediate: ()
  (emit-literal "1")
  (emit- "(+ )"))

(defword immediate:+ ()
  (emit- "(+ )"))

(defword immediate:begin ()
  (interpreted:here))

(defword immediate:again (x)
  (emit-branch "branch" x))

(defword immediate:while ()
  (immediate:)
  (cs-roll 1))

(defword immediate:repeat (x)
  (immediate:again x)
  (immediate:then))

(defword immediate:until (x)
  (emit-branch "0branch" x))

(defword interpreted:+ (n1 n2)
  (+ n1 n2))

(defword interpreted:- (n1 n2)
  (- n1 n2))

(defword interpreted:1+ (n)
  (1+ n))

(defword interpreted:* (n1 n2)
  (* n1 n2))

(defword interpreted:lshift (n1 n2)
  (ash n1 n2))

(defword interpreted:char (&parse )
  (char-code (char 0)))

(defword immediate:[char] (&parse )
  ( (( (char 0)))
    (emit-literal (cond
		    ((char= char #\') "'\\''")
		    ((char= char #\\) "'\\\\'")
		    (t (format nil "'~A'" char))))))

(defword immediate:|[']| (&parse )
  (emit-literal (tick )))

(defword interpreted:variable (&parse name)
  (output-header  "dodoes_code" (word-body "noop" 0))
  (output-line "  0"))

(defword interpreted:cell ()
  *cell-size*)

(defword immediate:cell ()
  (emit-literal *cell-size*))

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

(defword interpreted:invert (u)
  (mask-word (lognot u)))

(defword interpreted:rshift (n u)
  ( n (- u)))

(defword interpreted:= (n1 n2)
  ( (=  n1 n2) -1 0))

(defword interpreted:> (n1 n2)
  ( (> n1 n2) -1 0))

(defword immediate:[defined] (&parse name)
  ( (word-found-p name *vocabulary*) -1 0))

(defword immediate:[undefined] (&parse name)
  (interpreted:invert (immediate:[defined] name)))

(defword interpreted:  (&parse )
  (interpret-file ))
      
(defword immediate:[] (n)
  ( (zerop n)
    (skip-until "[]" "[]")))

(defword immediate:[] ()
  (skip-until "[]"))

(defword immediate:[then] ()
  nil)

;;; Print control stack.
(defword immediate:.cs ()
  ( *trace-output* "<~D> " (length *control-stack*))
  ( (x (reverse *control-stack*))
    ( *trace-output* "~A " x)))
