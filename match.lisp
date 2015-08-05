(defvar *variables* nil)

(defvar *single-matchies*
  '((:is gen-match-is)
    (:or gen-match-or)
    (:and gen-match-and)
    (:not gen-match-not)))

(defvar *segment-matchies*
  '((:* gen-segment-match)
    (:+ gen-segment-match+)
    (:? gen-segment-match?)
    (:if gen-match-if)))

(defmacro if-match (pattern input then &optional else)
  (let ((gelse (gensym "ELSE"))
	(ginput (gensym))
	*variables*)
    `(labels ((,gelse () ,else))
       (let ((,ginput ,input))
	 ,(gen-match pattern
		     ginput
		     (lambda () then)
		     (lambda () `(,gelse)))))))

(defun variable-p (pat)
  (and (symbolp pat)
       (let ((name (symbol-name pat)))
         (and (< 0 (length name))
              (char= #\? (aref name 0))))))

(defun gen-match-var (pattern input then else)
  (if (member pattern *variables*)
      `(if (equal ,pattern ,input)
           ,(funcall then)
           ,(funcall else))
      (progn
        (let ((*variables* (cons pattern *variables*)))
          `(let ((,pattern ,input))
             ,(funcall then))))))

(defun gen-match (pattern input then else)
  (let (result)
    (cond
      ((variable-p pattern)
       (gen-match-var pattern input then else))
      ((atom pattern)
       `(if (eql ',pattern ,input)
	    ,(funcall then)
	    ,(funcall else)))
      ((setq result (single-pattern-p pattern))
       (funcall (cadr result) (cdr pattern) input then else))
      ((setq result (segment-pattern-p pattern))
       (funcall (cadr result) pattern input then else))
      (t
       (let ((gcar (gensym))
	     (gcdr (gensym)))

	 `(if (consp ,input)
	      (let ((,gcar (car ,input))
		    (,gcdr (cdr ,input)))
		,(gen-match (car pattern)
			    gcar
			    (lambda ()
			      (gen-match (cdr pattern)
					 gcdr
					 then
					 else))
			    else))
	      ,(funcall else)))))))

(defun single-pattern-p (pattern)
  (and (consp pattern)
       (assoc (car pattern)
              *single-matchies*)))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
       (consp (car pattern))
       (assoc (caar pattern) *segment-matchies*)))

(defun gen-match-is (pattern input then else)
  (destructuring-bind (var pred) pattern
    `(if (,pred ,input)
         ,(gen-match-var var input then else)
         ,(funcall else))))

(defun gen-match-or (pattern input then else)
  (if (null pattern)
      (funcall else)
      (gen-match (car pattern)
		 input
		 then
		 (lambda ()
		   (gen-match-or (cdr pattern) input then else)))))

(defun gen-match-and (pattern input then else)
  (if (null pattern)
      (funcall then)
      (gen-match (car pattern)
		 input
		 (lambda ()
		   (gen-match-and (cdr pattern) input then else))
		 else)))

(defun gen-match-not (pattern input then else)
  `(if ,(gen-match pattern
		   input
		   (lambda () nil)
		   (lambda () t))
       ,(funcall then)
       ,(funcall else)))

(defun gen-nthcdr (n x)
  (case n
    (0 x)
    (1 `(cdr ,x))
    (2 `(cddr ,x))
    (3 `(cdddr ,x))
    (4 `(cddddr ,x))
    (otherwise `(nthcdr ,n ,x))))

(defun gen-segment-match (pattern input then else &optional (start 0))
  (let ((var (cadar pattern))
        (pat (cdr pattern)))
    (if (null pat)
        (gen-match-var var input then else)
        (let ((gpat (gensym))
              (gindex (gensym))
              (gblock (gensym)))
          `(if (consp ,input)
               (block ,gblock
                 (do ((,gpat ,(gen-nthcdr start input) (cdr ,gpat))
                      (,gindex ,start (1+ ,gindex)))
                     ((null ,gpat) ,(funcall else))
                   ,(gen-match pat gpat
                               (lambda ()
                                 (let ((g (gensym)))
                                   `(let ((,g (subseq ,input 0 ,gindex)))
                                      ,(gen-match-var
                                        var
                                        g
                                        (lambda ()
                                          `(return-from ,gblock
                                             ,(funcall then)))
                                        else))))
                               (lambda ()))))
               ,(funcall else))))))

(defun gen-segment-match+ (pattern input then else)
  (gen-segment-match pattern input then else 1))

(defun gen-segment-match? (pattern input then else)
  (let ((var (cadar pattern))
        (pat (cdr pattern)))
    (gen-match (cons var pat)
               input
               then
               (lambda ()
                 (gen-match-var var nil then else)))))

(defun gen-match-if (pattern input then else)
  `(if ,(cadar pattern)
       ,(gen-match (cdr pattern) input then else)
       ,(funcall else)))
