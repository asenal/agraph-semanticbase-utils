#|
;;;---- thanks to Shriram's work: http://www.ai.mit.edu/projects/dynlangs/ll1/shriram-talk.pdf
;;;---- this is a CL implementation of his scheme work
|#

;;------------------- solution 1. interpreter approach
(defparameter *fsm*
  '((s_init
	 ((A s_1)))
	(s_1
	 ((i s_1)
	  (C s_2)
	  (D s_2)
	  (B s_3)
	  (A s_end)))
	(s_2
	 ((B s_3)
	  (j s_2)
	  (A s_1)))
	(s_3
	 ((A s_2)
	  (B s_1)))))

(defun fsmcheck (state stream)
  "test: (fsmcheck 's_init '(A C M))"
  (if (null stream) t
	  (let ((new-state (cadr (assoc (first stream) (cadr (assoc state *fsm*))))))
		 (and new-state (fsmcheck new-state (rest stream))))))

;;------------------- solution 2. compiler approach
(defun fsmcheck2 (observe)
	(labels ((s_init (stream)
			   (if (null stream) t
				   (case (first stream)
					 (A (s_1 (rest stream)))
					 (otherwise nil))))
			 (s_1 (stream)
			   (if (null stream) t
				   (case (first stream)
					 (i (s_1 (rest stream)))
					 (A (s_2 (rest stream)))
					 (B (s_init (rest stream)))
					 (otherwise nil))))
			 (s_2 (stream)
			   (if (null stream) t
				   (case (first stream)
					 (A (s_1 (rest stream)))
					 (i (s_2 (rest stream)))
					 (B (s_init (rest stream)))
					 (otherwise nil)))))
	  (s_init observe)))
  
;;------------------- solution 3. macro approach
; -- this one expands transitions rules: (loop for (state transitions) in *fsm* collect `(,state (stream) (if (null stream) t (case (first stream) ,@(loop for (x y) in transitions collect `(,x (,y (rest stream)))) (otherwise nil)))))
(defmacro fsm (machine-plist)
  `(labels
	  ,(loop for (state transitions) in machine-plist
		  collect (state (stream)
						 (if (null stream)
							 t
							 (case (first stream)
							   (loop for (x y) in transitions collect `(,x (,y (rest stream))))
							   (otherwise nil)))))))
  
