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
	  (B s_3)))
	(s_2
	 ((B s_3)
	  (j s_2)
	  (A s_1)))
	(s_3
	 ((A s_2)
	  (B s_1)))))


(defun fsmcheck (state stream)
 "test: (fsmcheck-1 's_init '(A C B)) ==> t s_3"
 (if (null stream) 
     (values t state)
   ;; Input available.
   (let* ((transitions (second (assoc state *fsm*)))
		  (input (pop stream))
		  (transition (assoc input transitions)))
     (when transition
	   (let ((new-state (second transition)))
		 (fsmcheck-1 new-state stream))))))


;;------------------- solution 2. compiler approach
(defun fsmcheck2 (state_init input)
 (let ((current-state state_init))
   (loop
	  (let ((current-input (pop input)))
		(when (null current-input)
		  (return (values t current-state)))
	
		(setf current-state
			  (case current-state
				(s_init
				 (case current-input
				   (A 's_1)))
				(s_1
				 (case current-input
				   (i 's_1)
				   (C 's_2)
				   (D 's_2)
				   (B 's_3)))
				(s_2
				 (case current-input
				   (B 's_3)
				   (j 's_2)
				   (A 's_1)))
				(s_3
				 (case current-input
				   (A 's_2)
				   (B 's_1)))))
	
		(when (null current-state)
		  ;; Input does not match FSM
		  (return nil))))))

(defun this-one-stackoverflow (stream)
  (labels
	  ((s_init (stream)
  (if (null stream)
      t
    (case (first stream) (A (s_1 (rest stream))) (otherwise nil))))
 (s_1 (stream)
  (if (null stream)
      t
    (case (first stream)
      (i (s_1 (rest stream)))
      (C (s_2 (rest stream)))
      (D (s_2 (rest stream)))
      (B (s_3 (rest stream)))
      (otherwise nil))))
 (s_2 (stream)
  (if (null stream)
      t
    (case (first stream)
      (B (s_3 (rest stream)))
      (j (s_2 (rest stream)))
      (A (s_1 (rest stream)))
      (otherwise nil))))
 (s_3 (stream)
  (if (null stream)
      t
    (case (first stream)
      (A (s_2 (rest stream)))
      (B (s_1 (rest stream)))
      (otherwise nil)))))
	(s_init stream)))

;;------------------- solution 3. macro approach


;;------------------- benchmark --------------
(defun fsm-resample (fsm state_initial N)
  "(time (fsmcheck 's_init (mapcar #'car (fsm-resample *fsm* 's_init 1000000))))"
	(loop repeat N
	   for state =  state_initial then (second transition)
	   for transition = (alexandria:random-elt (second (assoc state fsm)))
		 collect transition))
