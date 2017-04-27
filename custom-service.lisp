;;------- setcion 0: initialization
(in-package :db.agraph.http.backend)
(defmacro db.agraph:custom-service (methods permissions name return-type arguments &body body)
  (check-type name string)
  (check-type permissions string)
  (check-type return-type keyword)
  `(service ,permissions ,methods store ("custom" ,name) ,arguments
            (with-query-environment ()
              ,(if (eq return-type :dynamic)
                   `(multiple-value-call 'output (guess-output-type (progn ,@body)))
                   `(output ,return-type (progn ,@body))))))



(in-package :db.agraph.user)
(register-namespace "kuijia" "http://example.com/kuijia#")
(register-namespace "xsd" "http://www.w3.org/2001/XMLSchema#")
(enable-!-reader)  
(enable-print-decoded t)

;;;--- this is a test case to update a global parameter.
(defparameter *HH* 100)
(db.agraph.user::custom-service
 :get "r" "test" :string ()
 *HH*)

;; $session/test -> 100
(db.agraph.user::custom-service
 :post "w" "test-update" :string ((x :integer 99))
 (setf *HH* x))

;; $session/test-update -d 'x=10' -X POST
;; $session/test -> 10

;;;-------------- App1: fuzzy match
(load "leventrie.lisp")
(db.agraph.user::custom-service
  :post "r" "edit-distance" :integer ((s1  :string) (s2 :string))
  (levenschtein s1 s2))
