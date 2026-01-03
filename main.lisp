(in-package :srl)

                                        ; Questions

;; Does importing the linear models through py4cl cause a loss of precision?
;; If so, does this affect the performance?

;; Do we need to include negative unit circle angles?

;; What level of depth should we go down to? 5?

;; Do we need recursive CONDs with logical operators?

;; How many models does the best decision tree need to solve HalfCheetah?

                                        ; TODO

;; Run a decision tree in ga.py and see how many models it takes for an acceptable score

;; Import a regression dataset and a classification dataset and place it in datasets/HalfCheetah-v5-Classification
;; and datasets/HalfCheetah-v5-Regression



                                        ; HalfCheetah-v5 State Labels

(defparameter *HalfCheetah-Labels*
  '(((OBS 0)  . root-z)          
    ((OBS 1)  . root-θ)          
    ((OBS 2)  . b-thigh-θ)
    ((OBS 3)  . b-shin-θ)
    ((OBS 4)  . b-foot-θ)
    ((OBS 5)  . f-thigh-θ)
    ((OBS 6)  . f-shin-θ)
    ((OBS 7)  . f-foot-θ)
    ((OBS 8)  . root-vx)         
    ((OBS 9)  . root-vz)         
    ((OBS 10) . root-ω)          
    ((OBS 11) . b-thigh-ω)
    ((OBS 12) . b-shin-ω)
    ((OBS 13) . b-foot-ω)
    ((OBS 14) . f-thigh-ω)
    ((OBS 15) . f-shin-ω)))
                                        ; HalfCheetah-v5 Constants

(defparameter *torso-length* 1.0)
(defparameter *head-length* 0.3)
(defparameter *b-thigh-length* 0.29)
(defparameter *b-shin-length* 0.3)
(defparameter *b-foot-length* 0.188)
(defparameter *f-thigh-length* 0.266)
(defparameter *f-shin-length* 0.212)
(defparameter *f-foot-length* 0.14)
(defparameter *dt* 0.05)

(defconstant *PI/6* (/ pi 6.0))           ; 30 deg
(defconstant *PI/4* (/ pi 4.0))           ; 45 deg
(defconstant *PI/3* (/ pi 3.0))           ; 60 deg
(defconstant *PI/2* (/ pi 2.0))           ; 90 deg
(defconstant *2PI/3* (* 2 (/ pi 3.0)))    ; 120 deg
(defconstant *3PI/4* (* 3 (/ pi 4.0)))    ; 135 deg
(defconstant *5PI/6* (* 5 (/ pi 6.0)))    ; 150 deg
(defconstant *PI* pi)                     ; 180 deg
(defconstant *7PI/6* (* 7 (/ pi 6.0)))    ; 210 deg
(defconstant *5PI/4* (* 5 (/ pi 4.0)))    ; 225 deg
(defconstant *4PI/3* (* 4 (/ pi 3.0)))    ; 240 deg
(defconstant *3PI/2* (* 3 (/ pi 2.0)))    ; 270 deg
(defconstant *5PI/3* (* 5 (/ pi 3.0)))    ; 300 deg
(defconstant *7PI/4* (* 7 (/ pi 4.0)))    ; 315 deg
(defconstant *11PI/6* (* 11 (/ pi 6.0)))  ; 330 deg
(defconstant *2PI* (* 2 pi))              ; 360 deg

                                        ; HalfCheetah-v5 Grammar

(defparameter *HalfCheetah-Grammar*
  '((<ROOT>             (IF <COND> <MODEL> <ROOT>)
                        <MODEL>)
    (<COMPARE>          < >)
    (<COND>             (<COMPARE> <POS-EXPR> <POS-EXPR>)
                        (<COMPARE> <VEL-EXPR> <VEL-EXPR>)
                        (<COMPARE> <ANGLE-EXPR> <ANGLE-EXPR>)
                        (<COMPARE> <POS-EXPR> <CONSTANT>)  
                        (<COMPARE> <VEL-EXPR> <CONSTANT>)
                        (<COMPARE> <ANGLE-EXPR> <CONSTANT>))
    (<POS-EXPR>         <POSITION>
                        (+ <POSITION> (* <VELOCITY> *DT*))
                        (* <LENGTH> (sin <ANGLE-EXPR>))      
                        (* <LENGTH> (cos <ANGLE-EXPR>)))     
    (<VEL-EXPR>         <VELOCITY>
                        (abs <VELOCITY>)                     
                        (* <ANGULAR-VELOCITY> <LENGTH>))
    (<ANGLE-EXPR>       <ANGLE>
                        (- <ANGLE> <ANGLE>)                 
                        (+ <ANGLE> (* <ANGULAR-VELOCITY> *DT*)))
    (<POSITION>         (OBS 0))
    (<ANGLE>            (OBS 1) (OBS 2) (OBS 3) (OBS 4) (OBS 5) (OBS 6) (OBS 7))
    (<VELOCITY>         (OBS 8) (OBS 9))
    (<ANGULAR-VELOCITY> (OBS 10) (OBS 11) (OBS 12) (OBS 13) (OBS 14) (OBS 15) (OBS 16))
    (<MODEL>            (USE-MODEL 0) (USE-MODEL 1) (USE-MODEL 2) (USE-MODEL 3) (USE-MODEL 4) (USE-MODEL 5) (USE-MODEL 6)
                        (USE-MODEL 7) (USE-MODEL 8) (USE-MODEL 9) (USE-MODEL 10) (USE-MODEL 11) (USE-MODEL 12)
                        (USE-MODEL 13) (USE-MODEL 14) (USE-MODEL 15) (USE-MODEL 16) (USE-MODEL 17) (USE-MODEL 18) (USE-MODEL 19))
    (<LENGTH>           *torso-length* *head-length* *b-thigh-length* *b-shin-length* *b-foot-length*
     *f-thigh-length*   *f-shin-length* *f-foot-length*)
    (<CONSTANT>         0.0 1.0 0.1 10.0 5.0 0.5 *PI/6* *PI/4* *PI/3* *PI/2* *2PI/3* *3PI/4*
                        *5PI/6* *PI* *7PI/6* *5PI/4* *4PI/3* *3PI/2* *5PI/3*
                        *7PI/4* *11PI/6* *2PI*)))

                                        ; HalfCheetah Genome (Depth 5)

(defparameter *HalfCheetah-Genome*
  '((<ROOT> . 5)
    (<COMPARE> . 18)
    (<COND> . 4)
    (<POS-EXPR> . 9)
    (<VEL-EXPR> . 9)
    (<ANGLE-EXPR> . 9)
    (<POSITION> . 12)
    (<ANGLE> . 24)
    (<VELOCITY> . 12)
    (<ANGULAR-VELOCITY> . 6)
    (<MODEL> . 8)
    (<LENGTH> . 18)
    (<CONSTANT> . 9)))
                                        ; Structured Grammatical Evolution

(defstruct individual
  genotype
  fitness)

(<ROOT> . #())
(<COMPARE> . #())
(<COND> . #())
(<POS-EXPR> . #())
(<VEL-EXPR> . #())
(<ANGLE-EXPR> . #())
(<POSITION> . #())
(<ANGLE> . #())
(<VELOCITY> . #())
(<ANGULAR-VELOCITY> . #())
(<MODEL> . #())
(<LENGTH> . #())
(<DT> . #())
(<CONSTANT> . #())


(defun length-of-rule-choices (rule grammar)
  (length (cdr (assoc rule grammar))))

(defun make-sge-genotype (genome)
  (loop for rule in genome
        for lhs = (car rule)
        for rhs = (cdr rule)
        collect (cons lhs (coerce (loop repeat rhs
                                        collect (random (length-of-rule-choices lhs *halfcheetah-grammar*)))
                                  'vector))))

;; A simple tracker: ((<ROOT> . 0) (<COND> . 0) ...)
(defun make-pointers (genotype)
  (mapcar (lambda (entry) (cons (car entry) 0)) genotype))  

(defun sge-decode (symbol genotype pointers)
  (let ((rule (assoc symbol *HalfCheetah-Grammar*)))
    (cond 
      ;; 1. If it's a list, we must map sge-decode over every element
      ((listp symbol) 
       (mapcar (lambda (s) (sge-decode s genotype pointers)) symbol))

      ;; 2. If it's a Non-Terminal in our grammar, pull from the genotype
      (rule 
       (let* ((drawer (cdr (assoc symbol genotype)))
              (ptr-entry (assoc symbol pointers))
              (current-ptr (cdr ptr-entry))
              (choice-index (aref drawer current-ptr)))
         
         ;; Increment the pointer
         (incf (cdr ptr-entry))
         
         ;; Recurse on the expansion
         (let ((expansion (elt (cdr rule) choice-index)))
           (sge-decode expansion genotype pointers))))

      ;; 3. Otherwise, it's a Terminal (like *DT*, *PI*, or (OBS 5))
      (t symbol))))

                   
(defun get-sge-blueprint (grammar max-depth)
  "Calculates the maximum possible occurrences of each non-terminal."
  (let ((blueprint (make-hash-table)))
    (labels ((count-max (symbol depth)
               (let ((rule (assoc symbol grammar)))
                 (when (and rule (> depth 0))
                   ;; Record that we found an occurrence of this symbol
                   (incf (gethash symbol blueprint 0))
                   ;; For SGE, we assume the user might pick the 'heaviest' path.
                   ;; We look at all choices and recursively find the one that 
                   ;; uses the MOST of each sub-symbol.
                   (dolist (derivation (cdr rule))
                     (if (listp derivation)
                         (dolist (item derivation) (count-max item (1- depth)))
                         (count-max derivation (1- depth))))))))
      (count-max '<ROOT> max-depth)
      ;; Convert to a simple alist
      (let ((result nil))
        (maphash (lambda (k v) (push (cons k v) result)) blueprint)
        result))))
;; everything in the list 


(defun explain (genotype)
  "Prints an individual's GENOME with human-readable labels."
  (pprint (sublis *halfcheetah-labels* genotype :test 'equal)))

(defun assign-fitness! (individual &key (mode :online) (seed 24012000))
  (case mode
    (:online (setf (individual-fitness individual) (rollout "HalfCheetah-v5" individual seed)))))

(defun tournament-selection (population tournament-size)
  "Randomly select TOURNAMENT-SIZE individuals with replacement from the population.
   Return the best individual in the tournament."
  (let ((tournament (loop repeat tournament-size
                          collect (random-choice population))))
    (setf tournament (sort tournament #'> :key #'individual-fitness))
    (first tournament)))

(defun evolve (pop-size budget)
  (let ((population (loop repeat pop-size
                          collect (make-individual))))
    (loop repeat budget
          for generation from 1
          do (format t "Generation ~A/~A" generation budget)
             ;; assign fitness
          do (loop for individual in population
                   do (setf (individual-fitness individual) (fitness individual)))
             ;; selection
          do (let ((new-population (loop repeat pop-size
                                         collect (tournament-selection population 3))))
               (setf population new-population))
             ;; variation
          do (dolist (ind population)
               ;; mutate
               (when (coin-flip 0.80)
                 (mutate ind)))
             ;; summary
          do (format t " Best Fitness ~,3F Average Fitness ~,3F~%"
                     (reduce #'max (loop for ind in populationq
                                         collect (individual-fitness ind)))
                     (/ (reduce #'+ (loop for ind in population
                                          collect (individual-fitness ind))) pop-size)))
    population))

                                        ; Bag of Linear Models

(defun load-bag (bag-path)
  (py4cl:import-module "pickle")
  (py4cl:python-eval 
   (format nil "pickle.load(open(~S, 'rb'))" bag-path)))

(defun append-bias-term (x)
  (np:append 1.0 x))

(defun use-model (n)
  (let ((linear-model (aref *bag* n))
        (input (append-bias-term *OBS*)))
    (np:matmul input linear-model)))

(defparameter *bag* (load-bag "/Users/brycemacinnis/Repos/gecco26/linear_models/HalfCheetah"))

(defvar *OBS*)

(defun OBS (n)
  (when *OBS*
    (aref *OBS* n)))

                                        ; Offline Evaluation

;(defun eval-on-dataset (X y)
  

                                        ; Gymnasium Rollouts 

(defun rollout (env-name individual &optional (seed 24012000))
  "Evaluates an INDIVIDUAL on a Gymnasium environment ENV-NAME on SEED."
  (let* ((env (gym:make env-name))
         (reward 0.0))
    (setf *OBS* (first (py4cl:chain env (reset :seed seed))))
    (loop repeat 1000
          do (let ((action (eval (individual-genotype individual))))
               (destructuring-bind (obs rew term trunc info)
                   (py4cl:chain env (step action))
                 (declare (ignore info))
                 (setf *OBS* obs)
                 (incf reward rew)
                 (when (or term trunc)
                   (return)))))
    (py4cl:chain env close)
    reward))

