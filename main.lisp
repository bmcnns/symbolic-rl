(in-package :srl)

(defun load-bag (bag-path)
  (py4cl:python-eval 
   (format nil "pickle.load(open(~S, 'rb'))" bag-path)))
               
(defparameter *bag* (load-bag "/Users/brycemacinnis/Repos/gecco26/linear_models/HalfCheetah"))

(defun append-bias-term (x)
  (np:append 1.0 x))

(defun use-model (n)
  (let ((linear-model (aref *bag* n))
        (input (append-bias-term *OBS*)))
    (np:matmul input linear-model)))

(defun rollout (env-name individual &optional (seed 24012000))
  (py4cl:import-module "gymnasium" :as "gym")
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

(defvar *OBS*)

(defun OBS (n)
  (when *OBS*
    (aref *OBS* n)))

(defparameter *HalfCheetah-Labels*
  '(((OBS 0)  . root-z)          ; Height
    ((OBS 1)  . root-θ)          ; Torso Pitch
    ((OBS 2)  . b-thigh-θ)
    ((OBS 3)  . b-shin-θ)
    ((OBS 4)  . b-foot-θ)
    ((OBS 5)  . f-thigh-θ)
    ((OBS 6)  . f-shin-θ)
    ((OBS 7)  . f-foot-θ)
    ((OBS 8)  . root-vx)         ; Forward velocity
    ((OBS 9)  . root-vz)         ; Vertical velocity
    ((OBS 10) . root-ω)          ; Torso angular velocity
    ((OBS 11) . b-thigh-ω)
    ((OBS 12) . b-shin-ω)
    ((OBS 13) . b-foot-ω)
    ((OBS 14) . f-thigh-ω)
    ((OBS 15) . f-shin-ω)
    ((OBS 16) . f-foot-ω)))

(defparameter *HalfCheetah-Grammar*
  '((<ROOT>           (IF <COND> <MODEL> <ROOT>)
                      <MODEL>)
    (<COMPARE>        < >)
    (<COND>           (<COMPARE> <POS-EXPR> <POS-EXPR>)
                      (<COMPARE> <VEL-EXPR> <VEL-EXPR>)
                      (<COMPARE> <ANGLE-EXPR> <ANGLE-EXPR>)
                      (<COMPARE> <POS-EXPR> <CONSTANT>)   ; Added: Compare to Numbers
                      (<COMPARE> <VEL-EXPR> <CONSTANT>)
                      (<COMPARE> <ANGLE-EXPR> <CONSTANT>))
    (<POS-EXPR>       <POSITION>
                      (+ <POSITION> (* <VELOCITY> <DT>))
                      (* <LENGTH> (sin <ANGLE-EXPR>))      ; Height
                      (* <LENGTH> (cos <ANGLE-EXPR>)))     ; Added: Reach
    (<VEL-EXPR>       <VELOCITY>
                      (abs <VELOCITY>)                     ; Added: Speed magnitude
                      (* <ANGULAR-VELOCITY> <LENGTH>))
    (<ANGLE-EXPR>     <ANGLE>
                      (- <ANGLE> <ANGLE>)                 ; Relative Pose
                      (+ <ANGLE> (* <ANGULAR-VELOCITY> <DT>)))
    (<POSITION>       (OBS 0))
    (<ANGLE>          (OBS 1) (OBS 2) (OBS 3) (OBS 4) (OBS 5) (OBS 6) (OBS 7))
    (<VELOCITY>       (OBS 8) (OBS 9))
    (<ANGULAR-VELOCITY> (OBS 10) (OBS 11) (OBS 12) (OBS 13) (OBS 14) (OBS 15) (OBS 16))
    (<MODEL>          (USE-MODEL 0) (USE-MODEL 1) (USE-MODEL 2) (USE-MODEL 3) (USE-MODEL 4) (USE-MODEL 5) (USE-MODEL 6)
                      (USE-MODEL 7) (USE-MODEL 8) (USE-MODEL 9) (USE-MODEL 10) (USE-MODEL 11) (USE-MODEL 12)
                      (USE-MODEL 13) (USE-MODEL 14) (USE-MODEL 15) (USE-MODEL 16) (USE-MODEL 17) (USE-MODEL 18) (USE-MODEL 19))
    (<LENGTH>         1.0 0.3 0.29 0.188 0.266 0.212 0.14)
    (<DT>             0.05)
    (<CONSTANT>       0.0 0.1 0.5 1.0 -1.0 0.785 1.57))) ; 0, 45 deg, 90 deg, etc.

(defun random-choice (seq)
  (elt seq (random (length seq))))

(defun coin-flip (p)
  (if (< (random 1.0) p)
      t
      nil))

(defun random-derivation (rule)
  (random-choice (cdr rule)))

(defun replace-derivation (derivation)
  (random-choice (remove derivation (cdr (find-if (lambda (rule) (member derivation rule)) *halfcheetah-grammar*)))))

(defun find-rule (rule)
  (find rule *halfcheetah-grammar* :key #'car))

(defun terminal-p (node)
  ;; something is a terminal if it is an atom (but not in the rule list)
  (or (and (atom node) (not (assoc node *halfcheetah-grammar*)))
      ;; and if it's a list beginning with OBS or USE-MODEL
      (and (listp node) (member (car node) '(USE-MODEL OBS)))))
      
(defun fill-in (expr)
  (cond ((find-rule expr)
         (let ((derivation (random-derivation (find-rule expr))))
           (if (listp derivation)
               (mapcar #'fill-in derivation)
               (fill-in derivation))))
        ((terminal-p expr) expr)
        (t (mapcar #'fill-in expr))))

(defun explain (individual)
  (pprint (sublis *halfcheetah-labels* (individual-genotype individual) :test 'equal)))
  
(defun fitness (individual &key (mode :online) (seed 24012000))
  (case mode
    (:online (rollout "HalfCheetah-v5" individual seed))))

(defstruct individual
  (genotype (fill-in '<ROOT>))
  fitness)

(defun tournament-selection (population tournament-size)
  (let ((tournament (loop repeat tournament-size
                          collect (random-choice population))))
    (setf tournament (sort tournament #'> :key #'individual-fitness))
    (first tournament)))

(defun count-expr (expr)
  (if (not (terminal-p expr))
      (reduce #'+ (mapcar #'count-expr expr))
      1))

(defun mutate (individual)
  nil)

(defun evolve (pop-size budget)
  (let ((population (loop repeat pop-size
                          collect (make-individual))))
    (loop repeat budget
          for generation from 1
          do (format t "Generation ~A/~A~%" generation budget)
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
                   (mutate ind))))
    population))
