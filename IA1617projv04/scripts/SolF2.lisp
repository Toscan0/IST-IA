;;al021

(load "datastructures.fas")
(load "auxfuncs.fas")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1
(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))


;;----------------------------------------------------------;;
;;--------------------Solution of phase 2-------------------;;

;;--Variaveis Globais--;;
(defparameter lstSol (list)) ;lista que contem os estados solucao

;;funcao aux
(defun procuraAux (node problem lim)
  (let ((corte)
    (res))
  (setf corte 1)
  (cond 
    ;Se o no for solucao
    ((funcall (problem-fn-isGoal problem) (node-state node)) 
      (return-from procuraAux (node-state node)))
    ;Ocorreu um corte
    ((equal lim 0) 
      (return-from procuraAux 0)))
  
  ;x in uma lista com todos os nexstates do estado de um no
  (loop for x in (funcall (problem-fn-nextStates problem) (node-state node)) do
    ;procuraAux para cada filho do respetivo no
    (setf res (procuraAux (make-node :state x) problem (- lim 1)))
    
    (cond
      ;se o res for 1 -> corte
      ((equal res 0) (setf corte 0))

      ;Se nao ocrreu corte/failure ent esse no e uma solucao
      ;adicionamos a lstSol
      ((not (equal res 2))                
        (setf lstSol (append (list x) lstSol))
        (return-from procuraAux res))))

  (if(eq corte 0) (return-from procuraAux 0)
     (return-from procuraAux 1))))


;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"

  (let ((i 0)
    (lst)
    (pos)
    (newstate))
  (setf lst (list))
  (setf pos (possible-actions))

  ; 9 numero de jogadas possiveis
  (loop while(<= i 8) do 
    (setf newstate (nextState st (nth i pos)))
    (setf lst (append (list newstate) lst))
    (setf i (+ i 1))) 
  lst))


;;; limdepthfirstsearch -> Procura em Profundidade Limitada (PPL)
(defun limdepthfirstsearch (problem lim)
  "limited depth first search"
  (let ((res))
  (setf lstSol (list))
  (setf res (procuraAux (make-node :state (problem-initial-state problem)) problem lim ))
  (setf lstSol (append (list (problem-initial-state problem)) lstSol))
  
  (cond
    ;Se ocorreu um corte
    ((eq res 0) (return-from limdepthfirstsearch :corte))
    ;Se ocorreu um failure
    ((eq res 1) (return-from limdepthfirstsearch nil) )
    ;else{ //se for encontrada uma solucao devolve a lista
  (t lstSol))))


;iterlimdepthfirstsearch -> Procura em profundidade iterativa (A*)
(defun iterlimdepthfirstsearch (problem)
  "limited depth first search"

  (let ((lim)
    (node)
    (result))
  (setf lim 0)
  (setf lstSol (list))
  (setf node (make-node :state (problem-initial-state problem)))
  
  (loop 
    ;faz a procuraAux ao primeiro no
    (setf result (procuraAux node problem lim))
    (cond
      ((not (equal result 0))
        ;se nao houver um corte entao a lstSol esta atualizada 
        ;;Entao juntamos estado inicial
        (setf lstSol (append (list (problem-initial-state problem)) lstSol))
        (return-from iterlimdepthfirstsearch lstSol)))
    (incf lim))
  nil))