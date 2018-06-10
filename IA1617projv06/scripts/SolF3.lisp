
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3 Entrega IA                     ;;;
;;;    Grupo 21:                     ;;;
;;;          - 81633 Joao Henriques  ;;;
;;;          - 82343 Pedro Cunha     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Submissao mosshak-> passar para .fas
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

;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"
  (let ((successors nil))
    (dolist (act (possible-actions) successors)
      (let ((new-state (nextState st act)))
  (if (not (member new-state successors :test #'equalp))
      (push new-state successors))))))

;;; Solucao e uma seq ordenada de estados
(defun solution (node)
  (let ((seq-states nil))
    (loop 
      (when (null node)
  (return))
      (push (node-state node) seq-states)
      (setf node (node-parent node)))
    (values seq-states)))


;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim &key cutoff?)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
  (labels ((limdepthfirstsearch-aux (node problem lim)
       (if (isGoalp (node-state node))
     (solution node)
     (if (zerop lim)
         :cutoff
         (let ((cutoff? nil))
           (dolist (new-state (nextStates (node-state node)))
       (let* ((new-node (make-node :parent node :state new-state))
        (res (limdepthfirstsearch-aux new-node problem (1- lim))))
         (if (eq res :cutoff)
             (setf cutoff? :cutoff)
             (if (not (null res))
           (return-from limdepthfirstsearch-aux res)))))
           (values cutoff?))))))
    (let ((res (limdepthfirstsearch-aux (make-node :parent nil :state (problem-initial-state problem))
          problem
          lim)))
      (if (eq res :cutoff)
    (if cutoff?
        :cutoff
        nil)
    res))))
              

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
  (let ((i 0))
    (loop
      (let ((res (limdepthfirstsearch problem i :cutoff? T)))
  (when (and res (not (eq res :cutoff)))
    (return res))
  (incf i)
  (if (> i lim)
      (return nil))))))
	


;; Solution of phase 3

;Recebe um node e devolve o :f desse mesmo node
(defun getNodeF (node)
  (return-from getNodeF (node-f node)))

;; Heuristic
(defun compute-heuristic (st)
  (let ((posicao (state-pos st)) ;posicao do estado atual 
    (posMeta (track-endpositions (state-track st))) ;posicao da meta 
    ;most-positive-fixnum maior numero que o lisp aceita( 2^5-1)
    (<M> most-positive-fixnum) ;distancia maxima
    (dist 0) ;distancia final entre a posicao e a meta
    (distx 0) ;distancia em x entre a posicao e a meta 
    (disty 0) ;distancia em y entre a posicao e a meta
    (posMetaAux ())) ;lista com a posicao da meta, para qual estamos a calcular a distancia
    
    ;se a posicao atual for um obstaculo
    (if(isObstaclep posicao (state-track st))
      (return-from compute-heuristic <M>))
    ;se a posicao atual for a meta, a distanica e 0
    (if(isGoalp st)
      (return-from compute-heuristic 0))
    
    ;vemos qual a menor distancia entre a posicao que estamos e as posicoes da meta
    (loop while(not (equal nil posMeta)) do
      ;vamos retirando as posicoes para as quais vamos calcular a distancia
      (setf posMetaAux (pop posMeta))
      ;vemos a distancia em x/y enhttp://stackoverflow.com/questions/15686162/scheme-cond-not-equaltre a posicao atual e a meta 
      (setf distx (abs (- (nth 0 posMetaAux) (nth 0 posicao))))
      (setf disty (abs (- (nth 1 posMetaAux) (nth 1 posicao))))
      ;ve qual a menor distancia
      (if(< distx disty)
          (setf dist disty) ;if
        (setf dist distx)))    ;else
    dist))

; A*
(defun a* (problem)
  "(list (make-node :state (problem-initial-state problem)))"
  (let((node)
    (nosNaoVis) ;lista com os nos ainda nao visitados
    (lst) ;lista com o resultado final
    (lstNextState) ;nextState do node
    (child))
    (setf lstNextState nil)
    (setf lst (list))
    ;como e o node inicial, o parent e nill e o g e 0
    (setf node 
      (make-node 
        :PARENT nil 
        :STATE (problem-initial-state problem) 
        :F (funcall (problem-fn-h problem) (problem-initial-state problem)) 
        :G 0 
        :H (funcall (problem-fn-h problem) (problem-initial-state problem))))
    (setf nosNaoVis (list node))
    
    (loop do
      ;se todos os nos ja tiverem sido vesitados
      (if(equal nil nosNaoVis)
        (return-from a* nil))
      ;o node pasaa a ser um no ainda nao visitado
      (setf node (pop nosNaoVis))
      ;se o node em que estamos e um no objetivo
      (if(funcall (problem-fn-isGoal problem) (node-state node))
        (progn
          (loop 
            (if(null node)
              (return-from a* lst))
            (push (node-state node) lst)
            (setf node (node-parent node)))))
      
      (dolist (lstNextState (funcall (problem-fn-nextStates problem) (node-state node)) nil)
        ;Como e o filho do node, o parent e o node, e o state e o estado onde estamos
        ;f = g+h
        (setf child
          (make-node
            :PARENT node 
            :STATE lstNextState 
            :F  (+ (funcall (problem-fn-h problem) lstNextState) (+(node-g node) (state-cost lstNextState)))
            :G  (+(node-g node) (state-cost lstNextState))
            :H  (funcall (problem-fn-h problem) lstNextState)))
        
        ;inser o child na lista com os nos que ainda n foram visitados
        (setf nosNaoVis (push child nosNaoVis)))
      
      ;ordenamos a lista usando a funcao sort 
      ;funcao existente em clisp ate a versao 2.49
      (setf nosNaoVis (sort nosNaoVis #'< :key #'getNodeF)))))

;best-search
(defun best-search (problem)
  "(list (make-node :state (problem-initial-state problem)))"
  ;se a posicao atual for um obstaculo
  (if(isObstaclep (state-pos (problem-initial-state problem)) (state-track (problem-initial-state problem)))
    (return-from best-search nil))
  ;se a posicao atual for a meta, a distanica e 0
  (if(isGoalp (problem-initial-state problem))
    (return-from best-search nil))
  
  (let((node)
    (nosNaoVis) ;lista com os nos ainda nao visitados
    (nosVis);
    (lst) ;lista com o resultado final
    (lstNextState) ;nextState do node
    (child))

    (setf lstNextState nil)
    (setf lst (list))
    (setf nosVis (list))
    ;como e o node inicial, o parent e nill e o g e 0
    (setf node 
      (make-node 
        :PARENT nil 
        :STATE (problem-initial-state problem) 
        :F (funcall (problem-fn-h problem) (problem-initial-state problem)) 
        :G 0 
        :H (funcall (problem-fn-h problem) (problem-initial-state problem))))
    (setf nosNaoVis (list node))

    (loop do
      ;se todos os nos ja tiverem sido vesitados
      (if(equal nil nosNaoVis)
        (return-from best-search nil))
      ;o node passa a ser um no ainda nao visitado
      (setf node (pop nosNaoVis))
      ;se o node em que estamos e um no objetivo
      (if(funcall (problem-fn-isGoal problem) (node-state node))
        (progn
          (loop 
            (if(null node)
              (return-from best-search lst))
            (push (node-state node) lst)
            (setf node (node-parent node)))))

      (dolist (lstNextState (funcall (problem-fn-nextStates problem) (node-state node)) nil)
        ;Como e o filho do node, o parent e o node, e o state e o estado onde estamos
        ;f = g+h
        (setf child
          (make-node
            :PARENT node 
            :STATE lstNextState 
            :F  (+ (funcall (problem-fn-h problem) lstNextState) (+(node-g node) (state-cost lstNextState)))
            :G  (+(node-g node) (state-cost lstNextState))
            :H  (funcall (problem-fn-h problem) lstNextState)))

        ;inser o child na lista com os nos que ainda n foram visitados
        (setf nosNaoVis (push child nosNaoVis)))
      ;ordenamos a lista usando a funcao sort 
      ;funcao existente em clisp ate a versao 2.49
      (setf nosNaoVis (sort nosNaoVis #'< :key #'getNodeF)))))