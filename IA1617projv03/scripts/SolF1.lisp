
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
;(load "datastructures.fas")
;(load "auxfuncs.fas")

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
	(null(nth (second pos) (nth (first  pos) (track-env track))))
)

(defun isGoalp (st) 
  "check if st is a goal state"
  (let ((pos (state-pos st)))
  	;para cada posicao da lista
  	(dolist (i (track-endpositions (state-track st)))
  		;se a posicao da lista for
  		(if (equal i pos) (return-from isGoalp t))))
  nil
)

(defun nextState (st act)
  "generate the nextState after state st and action act"
  (let (newstate)
    (setf newstate
      (make-STATE 
        :POS (list (+ (first (state-pos st)) (first act) (first (state-vel st))) (+ (second (state-pos st)) (second act) (second(state-vel st))))
        :VEL (list (+ (first act) (first (state-vel st))) (+ (second act) (second(state-vel st))))
  	    :ACTION act
  	    :COST 1
        :TRACK (state-track st)
        :OTHER nil
      )
    )
  	;se existir um obstaculo
  	(if (isObstaclep (state-pos newstate) (state-track st)) 
      ;custo +20, vel 0, pos e a posicao antiga
      (progn
        (setf (state-cost newstate) 20)
        (setf (state-vel newstate) (list 0 0))
        (setf (state-pos newstate) (state-pos st))
      )
    )
    ;se for a meta
    (if (isGoalp newstate)
      ;custo -100
      (setf (state-cost newstate) -100))
    newstate
  )
)
