;; (define findMoves
;;   (lambda (board, moveList, visited)
;;     (
;;      (let (blackKnights (findKnights 1 Board 0 '()))
;; 	 )
;;     )
;;    )
;;   )

;;Returns the indices in the board at which the Knight is placed
;;eg {1, 1, 1, 0 ,0 ....} with symbol = 1 will return (0, 1, 2)
(define findKnights
  (lambda (symbol Board index)
     (cond
      ((null? Board) '())
      ((eq? symbol (car Board))       	
       (append (list index) (findKnights symbol (cdr Board) (+ index 1))))       
      (else
       (append '() (findKnights symbol (cdr Board) (+ index 1))))
      )    
  ))
