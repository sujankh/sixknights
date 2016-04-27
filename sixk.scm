;; (define findMoves
;;   (lambda (board, moveList, visited)
;;     (
;;      (let (blackKnights (findKnights 1 Board 0 '()))
;; 	 )
;;     )
;;    )
;;   )


;; From the given list of positions remove the positions at which the knight is already at
;; its final position
;; eg. a Black knight at 10 , 11, 12 need not be moved
;; a white knight at 1, 2, 3 need not be moved

;; predicate is a lambda which is used to decide if a knight is in its final position or not
(define removeFinalPositions
  (lambda (positions predicate)
    (cond
     ((null? positions) '())
     ((predicate (car positions))
      (append '() (removeFinalPositions (cdr positions) predicate)))
     (else
      (append
       (list (car positions)) (removeFinalPositions (cdr positions) predicate))
      )      
     )
  ))

;;black is swapped if its position is >=9 (i.e. 9, 10, 11)
(define blackSwapped
  (lambda (position)
    (>= position 9)
    ))

;;white is swapped if its position is <=2 (i.e. 0, 1, 2)
(define whiteSwapped
  (lambda (position)
    (<= position 2)
    ))

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
