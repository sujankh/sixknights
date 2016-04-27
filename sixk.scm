;;Initial board
;; '(1 1 1 0 0 0 0 0 0 2 2 2)

;; (define findMoves
;;   (lambda (board, moveList, visited)
;;     (
;;      (let (blackKnights (findKnights 1 Board 0 '()))
;; 	 )
;;     )
;;    )
;;   )




;;Get all positions of white knights in the board that are not in their final pos
;;if the knight is already in final position (i.e 0, 1, 2), that position is not returned
(define getWhitePositions
  (lambda (board)
    (filterPositions (findKnights isWhite board 0) blackInitialPos)
  ))

;;Get all positions of black knight in the board
;;if the knight is already in final position (i.e 9, 10, 11), that position is not returned
(define getBlackPositions
  (lambda (board)
    (filterPositions (findKnights isBlack board 0) whiteInitialPos)
  ))

;;return all the positions filled by the knights in the board
(define getFilledPositions
  (lambda (board)
    (findKnights (lambda(x) (or (isBlack x) (isWhite x))) board 0)
    )
  )

;; get all the moves to which a black knight can go
;; a black knight does not return back to its initial positions (0, 1, 2)
(define getBlackMoves
  (lambda (index filledPositions)
    (filterPositions (getValidTargetPositions index filledPositions) blackInitialPos)    
    )
  )

;; get all the moves to which a black knight can go
;; a black knight does not return back to its initial positions (0, 1, 2)
(define getBlackMoves
  (lambda (index filledPositions)
    (filterPositions (getValidTargetPositions index filledPositions) blackInitialPos)    
    )
  )

;;a white does not move back to 10, 11, 12
(define getWhiteMoves
  (lambda (index filledPositions)
    (filterPositions (getValidTargetPositions index filledPositions) whiteInitialPos)    
    )
  )


;;get the target position to which a knight could move
;;the target position should not be where a knight already exists in the board
;;also 
(define getValidTargetPositions
  (lambda (index filledPositions)
    (delete-matching-items (getAllTargets index) (lambda(x) (isInvalidPosition x filledPositions)))
    ))

;;a position is invalid if
;;another knight has already filled the position
;;or as per our heuristic position = 4 or 7
;;we don't want our knights to jump to 4 or 7
(define isInvalidPosition
  (lambda (pos filledPositions)
    (or (eq? pos 4) (eq? pos 7) (member pos filledPositions))
  ))

;;returns all valid positions to which a knight could move
;;assumes the board has only one knight
(define getAllTargets
  (lambda (index)
    (let	
	(
	  (r (quotient index 3))
	  (c (modulo index 3))	  
	  )
      (keep-matching-items
       (map toindex
       (list
       (cons (- r 2) (+ c 1))
       (cons (- r 2) (- c 1))
       (cons (+ r 2) (+ c 1))
       (cons (+ r 2) (- c 1))

       (cons (+ r 1) (- c 2))
       (cons (- r 1) (- c 2))
       (cons (+ r 1) (+ c 2))
       (cons (- r 1) (+ c 2))
       ))
       (lambda (x) (>= x 0))
      ))
     )
  )

;;get linear index from rowcol which is a list(r, c)
 (define toindex
  (lambda (rowcol)
    (let ((r (car rowcol)) (c (cdr rowcol)))      
      (if (and (>= r 0) (<= r 3) (>= c 0) (<= c 2))
       (+ (* r 3) c) -1)
      )))


;; From the given list of positions remove the positions that return true with predicate
(define filterPositions
  (lambda (positions predicate)
    (delete-matching-items positions predicate)
  ))

;; From the given list of positions remove the positions at which the knight is already at
;; its final position
;; eg. a Black knight at 10 , 11, 12 need not be moved
;; a white knight at 1, 2, 3 need not be moved
;; predicate is a lambda which is used to decide if a knight is in its final position or not
;; (define removeFinalPositions
;;   (lambda (positions predicate)
;;     (cond
;;      ((null? positions) '())
;;      ((predicate (car positions))
;;       (append '() (removeFinalPositions (cdr positions) predicate)))
;;      (else
;;       (append
;;        (list (car positions)) (removeFinalPositions (cdr positions) predicate))
;;       )      
;;      )
;;   ))

;;initial position of white knights >=9 (i.e. 9, 10, 11)
(define whiteInitialPos
  (lambda (position)
    (>= position 9)
    ))

;;initial position of black knights <=2 (i.e. 0, 1, 2)
(define blackInitialPos
  (lambda (position)
    (<= position 2)
    ))

(define isBlack
  (lambda (symbol)
    (eq? symbol 1)
    ))

(define isWhite
  (lambda (symbol)
    (eq? symbol 2)
  ))


;;(findKnights (lambda(x) (or (isWhite x) (isBlack x))) '(1 1 1 0 0 0 0 0 0 2 2 2) 0)

;;Returns the indices in the board at which the Knight is placed
;;eg {1, 1, 1, 0 ,0 ....} with symbol = 1 will return (0, 1, 2)
(define findKnights
  (lambda (isKnight Board index)
     (cond
      ((null? Board) '())
      ;;((eq? symbol (car Board))
      ((isKnight (car Board))       	
       (append (list index) (findKnights isKnight (cdr Board) (+ index 1))))       
      (else
       (append '() (findKnights isKnight (cdr Board) (+ index 1))))
      )    
  ))
