;;Initial board
;; '(1 1 1 0 0 0 0 0 0 2 2 2)
;;(findMoves (list (cons '(2 2 2 0 0 0 0 0 0 1 1 1) '(()))))
;;(findMoves (list (cons '(1 1 1 0 0 0 0 0 0 2 2 2) '(()))))
;;queue is a list of cons cells
;;each cons cell: [board . moves that made this board]

(define findMoves
  (lambda (queue)
    (let ((board (caar queue)) (moveList (cdar queue)))
      (cond
       ((equal? board '(2 2 2 0 0 0 0 0 0 1 1 1)) moveList) ;;final state
       (else
	(let (
	      (newQueue (cdr queue)) ;;additions will be done to this queue
	      (knightPositions (getFilledPositions board))	      
	      )
	  ;;explore child states
	  ;;add them to the queue
	  ;;recurse
	  (findMoves (append newQueue (explorenewstates board knightPositions moveList)))
	  )
       ))
	)
    ))

;;finds the child states of the given board and adsds it to the queue
(define explorenewstates
  (lambda (board knightPositions moveList)
    (let ( (blackKnights (getBlackPositions board))
	   (whiteKnights (getWhitePositions board))
	   )
      ;;get the new states by moving black and white knights
      ;;append them and return
      (append
       (flatten (moveKnight 1 (lambda (x) (getBlackMoves x knightPositions)) blackKnights board moveList) '())
       (flatten (moveKnight 2 (lambda (x) (getWhiteMoves x knightPositions)) whiteKnights board moveList) '())
      )      
      )
    ))

;;movingFunction = getBlackMoves or getWhiteMoves
(define moveKnight
  (lambda (knight movingFunction fromPositions board moveList)
    (let (
	  ;;finalPositions is a list of (list of positions to which knight can move for each from)
	  (finalPositions (map movingFunction fromPositions))	  
	  )      
       (map
	(lambda (from toList) (getNewBoardNodes knight board from toList moveList))
	fromPositions finalPositions)
      
  )))

;;returns a list of child boards along with the moves that created the board
;;input: from and toList

(define getNewBoardNodes
  (lambda (knight board from toList moveList)  ;;(5 (6 9)) from 5, 6 and 9 can be reached
    (map (lambda (to) (getUpdatedBoardNode knight board from to moveList)) toList)
  ))

;;creates a cons cell [NewBoard . Moves till now]
;;appends the new move to the existing move list
(define getUpdatedBoardNode
  (lambda (knight board from to moveList)
   (cons (updateBoard  knight board from to) (append moveList (cons from to)))
  )
  )
    
;; update the board so that knight at from moves to position 'to'
;; invariant..to = 0 (i.e. destination should be empty)
(define updateBoard
  (lambda (knight board from to)
    (setList (setList board from 0 0 '()) to knight 0 '())
  ))

(define setList
  (lambda (lst index value i modified)
    (cond
     ((null? lst) lst)
     ((eq? index i) (append modified (list value) (cdr lst))) ;;replace element at index by value
     ((setList (cdr lst) index value (+ i 1) (append modified (list (car lst)))))
  ))))


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


(define flatten
  (lambda (lst f)
    (cond
     ((null? lst) f)
     (else
      (let ((cur (car lst)))
	(cond
	 ((not (list? cur)) (flatten (cdr lst) (append f (list cur))))
	 (else
	  (let ((len (length cur)))
	  (if (> len 1)
	      (append f (flatten cur '()) (flatten (cdr lst) '()))
	      (flatten (cdr lst) (append f cur))
	      ))))
	)))))
