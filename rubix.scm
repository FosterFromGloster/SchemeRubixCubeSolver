;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Programming Assignment --- Fixing The World    ;;
;; 25/3/15                                                   ;;
;; <James Foster FSTJAM001>   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;-------------------HELPER FUNCTIONS----------------------
;; ;print function for debugging purposes
(define (print . args)
  (cond ((not (null? args))
        (display (car args))
        (apply print (cdr args)))
  )
)

;; ;gets nth index of 0-indexed list. Can use list-ref instead
(define (index lst idx)
    (if (null? lst)
        lst
        (if (= idx 0)
            (car lst)
            (index (cdr lst) (- idx 1))
        )
    )
)

(define (in item lst)
    (if (null? lst)
        #f
        (if (equal? item (car lst))
            #t
            (in item (cdr lst))
        )
    )
)

(define (lengthHelper n lst)
    (if (null? lst)
        n
        (lengthHelper (+ n 1) (cdr lst))
    )
)

;; ;finds length of a list
(define (length lst)
    (lengthHelper 0 lst)
)



;---------------------SOLVED STATES------------------------
;solved states of a 2x2x2 rubiks cube
(define solvedStates
    '(  ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
        ((3 1) (1 1) (4 1) (2 1) (7 3) (5 3) (8 3) (6 3))
        ((4 1) (3 1) (2 1) (1 1) (8 3) (7 3) (6 3) (5 3))
        ((2 1) (4 1) (1 1) (3 1) (6 3) (8 3) (5 3) (7 3))

        ((5 5) (1 6) (7 5) (3 6) (6 5) (2 6) (8 5) (4 6))
        ((7 5) (3 6) (8 5) (4 6) (5 5) (1 6) (6 5) (2 6))
        ((8 5) (4 6) (6 5) (2 6) (7 5) (3 6) (5 5) (1 6))
        ((6 5) (2 6) (5 5) (1 6) (8 5) (4 6) (7 5) (3 6))

        ((2 5) (6 6) (4 5) (8 6) (1 5) (5 6) (3 5) (7 6))
        ((4 5) (8 6) (3 5) (7 6) (2 5) (6 6) (1 5) (5 6))
        ((3 5) (7 6) (1 5) (5 6) (4 5) (8 6) (2 5) (6 6))
        ((1 5) (5 6) (2 5) (6 6) (3 5) (7 6) (4 5) (8 6))

        ((7 1) (8 1) (5 1) (6 1) (3 3) (4 3) (1 3) (2 3))
        ((5 1) (7 1) (6 1) (8 1) (1 3) (3 3) (2 3) (4 3))
        ((6 1) (5 1) (8 1) (7 1) (2 3) (1 3) (4 3) (3 3))
        ((8 1) (6 1) (7 1) (5 1) (4 3) (2 3) (3 3) (1 3))

        ((3 2) (4 2) (7 4) (8 4) (1 2) (2 2) (5 4) (6 4))
        ((1 2) (3 2) (5 4) (7 4) (2 2) (4 2) (6 4) (8 4))
        ((2 2) (1 2) (6 4) (5 4) (4 2) (3 2) (8 4) (7 4))
        ((4 2) (2 2) (8 4) (6 4) (3 2) (1 2) (7 4) (5 4))

        ((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4))
        ((7 2) (5 2) (3 4) (1 4) (8 2) (6 2) (4 4) (2 4))
        ((8 2) (7 2) (4 4) (3 4) (6 2) (5 2) (2 4) (1 4))
        ((6 2) (8 2) (2 4) (4 4) (5 2) (7 2) (1 4) (3 4))
    )
)
;; ;-----------------------------------------------------


;; ;---------------------QUESTION 1.1-----------------------
;; ;helper function for rotating the cube. Recalculates the various orientations
;; ;of the sub-cubes

(define (recalculateOrientation orientation axis)
    (cond
        [(= axis 0)
            (if (> orientation 4)
                orientation
                (if(= orientation 4)
                    1
                    (+ orientation 1)
                )
            )
        ]
        [(= axis 1)
            (if (or (= orientation 1) (= orientation 3))
                orientation
                (cond
                    [(= orientation 2) 6]
                    [(= orientation 4) 5]
                    [(= orientation 5) 2]
                    [(= orientation 6) 4]
                )
            )
        ]
        [(= axis 2)
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 5]
                    [(= orientation 3) 6]
                    [(= orientation 5) 3]
                    [(= orientation 6) 1]
                )
            )
        ]
    )
)


;rotations are performed using the left hand rule
;rotates left 4 cubes along x axis
(define (rotateX ispositive state)
	(if ispositive
		(list (list (index  (index state 4) 0 ) (recalculateOrientation (index  (index  state 4) 1) 0 ) )
		(list (index (index state 1) 0 ) (index (index state 1) 1 ) ) 
		(list (index (index state 0) 0 ) (recalculateOrientation(index  (index state 0) 1) 0 ) )
		(list (index (index state 3) 0 ) (index (index state 3) 1 ) ) 
		(list (index (index state 6) 0 ) (recalculateOrientation(index  (index state 6) 1) 0 ) ) 
		(list (index (index state 5) 0 ) (index (index state 5) 1 ) )
		(list (index (index state 2) 0 ) (recalculateOrientation(index  (index state 2) 1) 0 ) ) 
		(list (index (index state 7) 0 ) (index  (index state 7) 1 ) ) ) 
	)
)

;rotates bottom 4 cubes along y axis
(define (rotateY ispositive state)
	(if  ispositive
		(list ( list (index (index state 0) 0 ) (index (index state 0) 1 ) ) 
		(list (index (index state 1) 0 ) (index (index state 1) 1 ) ) 
		(list (index (index state 2) 0 ) (index (index state 2) 1 ) ) 
		(list (index (index state 3) 0 ) (index (index state 3) 1 ) ) 
		(list (index (index state 5) 0 ) (recalculateOrientation(index (index state 5) 1) 1 ) )
		(list (index (index state 7) 0 ) (recalculateOrientation(index (index state 7) 1) 1 ) )
		(list (index (index state 4) 0 ) (recalculateOrientation(index (index state 4) 1) 1 ) ) 
		(list (index (index state 6) 0 ) (recalculateOrientation(index (index state 6) 1) 1 ) ) ) 
	)
)

;rotates back 4 cubes along z axis
(define (rotateZ ispositive state)		
	(if ispositive
		(list ( list (index (index state 1) 0 ) (recalculateOrientation(index (index state 1) 1) 2 ) )
		(list (index (index state 5) 0 ) (recalculateOrientation(index (index state 5) 1) 2 ) )
		(list (index (index state 2) 0 ) (index (index state 2) 1 ) ) 
		(list (index (index state 3) 0 ) (index (index state 3) 1 ) ) 
		(list (index (index state 0) 0 ) (recalculateOrientation(index (index state 0) 1) 2 ) )
		(list (index (index state 4) 0 ) (recalculateOrientation(index (index state 4) 1) 2 ) )
		(list (index (index state 6) 0 ) (index (index state 6) 1 ) ) 
		(list (index (index state 7) 0 ) (index (index state 7) 1 ) ) ) 
	)
)

;; ;helper for rotate function
;; ;to do the negative rotation the positive rotation is applied three times
(define (rotateHelper char state)
    (cond
        [(char=? char #\x) (rotateX #t state)]
        [(char=? char #\X) (rotateX #t (rotateX #t (rotateX #t state)))]
        [(char=? char #\y) (rotateY #t state)]
        [(char=? char #\Y) (rotateY #t (rotateY #t (rotateY #t state)))]
        [(char=? char #\z) (rotateZ #t state)]
        [(char=? char #\Z) (rotateZ #t (rotateZ #t (rotateZ #t state)))]
    )
)

;; ;parses a string for rotations
(define (rotate rotations state)
    (if (= (string-length rotations) 0)
        state
        (rotate (substring rotations 1 (string-length rotations)) (rotateHelper (string-ref rotations 0) state))
    )
)


;-----------------------QUESTION 1.2-----------------------
;generates the successor states of the current given rubiks cube state
;; ;recursive function that returns the history with the previous moves included
(define (historyHelper history successorMoves prevMoves)
	(if (null? successorMoves)
        	      history
            	      (historyHelper  (cons (append prevMoves (car successorMoves) ) history   )  (cdr successorMoves) prevMoves)
    	)
)

(define (generateSuccessorStates state prevMoves) 
    (list
        (list
            (rotateX #t state)
            (rotateX #t (rotateX #t (rotateX #t state)))
            (rotateY #t state)
            (rotateY #t (rotateY #t (rotateY #t state)))
            (rotateZ #t state)
            (rotateZ #t (rotateZ #t (rotateZ #t state)))
        )
        (historyHelper '() '(("Z") ("z") ("Y") ("y") ("X") ("x")) prevMoves)
    )
)


;-----------------------------QUESTION 2.1--------------------------

;finds all the states at a specific depth

;; ;finds all the states at a specific depth
;; ;Recursive function that iterates through the given states generated by gen states building up a new list with each new turn
(define (depth stateAfterTurns  n builder)
	(if (= n 0)
	     stateAfterTurns
	     (depth (stateIterator (car stateAfterTurns) builder (car (cdr stateAfterTurns))) (- n 1) builder)
	)
)

;; ;the state iterator goes through any list of states and moves for each state generates a new set of states with the previous moves
(define (stateIterator states builder moves)
	  (if (null? states)
		(listSplit (reverse builder) '() '() 0)
		(stateIterator (cdr states) (append builder (generateSuccessorStates (car states) (car moves) ) ) (cdr moves) )          
	)
)

;; ;Because the stateIterator adds new generator states each time a new state is encountered the moves are added after the new states.
;; ;This wont do as the depth function requires the list in a certain format for the recursion to work. This means that I slip the list
;; ;Adding the moves to the end of all the states.
(define (listSplit builtList statesBuilder movesBuilder dex)
	(if (null? builtList )
		(cons movesBuilder (list statesBuilder))
		(if (= dex 0 ) 
			(listSplit (cdr builtList) (append (car builtList) statesBuilder) movesBuilder 1)
			(listSplit (cdr builtList) statesBuilder (append (car builtList) movesBuilder) 0)	

		)
	)
)

;; ;the required interface just checks that the number of turns is not 0 before the recursive depth function is called
(define (genStates n state moves)
	(if (= n 0)
		(cons (cons state '()) (list (list '())) )
		(depth (generateSuccessorStates state '()) (- n 1) '() )
	)
)

;-----------------------------QUESTION 2.2--------------------------
; ;returns the number of states the rubiks cube can have.
(define (numberOfStates n)
	(expt 6 n) 	
)


;---------------------------QUESTION 3.1-----------------------
;Solves a rubiks cube using breadth first search. Can solve up to roughly 7 moves.
;; ;So I used a number of helper functions in this part.

;; ;This is the main interface that checks that the given state isint already solved
;; ;else run the solver
(define (solveCube solved initial n)
	(if( stateFoundChecker solved initial '('( '() ) '( '() )) 0)
		'()
		(cubeSolverHelper solved initial (+ n 1) #f)
	)    
)

;; ;This helper function returns the moves from the genstates list if the state appears in the solved states otherwise returns false
(define (stateFoundChecker solved state genStates dex)
	(if (inSolved state solved)
		(index (car (cdr genStates)) dex)
		#f
	)
)

;; ;This helper method is of no use..yet	
(define (getTurnIndex index states)
	(+ (/ (length (car states)) 2 ) index)
)

;; ;This is kinda the main cube solver helper method. It is recursive and mainly checks if the variable found returned by
;; ;stateIteratorHelper
;; ;is either a boolean or not. This will determine whether the program has actually found the moves or whether it needs to
;; ;generate the next
;; ;states
(define (cubeSolverHelper solved initial n found)
	(if ( = n 7)
		#f
		(if(boolean? found)
			(cubeSolverHelper solved initial  (+ n 1) (stateIteratorHelper solved (genStates n initial '()) #f 0))
			found	
		)
	)	
)

;; ;The state iterator just goes through the states given to it checking if each one is a solved state and keeping track of the index
(define (stateIteratorHelper solved states found dex)
	(if (null? (car states))
		#f
		(if (boolean? found)
			(stateIteratorHelper solved (cons (cdr (car states)) (cdr states)) (stateFoundChecker solved (car (car states)) states dex)  (+ dex 1))
			found
		)
	)
)

;; ;I fashioned my own comparitor equals function here because i wasnt sure whether the supplied helper went deep enough.
;; ;Also I think this one might be more effiicient as it returns false as soon as two numbers are not the same.
(define (is-state-solved state stateToCheck found)
	(if (null? state)
		found
		(if found
			(if  (= (index (car state) 0) (index (car stateToCheck) 0))
				(if (= ( index (car state) 1 ) (index (car stateToCheck) 1 ))
					(is-state-solved (cdr state) (cdr stateToCheck) #t)
					(is-state-solved (cdr state) (cdr stateToCheck) #f)
				)
				(is-state-solved (cdr state) (cdr stateToCheck) #f)	
		
			)
			found
		)
	)
)

;; ;This is just a simple recursive function that goes through the solved states and checks the state given against them
(define (inSolved item lst)
    (if (null? lst)
        #f
        (if (is-state-solved item (car lst) #t)
            #t
            (inSolved item (cdr lst))
        )
    )
)

