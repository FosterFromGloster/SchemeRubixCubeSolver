(load "rubix.scm")

(print "Solving cube test \n ")
;Solving the cube unit tests. All should return true
(print (solveCube solvedStates (rotate "xz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0) "\n")
(print (solveCube solvedStates (rotate "xYz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0) "\n")
(print (solveCube solvedStates (rotate "xzyZ" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0) "\n")
(print (solveCube solvedStates (rotate "yXZxz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0) "\n")
(print (solveCube solvedStates (rotate "zXyXzY" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0) "\n")
;;next one may take a lil while
(print (solveCube solvedStates (rotate "xYzXzxy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0) "\n")

(print "testing recalculate orientation \n")
;Tests for recalculate orientation
(print (= 2 (recalculateOrientation 1 0)) "\n")
(print (= 5 (recalculateOrientation 5 0)) "\n")
(print (= 1 (recalculateOrientation 1 1)) "\n")
(print (= 6 (recalculateOrientation 2 1)) "\n")
(print (= 5 (recalculateOrientation 1 2)) "\n")
(print (= 2 (recalculateOrientation 2 2)) "\n")

(print "testing rotations \n")
;Tests for rotations
(print (equal? (rotate "y" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))) "\n")
(print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (rotateZ #t (rotateY #t (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))) "\n")
(print (equal? (rotate "yXz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (rotateZ #t (rotateX #t(rotateX #t (rotateX #t (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")
(print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (rotateX #t (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))) "\n")

;Tests for generateSuccessorStates
(print "testing generateSuccessorStates \n")
(print (equal? (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
         (list
             (list
            	(car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
            	(car (rotateX #t (rotateX #t (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))
            	(car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
            	(car (rotateY #t (rotateY #t (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))
            	(car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
            	(car (rotateZ #t (rotateZ #t (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))
             
             )
             '( ("x") ("X") ("y") ("Y") ("z") ("Z"))
         )
     )
 "\n")

;Test for number of states
(print "testing number of states \n")
(print (equal? (numberOfStates 10) 60466176))





