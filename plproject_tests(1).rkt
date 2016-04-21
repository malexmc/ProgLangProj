#lang racket

(require "FinalProject.rkt")

(define (fun_double)
  (fun
   "double" "x"
   (add (var "x") (var "x"))
   )
  )

(define (fun_isPositive)
  (fun
   "isPositive" "x"
   (ifgreater (var "x") (int 0) (int 1) (int 0))
   )
  )

(define (fun_countDown)
  (fun
   "countDown" "x"
   (ifgreater (var "x") (int 0)
              (call
               (var "countDown")
               (add (var "x") (int -1))
               )
              (int 0)
              )
   )
  )

(define (fun_fiboBad)
  (fun
   "fiboBad" "x"
   (ifgreater
    (int 1) (var "x")
    (int 0)
    (ifgreater (int 2) (var "x")
               (int 1)
               (add
                (call
                 (var "fiboBad")
                 (add (var "x") (int -1))
                 )
                (call
                 (var "fiboBad")
                 (add (var "x") (int -2))
                 )
                )
               )
    )
   )
  )

; data: (iteration, numIterations, previous, current) 
(define (fun_fiboGood)
  (fun
   "wrapperFunc" "numIterations"
   (mlet "wrapper"    
         (fun
          "fiboGood" "data"
          (mlet
           "iter" (fst (var "data"))
           (mlet
            "numI" (fst (snd (var "data")))
            (mlet
             "prev" (fst (snd (snd (var "data"))))
             (mlet
              "curr" (fst (snd (snd (snd (var "data")))))
              ; if iter == numI ...
              (ifgreater
               (add
                (int 1) (var "iter")
                )
               (var "numI")
               
               ; ... then return the prev value
               (var "prev")
               
               ; otherwise, recurse
               (call
                (var "fiboGood")

                ; build our list of data
                (apair
                 (add
                  (int 1) (var "iter")
                  )
                 (apair
                  (var "numI")
                  (apair
                   (var "curr")
                   (apair
                    (add
                     (var "prev")
                     (var "curr")
                     )
                    (aunit)
                    )
                   )
                  )
                 )
                )
               )
              )
             )
            )
           )
          )

         ; create the initial list of data using the user
         ; provided numIterations
         (call
          (var "wrapper")
          (apair (int 0)
                 (apair (var "numIterations")
                        (apair (int 0)
                               (apair (int 1)
                                      (aunit)
                                      )
                               )
                        )
                 )
          )
         )
   )
  )
               
(define tests
  (list
   ; int tests
   (list 'int_1 (int 4) (int 4))
   (list 'int_2 (int 0) (int 0))

   ; add tests
   (list 'add_1 (add (int 2) (int 3)) (int 5))
   (list 'add_2 (add (int 0) (int 1)) (int 1))

   ; ifgreater tests
   (list 'ifgreater_1 (ifgreater (int 1) (int 0) (int 7) (int 3)) (int 7))
   (list 'ifgreater_2 (ifgreater (int 0) (int 1) (int 7) (int 3)) (int 3))
   (list 'ifgreater_3 (ifgreater (int 0) (int 0) (int 7) (int 3)) (int 3))

   ; fun tests
   (list 'call_1 (call (closure '() (fun_double)) (int 3)) (int 6))
   (list 'call_2 (call (closure '() (fun_isPositive)) (int 1)) (int 1))
   (list 'call_3 (call (closure '() (fun_countDown)) (int 8)) (int 0))

   ; mlet tests
   (list 'mlet_1 (mlet "x" (int 23) (add (int 7) (var "x"))) (int 30))
   (list 'mlet_2 (mlet "swamp" (int 54321) (add (int -54321) (var "swamp"))) (int 0))

   ; apair tests
   (list 'apair_1 (fst (apair (int 4) (int 2))) (int 4))
   (list 'apair_2 (snd (apair (int 4) (int 2))) (int 2))

   ; aunit tests
   (list 'aunit_1 (fst (apair (aunit) (int 3))) (aunit))
   (list 'aunit_2 (snd (apair (int 9) (aunit))) (aunit))

   ; isaunit tests
   (list 'isaunit_1 (isaunit (aunit)) (int 1))
   (list 'isaunit_2 (isaunit (int 2)) (int 0))

   ; fibo tests
   (list 'fibo_1 (call (closure '() (fun_fiboBad)) (int 28)) (int 317811))
   (list 'fibo_2 (call (closure '() (fun_fiboGood)) (int 0)) (int 0))
   (list 'fibo_3 (call (closure '() (fun_fiboGood)) (int 1)) (int 1))
   (list 'fibo_4 (call (closure '() (fun_fiboGood)) (int 28)) (int 317811))
   (list 'fibo_5 (call (closure '() (fun_fiboGood)) (int 50)) (int 12586269025))
   )
  )

(define (runTests tests)
  ; Get the next test data
  (let* ([test (car tests)]
         [name (car test)]
         [expression (car (cdr test))]
         [result (car (cdr (cdr test)))])
    
    ; Print the name and a space
    (display name)
    (display " ")

    ; Run the test and print the result
    (if
     (equal?
      (eval-exp
       expression
       )
      result
      )
     (display "pass")
     (display "FAIL!!")
     )
    (display "\n")

    ; Recurse on the rest of the tests
    (if
     (equal? (cdr tests) '())
     (display "~~ DONE ~~")
     (runTests
      (cdr tests)
      )
     )
    )  
  )

(runTests tests)
