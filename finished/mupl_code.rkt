;; CS 4003: Programming Languages, Team Project
;; Authors: William Hauber, Matthew McClellan, and John Mikolay
;; Code adapted in part from project by Ernestas Poskus @ https://github.com/ernestas-poskus

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (operand1 operand2)  #:transparent)  ;; add two expressions
(struct ifgreater (leftSideOperand rightSideOperand trueResult falseResult)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (functionName functionArg functionBody) #:transparent) ;; a recursive(?) 1-argument function
(struct call (functionClosure functionArg)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (pairHead pairTail)     #:transparent) ;; make a new pair
(struct fst  (currentPair)    #:transparent) ;; get first part of a pair
(struct snd  (currentPair)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (currentUnit) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Part 1 - Warm-up

;;Converts a racket list to a mupl list
(define (racketlist->mupllist racketList)
  (if (null? racketList)
      (aunit)
      (apair (car racketList) (racketlist->mupllist (cdr racketList)))))

;;Converts a mupl list to a racket list
(define (mupllist->racketlist muplList)
  (if (aunit? muplList)
      null
      (cons (apair-pairHead muplList) (mupllist->racketlist (apair-pairTail muplList)))))


;; Part 2 - Implementing the language

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
         
        ;;Adds two operands
        [(add? e) 
        
        ;;Evaluate the parametersunder the environment
         (let ([racketOperand1 (eval-under-env (add-operand1 e) env)]
               [racketOperand2 (eval-under-env (add-operand2 e) env)])
               
           ;;If they both are ints, add them
           (if (and (int? racketOperand1)
                    (int? racketOperand2))
               (int (+ (int-num racketOperand1) 
                       (int-num racketOperand2)))
               (error "MUPL addition applied to non-number")))]
               
        ;; returns the given int e
        [(int? e)
         e]
         
        ;;Decides if leftSideOperand is greater than rightSideOperand. If so, do trueResult, else do falseResult
        [(ifgreater? e)
        
        ;;Evaluete left and right side operands under environment
         (let ([racketLeftSideOperand (eval-under-env (ifgreater-leftSideOperand e) env)]
               [racketRightSideOperand (eval-under-env (ifgreater-rightSideOperand e) env)])
               
           ;;If racketLeftSideOperand and racketRightSideOperand are ints, compare them. If true, evaluate trueResult, else evaluate falseResult
           (if (and (int? racketLeftSideOperand)
                    (int? racketRightSideOperand))
               (if (> (int-num racketLeftSideOperand)
                      (int-num racketRightSideOperand))
                   (eval-under-env (ifgreater-trueResult e) env)
                   (eval-under-env (ifgreater-falseResult e) env))
               (error "MUPL ifgreater applied to non-number")))]  
               
               
        ;;Evaluate function e using the racket environment as a closure
        [(fun? e) (closure env e)]
        
        
        ;;Models a function call
        [(call? e)
         ;;creates the racket function closure from the mupl expression
         (let ([currentClosure (eval-under-env (call-functionClosure e) env)])

           ;;If the closure looks good, call the function, else throw error
           (if (closure? currentClosure)
               (let* ([racketFunctionClosure (closure-fun currentClosure)]
                      [functionArg (eval-under-env (call-functionArg e) env)]
                      [env (cons (cons (fun-functionArg racketFunctionClosure) functionArg) (closure-env currentClosure))])
                 (if (fun-functionName racketFunctionClosure)
                   (let ([env (cons (cons (fun-functionName racketFunctionClosure) currentClosure) env)])
                     (eval-under-env (fun-functionBody racketFunctionClosure) env))
                   (eval-under-env (fun-functionBody racketFunctionClosure) env)))
               (error "First param for call is not a closure")))]
               
               
        ;;Evaluates e, then evaluates the second value in an environment which maps that e's value to variable.        
        [(mlet? e)
         (let* ([assignedValue (eval-under-env (mlet-e e) env)]
                [variable (mlet-var e)])
           (eval-under-env (mlet-body e) (cons (cons variable assignedValue) env)))]
           
        ;; Checks if e is apair. Returns the paired values of racketPairHead and racketPairTail
        [(apair? e)

         ;;Evaluates and racketizes pairHead and pairTail 
         (let ([racketPairHead (eval-under-env (apair-pairHead e) env)]
               [racketPairTail (eval-under-env (apair-pairTail e) env)])

           ;;Returns the new pair of racket values
           (apair racketPairHead racketPairTail))]
           
           
       ;;Gets first element of a pair.
        [(fst? e)
        
        ;;set racketCurrentPair as racketized MUPL pair
         (let ([racketCurrentPair (eval-under-env (fst-currentPair e) env)])
         
         ;;If racketCurrentPair is actually a pair, do fst and return head of pair. Else throw error
         (if (apair? racketCurrentPair)
             (apair-pairHead racketCurrentPair)
             (error "MUPL first applied to non-pair")))]
             
             
        ;;Gets second element of a pair.
        [(snd? e)
        
        ;;set racketCurrentPair as racketized MUPL pair
         (let ([racketCurrentPair (eval-under-env (snd-currentPair e) env)])
         
         ;;If racketCurrentPair is actually a pair, do fst and return tail of pair. Else throw error
           (if (apair? racketCurrentPair)
               (apair-pairTail racketCurrentPair)
               (error "MUPL second appied to non-pair")))]
               
        
        ;;returns the given aunit
        [(aunit? e) e]
        
        ;;Decides if variable is a unit. Units signify the end of structures (like null terminators end c++ arrays).
        [(isaunit? e)
        
        ;;Racketize the given unit
         (let ([racketCurrentUnit (eval-under-env (isaunit-currentUnit e) env)])
         
         ;;If it's actually a unit, return 1, else return 0
           (if (aunit? racketCurrentUnit)
               (int 1)
               (int 0)
               ))]
               
        ;;returns the given closure 
        [(closure? e)
         e]
         
         
        [#t (error "bad MUPL expression")]))
        
        
        
        

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Part 3 - Expanding the language

;;If e1 is a unit, then return trueResult, else return falseResult
(define (ifaunit e1 trueResult falseResult)
  (ifgreater (isaunit e1)
             (int 0)
             trueResult
             falseResult))

;;sequentially binds each (string . MUPL Expression) pair and adds
;; them to the same environment each time. Then evaluates muplExpression parameter
;; in this created environment.  
(define (mlet* racketPairList muplExpresion)

  ;;If racketPairList isn't null, make muplexpression using
  (if (null? racketPairList)
      muplExpresion
      (mlet (car (car racketPairList))
            (cdr (car racketPairList))
            (mlet* (cdr racketPairList) muplExpresion))))

;;Compares integers int1 and int2. If they are equal, evaluate trueResult
;;, else evaluate falseResult
(define (ifeq int1 int2 trueResult falseResult)
  (mlet* (list (cons "_x" int1) (cons "_y" int2))
         (ifgreater (var "_x")
                    (var "_y")
                    falseResult
                    (ifgreater (var "_y")
                               (var "_x")
                               falseResult
                               trueResult))))

;; Part 4 - Using the language

;;Takes in a function and optputs a new function which then takes in a mupl list
;; and applies the parameter function to each element in the list, then untimately
;; returns a new mupl list with the altered values.
(define mupl-map
  (fun #f "funcToApply"
       ; Body of this function is actually a function definition
       (fun "iterator" "list"
            ; If we hit a unit ...
            (ifgreater
             (isaunit (var "list"))
             (int 0)
             ; ... return a unit, as we're out of values
             (aunit)

             ; otherwise, return a pair with the mapped value and recurse
             (apair
              (call (var "funcToApply") (fst (var "list")))
              (call (var "iterator") (snd (var "list"))))
             )
            )
       )
  )

;;Uses the mupl-map function to make a map which adds N to each element
;;of an integer list.
(define mupl-mapAddN
  ; Add a reference to our map function
  (mlet
   "map" mupl-map
   (fun #f "n"
        ; Apply the map function
        (call
         (var "map")
         ; Define the function to add n to the values
         (fun
          #f "x"
          (add (var "x") (var "n"))
          )
         )
        )
   )
  )