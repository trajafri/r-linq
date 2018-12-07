#lang racket

(require (for-syntax syntax/parse #;db))

(provide ;; operations provided
 from #;from-db join group-join where orderby select select-many groupby
 group in on by into)

;                                                                 
;                                                                 
;      ;;                                ;                        
;     ;                           ;                               
;     ;                           ;                               
;   ;;;;;  ;   ;  ; ;;    ;;;   ;;;;;  ;;;     ;;;   ; ;;    ;;;  
;     ;    ;   ;  ;;  ;  ;;  ;    ;      ;    ;   ;  ;;  ;  ;   ; 
;     ;    ;   ;  ;   ;  ;        ;      ;    ;   ;  ;   ;  ;     
;     ;    ;   ;  ;   ;  ;        ;      ;    ;   ;  ;   ;   ;;;  
;     ;    ;   ;  ;   ;  ;        ;      ;    ;   ;  ;   ;      ; 
;     ;    ;   ;  ;   ;  ;;       ;      ;    ;   ;  ;   ;  ;   ; 
;     ;     ;;;;  ;   ;   ;;;;    ;;;  ;;;;;   ;;;   ;   ;   ;;;  
;                                                                 
;                                      

;; join: [A -> X] [B -> Y] [X Y -> Boolean] [A B -> Z] [SequenceOf A] [SequenceOf B] -> [ListOf Z]
;; joins the given list based on the last function when the pred is satisfied.
;; the first two functions are used to extract values from A and B.
;; Note that first list is outer and second list is inner.
(define (join a->x b->y xy->bool ab->z la lb)
  (for*/list [(a la)
              (b lb)
              #:when (xy->bool (a->x a) (b->y b))]
    (ab->z a b)))

(module+ test (require rackunit)
  (check-equal? (join (λ (x) x) (λ (x) x) = (λ (x y) x)
                      '(1 2 3) '(3 4 5))
                `(3))
  (check-equal? (join (λ (x) x) (λ (x) x) (λ (x y) (= 0 (modulo x y))) cons '(1 2 3 4 5) '(1 2 3 4 5))
                `((1 . 1) (2 . 1) (2 . 2) (3 . 1) (3 . 3) (4 . 1) (4 . 2) (4 . 4) (5 . 1) (5 . 5)))
  (check-equal? (join (λ (x) (car x)) (λ (x) (car x)) string=?
                      (λ (x y) (cons (car x) (cons (cddr x) (cdr y))))
                      (list (cons "Peter" (cons 18 #t)) (cons "Jack" (cons 12 #f)) (cons "Tom" (cons 29 #t)) (cons "Pat" (cons 19 #t)))
                      (list (cons "Sam" "Mac") (cons "Tom" "Dell") (cons "Jack" "Lenovo") (cons "Peter" "Toshiba") (cons "Tom" "Mac")))
                '(("Peter" #t . "Toshiba") ("Jack" #f . "Lenovo") ("Tom" #t . "Dell") ("Tom" #t . "Mac")))
  (check-equal? (join (λ (x) x) (λ (x) x) equal?
                      (λ (x y) (cons x y))
                      "helo" "helo")
                (map (λ (x) (cons x x)) (string->list "helo"))))

;; group-join: [A -> X] [B -> Y] [X Y -> Boolean] [A [Listof B] -> Z] [SequenceOf A] [SequenceOf B] -> [ListOf Z]
;; joins the given list based on the last function when the pred is satisfied.
;; the first two functions are used to extract values from A and B.
;; Note that first list is outer and second list is inner.
;; Unlike join, this function will pair every satisfied value from lb
;; as a list to the item in la.
(define (group-join a->x b->y xy->bool ab->z la lb)
  (for*/list [(a la)]
    (ab->z a (for/list ([b lb]
                        #:when (xy->bool (a->x a) (b->y b)))
               b))))

(module+ test
  (check-equal? (group-join (λ (x) x) (λ (x) x) (λ (x y) (= 0 (modulo x y))) (λ (x y) (list x y))'(1 2 3 4 5) '(1 2 3 4 5))
                `((1 (1)) (2 (1 2)) (3 (1 3))(4 (1 2 4)) (5 (1 5))))
  (check-equal? (group-join (λ (x) (car x)) (λ (x) (car x)) string=?
                            (λ (x y) (list (car x) (cddr x) (map cdr y)))
                            (list (cons "Peter" (cons 18 #t)) (cons "Jack" (cons 12 #f)) (cons "Tom" (cons 29 #t)) (cons "Pat" (cons 19 #t)))
                            (list (cons "Sam" "Mac") (cons "Tom" "Dell") (cons "Jack" "Lenovo") (cons "Peter" "Toshiba") (cons "Tom" "Mac")))
                '(("Peter" #t ("Toshiba")) ("Jack" #f ("Lenovo")) ("Tom" #t ("Dell" "Mac")) ("Pat" #t ()))))

;; where: [X -> Boolean] [SequenceOf X] -> [ListOf X]
;; sequence-filter
(define (where pred ls)
  (let [(ans (sequence-filter pred ls))]
    (if (list? ans) ans (sequence->list ans))))

(module+ test
  (check-equal? (where even? '(1 2 3 4 5)) (filter even? '(1 2 3 4 5)))
  (check-equal? (where even? 5) (filter even? '(0 1 2 3 4))))

;; orderby: [SequenceOf X] [NonEmptyListof [X X -> Boolean]]  -> [ListOf X]
;; sorts the given list by the order described by the given list of functions.
(define (orderby lx fs)
  (let ((cfs (compose-function (car fs) (cdr fs))))
    (sort (if (list? lx) lx (sequence->list lx)) cfs)))

(module+ test
  (check-equal? (orderby '((1 . 2) (2 . 3) (3 . 3) (1 . 1) (2 . 2))
                         `(,(λ (x y) (< (cdr x) (cdr y)))
                           ,(λ (x y) (< (car x) (car y)))))
                '((1 . 1) (1 . 2) (2 . 2) (2 . 3) (3 . 3)))
  
  (check-equal? (orderby '((1 . 2) (2 . 3) (3 . 3) (1 . 1) (2 . 2)) `(,(λ (x y) (> (car x) (car y))) ,(λ (x y) (< (cdr x) (cdr y)))))
                '((3 . 3) (2 . 2) (2 . 3) (1 . 1) (1 . 2))))

;; compose-functions: [X X -> Boolean] [Listof [X X -> Boolean]] -> [X X -> Boolean]
(define (compose-function f fs)
  (foldl (lambda (f acc)
           (lambda (x y) (or (acc x y) (and (not (acc y x)) (f x y)))))
         f fs))

(module+ test
  (define <cdr-then-<car (compose-function (λ (x y) (< (cdr x) (cdr y))) `(,(λ (x y) (< (car x) (car y))))))
  (check-true (<cdr-then-<car `(1 . 2) `(1 . 3)))
  (check-false (<cdr-then-<car `(2 . 2) `(1 . 2)))
  (check-true (<cdr-then-<car `(1 . 2) `(2 . 2)))
  (check-false (<cdr-then-<car `(1 . 2) `(1 . 2)))
  (define <cadr-then-<car-then->cddr (compose-function (λ (x y) (< (cadr x) (cadr y)))
                                                       `(,(λ (x y) (< (car x) (car y))) ,(λ (x y) (> (cddr x) (cddr y))))))
  (check-false (<cadr-then-<car-then->cddr '(1 2 . 3) '(1 2 . 4)))
  (check-true (<cadr-then-<car-then->cddr '(1 2 . 3) '(1 2 . 1)))
  (check-false (<cadr-then-<car-then->cddr '(1 2 . 3) '(1 1 . 1)))
  (check-true (<cadr-then-<car-then->cddr '(1 2 . 3) '(1 3 . 1)))
  (check-false (<cadr-then-<car-then->cddr '(2 2 . 3) '(1 2 . 1)))
  (check-true (<cadr-then-<car-then->cddr '(1 2 . 3) '(3 2 . 1))))

;; select: [X -> Y] [SequenceOf X] -> [ListOf Y]
;; sequence-map.
(define (select f ls)
  (let [(ans (sequence-map f ls))]
    (if (list? ans) ans (sequence->list ans))))

(module+ test
  (check-equal? (select add1 '(1 2 3)) (map add1 '(1 2 3)))
  (check-equal? (select list '(1 2 3)) '((1) (2) (3))))

;; select-many: [X -> [Sequence Y]] [X Y -> Z] [Sequence X] -> [ListOf Z]
;; This can be read like a nested for/list statement,
;; where x->y is applied to each element, x, in lx,
;; and xy->z is applied to each x and each (x->y x).
(define (select-many x->y xy->z lx)
  (for*/list [(x lx)
              (y (x->y x))]
    (xy->z x y)))

(module+ test
  (check-equal? (select-many (λ (x) x)
                             (λ (x y) y) '((1 2 3) (4 5 6) (7 8 9))) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (select-many (λ (x) x)
                             (λ (x y) (list y x)) '((1 2 3) (4 5 6) (7 8 9)))
                '((1 (1 2 3)) (2 (1 2 3)) (3 (1 2 3))
                              (4 (4 5 6)) (5 (4 5 6)) (6 (4 5 6))
                              (7 (7 8 9)) (8 (7 8 9)) (9 (7 8 9))))
  (check-equal? (select-many (λ (x) (list x))
                             (λ (x y) (map + x y)) '((1 2 3) (4 5 6) (7 8 9)))
                '((2 4 6) (8 10 12) (14 16 18)))
  (check-equal? (select-many (λ (x) (list (curry * 2)))
                             (λ (x y) (map y x)) '((1 2 3) (4 5 6) (7 8 9)))
                '((2 4 6) (8 10 12) (14 16 18)))
  (check-equal? (select-many (λ (x) (map (λ (x) (curry * x)) x))
                             (λ (x y) (map y x)) '((1 2 3) (4 5 6) (7 8 9)))
                '((1 2 3) (2 4 6) (3 6 9) (16 20 24) (20 25 30) (24 30 36) (49 56 63) (56 64 72) (63 72 81)))
  (check-equal? (select-many (λ (x) (map add1 x))
                             (λ (x y) (foldr + y x)) '((1 2 3) (4 5 6) (7 8 9)))
                '(8 9 10 20 21 22 32 33 34))
  (check-equal? (select-many (λ (x) (list x))
                             (λ (x y) (integer->char (+ (char->integer x) 3))) `"hello sup")
                '(#\k #\h #\o #\o #\r #\# #\v #\x #\s)))

;; groupby: [X -> Y] [X -> Z] [SequenceOf X] -> [ListOf (list Z [ListOf Y])]
;; groups item using x->z and applies x->y (the selection function) to the grouped items.
(define groupby
  (λ (x->y x->z lx)
    (into-assv (group-by x->z (if (list? lx) lx (sequence->list lx))) x->z x->y)))

(module+ test
  (check-equal? (groupby (λ (x) x) (λ (x) (modulo x 3)) '(1 2 3 4 5 6))
                '((1 (1 4)) (2 (2 5)) (0 (3 6))))
  (check-equal? (groupby (λ (x) x) even? '(1 2 3 4 5 6))
                '((#f (1 3 5)) (#t (2 4 6))))
  (check-equal? (groupby (λ (x) (cadr x))
                         (λ (x) (car x))
                         `((Turab Paulette) (Turab Joshua) (Paulette Turab) (Lalo Turab)
                                            (Dana Joe) (Joshua Lalo) (Jacob Nick) (Nick Lalo)
                                            (Sam Joshua) (Will Jacob) (Brie Dana) (Joe Joshua) (Joe Will)))
                '((Turab (Paulette Joshua)) (Paulette (Turab)) (Lalo (Turab))
                                            (Dana (Joe)) (Joshua (Lalo)) (Jacob (Nick))
                                            (Nick (Lalo)) (Sam (Joshua)) (Will (Jacob))
                                            (Brie (Dana)) (Joe (Joshua Will)))))

;; into-assv: [Listof [Listof X]] [X -> Y] [X -> Z]-> [ListOf (list Z [ListOf Y])]
;; takes the grouped items and transforms it into an association list with the key
;; of each group as the first element, and applies the selection function to the grouped items
(define (into-assv ls f x->y)
  (map (λ (pair) (let ((key (f (car pair))))
                   `(,key ,(map x->y pair))))
       ls))

(module+ test
  (check-equal? (into-assv '((1 4) (2 5) (3 6)) (λ (x) (modulo x 3)) (λ (x) x))
                '((1 (1 4)) (2 (2 5)) (0 (3 6))))
  (check-equal? (into-assv '((1 3 5) (2 4 6)) even? (λ (x) x))
                '((#f (1 3 5)) (#t (2 4 6))))
  (check-equal? (into-assv '(((Turab Paulette) (Turab Joshua)) ((Paulette Turab)) ((Lalo Turab))
                                                               ((Dana Joe)) ((Joshua Lalo)) ((Jacob Nick))
                                                               ((Nick Lalo)) ((Sam Joshua)) ((Will Jacob))
                                                               ((Brie Dana)) ((Joe Joshua) (Joe Will)))
                           (λ (x) (car x)) (λ (x) (cadr x)))
                '((Turab (Paulette Joshua)) (Paulette (Turab)) (Lalo (Turab))
                                            (Dana (Joe)) (Joshua (Lalo)) (Jacob (Nick))
                                            (Nick (Lalo)) (Sam (Joshua)) (Will (Jacob))
                                            (Brie (Dana)) (Joe (Joshua Will)))))

;                                                          
;                                                          
;   ;;;      ;                                             
;     ;                                                    
;     ;                                                    
;     ;    ;;;    ; ;;    ;;;;          ;;;   ;;;;    ;;;  
;     ;      ;    ;;  ;  ;   ;         ;   ;  ;   ;  ;   ; 
;     ;      ;    ;   ;  ;   ;         ;   ;  ;   ;  ;     
;     ;      ;    ;   ;  ;   ;         ;   ;  ;   ;   ;;;  
;     ;      ;    ;   ;  ;   ;         ;   ;  ;   ;      ; 
;     ;      ;    ;   ;  ;   ;         ;   ;  ;   ;  ;   ; 
;      ;;  ;;;;;  ;   ;   ;;;;          ;;;   ;;;;    ;;;  
;                            ;                ;            
;                            ;                ;            
;                            ;                ;            
                     

;; query-expression
;; (from Id in selExpr . QBody)
;; This is how users will initialize linq queries.
;; converts query-expression to (src . QBody), and passes it to handle-query-body
(define-syntax (from stx)
  (syntax-parse stx #:literals(in)
    [(_ var:id ~! in src:expr e e2 ...)
     (handle-query-body #'var #'src (env-add (make-env) #'var #'(λ (x) x)) #`(e e2 ...))]))

;; query-body-clause:
;; `(curr-src [(from var in src) |
;;             (join var in src on oExpr func iExpr) |
;;             (join var in src on oExpr func iExpr into var2) |
;;             (let var = expr) |
;;             (where expr) |
;;             (orderby (var func) (var2 func2) ...) |
;;             (select selExpr into var) |           These are query continuation statements.
;;             (group selExpr by keyExpr intp var)] ... final-query-clause)
;;  
;;  |
;;
;; final-query-clause
;; `(curr-src [(select selExpr) | (group selExpr by keyExpr)])

;; var is the first variable found in the linq query (this is then used to make bindings in lambda expressions.).
;; This is done to avoid binding issues. For example, in (from i in src ...), the user knows that i is referring
;; to the variable i in the linq query, therefore we don't have to worry about any i's bound outside of the linq query.
;; However, if we chose a temporary name, then we might shadow something in the environment bound with the same name.

;; src is the data structure obtained by the last query operation. It is initialized as the src
;; in the initial from statement.

;; env is a mapping each variable to it's `accessor` function. When a query introduces new variables,
;; this variable is consd on top of old value, and then adds the relevant function to access
;; that variable in the environment. It is initialized with var mapped to id (since our initital src is a
;; [ListOf τ] where var is of type τ. Eventually we get [ListOf (Pair τ1 τ)] where τ1 is type of some new
;; variable y. At this point, var is mapped to (cdr · id) and y is mapped to car. So, for `n` new variables
;; introduced after var, the var will be mapped to ((cdr × n) · id).

;; queries is just a list containing all query-body-clauses and final query clause.
(define-for-syntax (handle-query-body var src env queries)
  (syntax-parse queries
    [(e) (syntax-parse #'e #:literals (by group select)
           [(select selExpr:expr)
            ;; apply select on src with selExpr (map with selExpr on src).
            #`(select (λ (#,var) #,(insert-vars var #'selExpr env)) #,src)]
           [(group selExpr:expr ~! by keyExpr:expr)
            ;; apply groupby on src with selExpr where groups are made using keyExpr.
            #`(groupby (λ (#,var) #,(insert-vars var #'selExpr env))
                       (λ (#,var) #,(insert-vars var #'keyExpr env))
                       #,src)]
           [_ (raise-syntax-error 'linq-query "A query body must end with a select or a groupby clause." #'e)])]
    [(e1 e2 ...) (syntax-parse #'e1 #:literals (by let group in on into orderby from join select where) ;#:datum-literals (from join select)
                   [((~or select group) selExpr:expr (~optional (~seq by keyExpr:expr)))
                    (raise-syntax-error 'linq-query
                                        "Final query clauses (select or group) should only appear at the end of a query. Use a query continuation to avoid this."
                                        #'e1)]
                   [(from v:id in newSrc:expr)
                    ;; apply selectmany between src and newSrc, then add v to env as car and `add` cdr to previous bindings.
                    (define new-src (handle-inner-from src var #'newSrc #'v env))
                    (define new-env (env-add (update-bindings env) #'v car))
                    (handle-query-body var new-src new-env #'(e2 ...))]
                   [(join v:id in newSrc:expr on oExpr func iExpr)
                    ;; apply join between src and newSrc, then add v to env as car and `add` cdr to previous bindings.
                    (define new-src (handle-joins #'join src var #'newSrc #'v #'oExpr #'iExpr #'func env))
                    (define new-env (env-add (update-bindings env) #'v car))
                    (handle-query-body var new-src new-env #'(e2 ...))]
                   [(join v:id in newSrc:expr on oExpr func iExpr into v1:id)
                    ;; apply group-join between src and newSrc, then add v1 to env as car and `add` cdr to previous bindings.
                    (define new-src (handle-joins #'group-join src var #'newSrc #'v #'oExpr #'iExpr #'func env))
                    (define new-env (env-add (update-bindings env) #'v1 car))
                    (handle-query-body var new-src new-env #'(e2 ...))]
                   [(let v:id ~! newSrc:expr)
                    ;; apply join between src and newSrc, then add v to env as car and `add` cdr to previous bindings.
                    (define new-src #`(select (λ (#,var) (cons #,(insert-vars var #'newSrc env) #,var)) #,src))
                    (define new-env (env-add (update-bindings env) #'v car))
                    (handle-query-body var new-src new-env #'(e2 ...))]
                   [(where predExpr:expr)
                    ;; apply where on src (filter wrt predExpr).
                    (define new-src #`(where (λ (#,var) #,(insert-vars var #'predExpr env)) #,src))
                    (handle-query-body var new-src env #'(e2 ...))]
                   [(orderby (keyExpr:expr sFunc) (keyExpr2:expr sFunc2)...)
                    ;; apply orderby on src (sort using sFunc wrt keyExpr) ... .
                    (define new-src (handle-orderby src var #`((keyExpr sFunc) (keyExpr2 sFunc2) ...) env))
                    (handle-query-body var new-src env #'(e2 ...))]
                   [(select selExpr into ~! v:id)
                    ;; apply select on src, then add v to env as car and `add` cdr to previous bindings.
                    (handle-query-body #'v #`(select (λ (#,var) #,(insert-vars var #'selExpr env)) #,src)
                                       (env-add (make-env) #'v #'(λ (x) x))
                                       #'(e2 ...))]
                   [(group selExpr:expr by keyExpr:expr into ~! v:id)
                    ;; apply groupby on src with selExpr where groups are made using keyExpr,
                    (handle-query-body #'v 
                                       #`(groupby (λ (#,var) #,(insert-vars var #'selExpr env))
                                                  (λ (#,var) #,(insert-vars var #'keyExpr env))
                                                  #,src)
                                       (env-add (make-env) #'v #'(λ (x) x))
                                       #'(e2 ...))]
                   [else (raise-syntax-error 'linq-query
                                             "Not a proper query operation."
                                             #'else)])]))

(define-for-syntax (handle-inner-from outer-src outer-var inner-src inner-var env)
  (if (env-has-key? env inner-var)
      (raise-syntax-error 'from "Variable already bound in current linq query" inner-var)
      #`(select-many (λ _ #,inner-src)
                     (λ (x y) (cons y x))
                     #,outer-src)))

(define-for-syntax (handle-joins which-join outer-src outer-var inner-src inner-var o-expr i-expr join-func env)
  (if (env-has-key? env inner-var)
      (raise-syntax-error 'from "Variable already bound in current linq query" inner-var)
      #`(#,which-join (λ (#,outer-var) #,(insert-vars outer-var o-expr env))
                      (λ (#,inner-var) #,i-expr)
                      #,join-func (λ (x y) (cons y x))
                      #,outer-src #,inner-src)))

(define-for-syntax (handle-orderby src var order-ls env)
  #`(orderby #,src
             (list #,@(map (lambda (op) (pair->func op var env))
                           (syntax-e order-ls)))))

(define-for-syntax (pair->func p var env)
  (syntax-parse p
    [(expr func)
     (define expr* #`(lambda (#,var) #,(insert-vars var #'expr env)))
     #`(lambda (x y) (func (#,expr* x) (#,expr* y)))]))


;; Transforms linq variables to their respective function applications

;; var is variable introduced in the lambda expression with expr as its body.

;; expr is the expression that might contain bound variables that are unbound in
;; Racket's environment but are bound in Linq's environment.

;; env is the env (heh).

;; Each query clause has an expression, which usually refers to a variable unbound in its own context,
;; but it might have been introduced as a variable in some previous query clause.
;; To avoid Racket throwing an error, this function adds all variables in Linq's environment
;; to Racket's environment. For example, if `i` is bound to f in Linq's env, then insert-vars bounds
;; `i` to (f var) in Racket's env.

;; insert-vars: Id Syntax Env -> Syntax
;; creates a let expression, binding every variable in the environment
;; to its corresponding function applied to the id
(define-for-syntax (insert-vars var expr env)
  (foldr (λ (kv acc)
           #`(let ((#,(car kv) (#,(cdr kv) #,var))) #,acc)) expr env))


;; make-env: -> Env
(define-for-syntax (make-env)
  '())

;; env-add: Env Key Val -> Env
(define-for-syntax (env-add env k v)
  (cons (cons k v) env))

;; env-ref: Env Key -> Val
(define-for-syntax (env-ref env k)
  (let ((v (and (identifier? k) (assoc k env free-identifier=?))))
    (if v (cdr v) v)))

;; env-has-key?: Env Key -> Boolean
(define-for-syntax (env-has-key? env k)
  (foldr (λ (x y) (if (free-identifier=? (car x) k) #t y)) #f env))

;; env-add: Env -> [Listof Key]
(define-for-syntax (env-keys env)
  (map car env))

;; update-bindings: Env -> Env
;; for each key k in env (where k is mapped to f), this function
;; maps k to (f · cdr).
(define-for-syntax (update-bindings env)
  (for/list [(k (env-keys env))]
    (cons k #`(compose #,(env-ref env k) cdr))))

#;(define-syntax from-db
    (syntax-parser 
      [(_ usrname:string dbname:string query)
       (let [(db (postgresql-connect #:user (syntax-e #'usrname)
                                     #:database (syntax-e #'dbname)))]
         (foldr (λ (kv acc)
                  #`(let ((#,(datum->syntax #'query (car kv)) `#,@(cdr kv))) #,acc))
                #'query
                (map (λ (t) `(,(string->symbol t)
                              ,(map vector->list
                                    (query-rows db (string-append "select * from " t)))))
                     (list-tables db))))]))

;                                                                  
;                                                                  
;                                                                  
;   ;;                                                   ;         
;   ;;                                                   ;         
;   ;;                                                   ;         
;   ;;  ;;   ;;;;   ;    ; ;      ;  ;;;;   ;;  ;;   ;;; ;   ;;;;  
;   ;; ;;    ;  ;;  ;    ;  ; ;; ;  ;;  ;;   ;;; ;  ;;  ;;  ;;     
;   ;;;;    ;    ;   ;  ;   ; ;; ;  ;    ;   ;;  ;  ;    ;  ;;     
;   ;;;     ;;;;;;   ;  ;   ; ;; ;  ;    ;   ;;     ;    ;    ;;;  
;   ;; ;    ;        ;  ;   ; ;; ;  ;    ;   ;;     ;    ;      ;; 
;   ;;  ;    ;        ;;    ;;  ;;  ;;  ;;   ;;     ;;  ;;  ;   ;; 
;   ;;   ;   ;;;;     ;;    ;;  ;;   ;;;;   ;;;;     ;;; ;   ;;;;  
;                     ;;                                           
;                     ;                                            
;                   ;;                                             
;                                                                  

;; in
(define-syntax in
  (syntax-parser
    [_ #'(raise-syntax-error #f "in should only appear as a keyword in a from or join clause." #'in)]))

;; by
(define-syntax by
  (syntax-parser
    [_ #'(raise-syntax-error #f "by should only appear as a keyword in an orderby clause." #'in)]))

;; into
(define-syntax into
  (syntax-parser
    [_ #'(raise-syntax-error #f "into should only appear as a keyword in a query continuation expression." #'in)]))

;; on
(define-syntax on
  (syntax-parser
    [_ #'(raise-syntax-error #f "on should only appear as a keyword in a join expression." #'on)]))

;; group
(define-syntax group
  (syntax-parser
    [_ #'(raise-syntax-error #f "group clause should only appear in a linq query. You may want to use the groupby function instead." #'on)]))

