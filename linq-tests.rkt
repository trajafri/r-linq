#lang racket
(require r-linq rackunit)

;;; The following examples are based on the examples shown here:
;;; https://msdn.microsoft.com/en-us/library/bb308959.aspx

(define names `("Burke" "Connor" "Frank"
                        "Everett" "Albert" "George"
                        "Harris" "David"))

(check-equal? (from s in names
                    (where (= (string-length s) 5))
                    (orderby (s string<?))
                    (select (string-upcase s)))
              `("BURKE" "DAVID" "FRANK"))

; A Person is a (Person String int Boolean)
(struct Person [name age canCode])

(define people `(,(Person "Allen Frances" 11 false)
                 ,(Person "Your Mom" 59 false)
                 ,(Person "Burke Madison" 50 true)
                 ,(Person "Connor Morgan" 59 false)
                 ,(Person "David Charles" 33 true)
                 ,(Person "Everett Frank" 16 true)))

(check-equal? (from p in people
                    (where (> (Person-age p) 20))
                    (orderby ((Person-age p) >) ((Person-name p) string<?))
                    (select (list (Person-name p) (> (Person-age p) 30) (Person-canCode p))))
              '(("Connor Morgan" #t #f) ("Your Mom" #t #f) ("Burke Madison" #t #t) ("David Charles" #t #t)))

(check-equal? (from p in people
                    (where (> (Person-age p) 20))
                    (orderby ((Person-age p) >) ((Person-name p) string<?))
                    (group (list (Person-name p) (> (Person-age p) 30) (Person-canCode p)) 
                           by (Person-canCode p)))
              '((#f (("Connor Morgan" #t #f) ("Your Mom" #t #f))) (#t (("Burke Madison" #t #t) ("David Charles" #t #t)))))

(check-equal? (from s1 in names
                    (where (= (string-length s1) 5))
                    (from s2 in names)
                    (where (string=? s1 s2))
                    (select (string-append s1 " " s2)))
              '("Burke Burke" "Frank Frank" "David David"))

(check-equal? (from n in names
                    (join p in people on n string=? (Person-name p) into matching)
                    (select (list n (length matching))))
              `(("Burke" 0) ("Connor" 0) ("Frank" 0)
                            ("Everett" 0) ("Albert" 0) ("George" 0)
                            ("Harris" 0) ("David" 0)))

(define people2 `(,(Person "Allen" 11 false)
                  ,(Person  "Burke" 50 true)
                  ,(Person  "Connor" 59 false)
                  ,(Person  "David" 33 true)
                  ,(Person  "Everett" 16 true)))

(check-equal? (from n in names
                    (join p in people2 on n string=? (Person-name p) into matching)
                    (select (list n (length matching))))
              '(("Burke" 1) ("Connor" 1) ("Frank" 0)
                            ("Everett" 1) ("Albert" 0) ("George" 0)
                            ("Harris" 0) ("David" 1)))

(check-equal? (from item in names
                    (orderby (item string<?))
                    (group item by (string-length item) into lengthGroups)
                    (orderby ((car lengthGroups) >))
                    (select lengthGroups))
              '((7 ("Everett")) (6 ("Albert" "Connor" "George" "Harris")) (5 ("Burke" "David" "Frank"))))

;; Examples to test


;                              
;                              
;                              
;   ;;;;;; ;;;;;   ;;;;  ;    ;
;   ;      ;    ;  ;  ;  ;;  ;;
;   ;      ;    ; ;    ; ;;  ;;
;   ;      ;    ; ;    ; ; ;; ;
;   ;;;;;; ;;;;;  ;    ; ; ;; ;
;   ;      ;   ;  ;    ; ; ;; ;
;   ;      ;    ; ;    ; ;    ;
;   ;      ;    ;  ;  ;  ;    ;
;   ;      ;     ; ;;;;  ;    ;
;                              
;                              
;                              


;; multi-froms = cross product
(check-equal? (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (select i))
              (build-list 27 (λ _ 1)))

;; multi-froms = cross product
(check-equal? (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (select j))
              (build-list 27 (λ _ 2)))

;; multi-froms = cross product
(check-equal? (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (select k))
              (build-list 27 (λ _ 3)))

;; froms = select-many
(check-equal? (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (select (cons i (cons j k))))
              (select-many (λ _ (select-many (λ _ '(3 3 3)) cons '(2 2 2)))
                           cons '(1 1 1)))

(check-equal? (from x in '(1 2 3)
                    (where (> x 1))
                    (join y in '(4 5 6) on 1 = 1)
                    (select (+ x y)))
              (from x in '(1 2 3)
                    (where (> x 1))
                    (from y in '(4 5 6))
                    (select (+ x y))))

(check-equal? (from x in '(1 2 3)
                    (where (> x 1))
                    (from y in '(4 5 6))
                    (select (cons (add1 x) y)))
              '((3 . 4) (3 . 5) (3 . 6) (4 . 4)
                        (4 . 5) (4 . 6)))

(check-equal? (from y in 5
                    (where (> y 2))
                    (select (+ y y)))
              '(6 8))

;                              
;                              
;                              
;     ;;;   ;;;;  ;;;;;  ;;   ;
;       ;   ;  ;    ;    ;;   ;
;       ;  ;    ;   ;    ; ;  ;
;       ;  ;    ;   ;    ; ;  ;
;       ;  ;    ;   ;    ; ;; ;
;       ;  ;    ;   ;    ;  ; ;
;       ;  ;    ;   ;    ;  ; ;
;   ;   ;   ;  ;    ;    ;   ;;
;    ;;;    ;;;;  ;;;;;  ;   ;;
;                              
;                              
;                              

;; froms = join on true
(check-equal? (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (select (cons i (cons j k))))
              (from i in '(1 1 1)
                    (join j in '(2 2 2) on 1 = 1)
                    (join k in '(3 3 3) on 1 = 1)
                    (select (cons i (cons j k)))))

;; join tests
(check-equal? (from i in '(1 1 1)
                    (join j in '(2 2 2) on i > j)
                    (join k in '(3 3 3) on j > k)
                    (select (cons i (cons j k))))
              empty)

(check-equal? (from i in '(1 1 1)
                    (join j in '(2 2 2) on i < j)
                    (join k in '(3 3 3) on j > k)
                    (select (cons i (cons j k))))
              empty)

(check-equal? (from i in '(1 1 1)
                    (join j in '(2 2 2) on i > j)
                    (join k in '(3 3 3) on j < k)
                    (select (cons i (cons j k))))
              empty)

(check-equal? (from i in '(1 1 1)
                    (join j in '(2 2 2) on i < j)
                    (join k in '(3 3 3) on j < k)
                    (select (cons i (cons j k))))
              (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (select (cons i (cons j k)))))

(check-equal? (from i in '(1 1 1)
                    (join j in '(2 2 2) on i < j)
                    (join k in '(3 3 3) on j < k)
                    (select (cons i (cons j k))))
              (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (select (cons i (cons j k)))))

(check-equal? (from i in '(1 2 3)
                    (join j in '(2 3 4) on i = j)
                    (join k in '(3 4 5) on j = k)
                    (select (cons i (cons j k))))
              '((3 3 . 3)))

(check-equal? (from i in '(1 2 3)
                    (join j in '(2 3 4) on i < j into matches)
                    (select (cons i matches)))
              '((1 2 3 4) (2 3 4) (3 4)))

(check-equal? (from x in '(1 2 3)
                    (join y in '(4 5 6) on x (λ (_ y) (even? y)) y)
                    (select x))
              '(1 1 2 2 3 3))

(check-equal? (from x in '(1 2 3)
                    (join y in '(4 5 6) on x (λ (x y) (equal? y (+ x 3))) y)
                    (select x))
              '(1 2 3))

;                       
;                       
;                       
;   ;      ;;;;;;;;;;;;;
;   ;      ;        ;   
;   ;      ;        ;   
;   ;      ;        ;   
;   ;      ;;;;;;   ;   
;   ;      ;        ;   
;   ;      ;        ;   
;   ;      ;        ;   
;   ;;;;;; ;;;;;;   ;   
;                       
;                       
;                       

(check-equal? (from i in '(1 2 3)
                    (let j 2)
                    (select j))
              '(2 2 2))

(check-equal? (from i in '(1 2 3)
                    (from j in '(4 5 6))
                    (let ans (+ i j))
                    (select ans))
              '(5 6 7 6 7 8 7 8 9))

(check-equal? (from i in '(3 4)
                    (join j in '(4 5) on (add1 i) = j)
                    (let ans (* j 2))
                    (select (cons i (cons j ans)) into ans)
                    (select ans))
              '((3 4 . 8) (4 5 . 10)))

(check-equal? (from i in '(1 2 3 4)
                    (let j (+ i 2))
                    (orderby [j >])
                    (select j))
              '(6 5 4 3))


;                                     
;                                     
;                                     
;  ;     ; ;    ; ;;;;;; ;;;;;  ;;;;;;
;  ;  ;  ; ;    ; ;      ;    ; ;     
;  ;  ;  ; ;    ; ;      ;    ; ;     
;  ; ; ; ; ;    ; ;      ;    ; ;     
;  ; ; ; ; ;;;;;; ;;;;;; ;;;;;  ;;;;;;
;  ; ; ; ; ;    ; ;      ;   ;  ;     
;   ;; ;;  ;    ; ;      ;    ; ;     
;   ;   ;  ;    ; ;      ;    ; ;     
;   ;   ;  ;    ; ;;;;;; ;     ;;;;;;;
;                                     
;                                     
;                                     


;; where = filter
(check-equal? (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (where (< k 4))
                    (select i))
              (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (select i)))

(check-equal? (from i in '(1 1 1)
                    (from j in '(2 2 2))
                    (from k in '(3 3 3))
                    (where (< k 3))
                    (select i))
              empty)

(check-equal? (from x in '(1 2 3)
                    (where (> x 2))
                    (select (+ x 2)))
              '(5))

(check-equal? (from x in '(1 2 3)
                    (where (< x 2))
                    (select (+ x 3)))
              '(4))

;                                                   
;                                                   
;                                                   
;    ;;;;  ;;;;;  ;;;;   ;;;;;; ;;;;;  ;;;;; ;     ;
;    ;  ;  ;    ; ;   ;  ;      ;    ; ;    ; ;   ; 
;   ;    ; ;    ; ;    ; ;      ;    ; ;    ;  ; ;  
;   ;    ; ;    ; ;    ; ;      ;    ; ;    ;  ; ;  
;   ;    ; ;;;;;  ;    ; ;;;;;; ;;;;;  ;;;;;    ;   
;   ;    ; ;   ;  ;    ; ;      ;   ;  ;    ;   ;   
;   ;    ; ;    ; ;    ; ;      ;    ; ;    ;   ;   
;    ;  ;  ;    ; ;   ;  ;      ;    ; ;    ;   ;   
;    ;;;;  ;     ;;;;;   ;;;;;; ;     ;;;;;;    ;   
;                                                   
;                                                   
;                                                   

(check-equal? (from i in '(1 2 3)
                    (from j in '(1 2 1 2))
                    (orderby [i >] [j >])
                    (select (list i j)))
              `((3 2) (3 2) (3 1) (3 1) (2 2) (2 2) (2 1) (2 1) (1 2) (1 2) (1 1) (1 1)))

(define get-rand-list (λ (n) (build-list n (λ _ (random 10)))))

(for ([i 100])
  (let [(curr-list (get-rand-list 20))]
    (check-equal? (from i in curr-list
                        (from j in curr-list)
                        (orderby [j <] [i >])
                        (select (cons i j)))
                  (sort (for*/list ([i curr-list]
                                    [j curr-list])
                          (cons i j))
                        (λ (x y) (if (= (cdr x) (cdr y)) (> (car x) (car y)) (< (cdr x) (cdr y))))))))

(check-equal? (list->string
               (from i in "aljfuyawebfsa"
                (orderby [(char->integer i) <])
                (select i)))
              "aaabeffjlsuwy")

;                                            
;                                            
;                                            
;    ;;;;  ;;;;;; ;      ;;;;;;   ;;; ;;;;;;;
;   ;    ; ;      ;      ;       ;   ;   ;   
;   ;      ;      ;      ;      ;        ;   
;   ;;     ;      ;      ;      ;        ;   
;    ;;;;  ;;;;;; ;      ;;;;;; ;        ;   
;        ; ;      ;      ;      ;        ;   
;        ; ;      ;      ;      ;        ;   
;   ;    ; ;      ;      ;       ;   ;   ;   
;    ;;;;  ;;;;;; ;;;;;; ;;;;;;   ;;;    ;   
;                                            
;                                            
;

(check-equal? (from i in '(x (x y) ((λ (x) (x x)) (λ (y) (y y))) (λ (y) (y y)))
                    (select (match i
                              [`,x #:when (symbol? x) "symbol"]
                              [`(λ (,x) ,y) "lambda"]
                              [`(,x ,y) "application"])))
              '("symbol" "application" "application" "lambda"))

(check-equal? (from i in '(1 2 3)
                    (from j in '(4 5 6))
                    (select (+ i j) into sums)
                    (select sums))
              `(5 6 7 6 7 8 7 8 9))

(check-equal? (from i in '(1 2 3)
                    (from j in `(,(curry = 1) ,(curry = 2) ,(curry = 3)))
                    (select (j i)))
              `(#t #f #f #f #t #f #f #f #t))

(check-equal? (from i in '("1" "4" "notbanana" "(λ)" "banana" "λ")
                    (let banana? (curry string=? "banana"))
                    (select (banana? i) into ans)
                    (select ((λ (x) (if x "A Banana" "Not A Banana")) ans)))
              `("Not A Banana" "Not A Banana" "Not A Banana" "Not A Banana" "A Banana" "Not A Banana"))


;                                     
;                                     
;                                     
;     ;;;  ;;;;;   ;;;;  ;    ; ;;;;; 
;    ;   ; ;    ;  ;  ;  ;    ; ;    ;
;   ;      ;    ; ;    ; ;    ; ;    ;
;   ;      ;    ; ;    ; ;    ; ;    ;
;   ;   ;; ;;;;;  ;    ; ;    ; ;;;;; 
;   ;    ; ;   ;  ;    ; ;    ; ;     
;   ;    ; ;    ; ;    ; ;    ; ;     
;    ;   ; ;    ;  ;  ;  ;    ; ;     
;     ;;;  ;     ; ;;;;   ;;;;  ;     
;                                     
;                                     
;                                     

(check-equal? (from x in '(1 2 3 5 6 7 3 5 2 7 9)
                    (group x by (modulo x 3)))
              '((1 (1 7 7)) (2 (2 5 5 2)) (0 (3 6 3 9))))

(check-equal? (from x in '(1 2 3 5 6 7 3 5 2 7 9)
                    (group x by (even? x)))
              '((#f (1 3 5 7 3 5 7 9)) (#t (2 6 2))))

(check-equal? (from i in '(1 2 3 4 5 6 7 8 9)
                 (let k (odd? i))
                 (group i by k))
              '((#t (1 3 5 7 9)) (#f (2 4 6 8))))

(check-equal? (from i in '("hello" "there" "hi" "cow" "car" "cat" "dog" "test")
                 (let k (substring i 0 1))
                 (group i by k))
              '(("h" ("hello" "hi")) ("t" ("there" "test")) ("c" ("cow" "car" "cat")) ("d" ("dog"))))
