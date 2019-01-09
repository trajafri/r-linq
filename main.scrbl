#lang scribble/manual

@(require (for-label r-linq
                     (except-in racket/base let))
          scribble/example)

@(define (make-linq-eval)
   (make-base-eval #:lang 'racket/base
                   '(require r-linq)))

@(define-syntax-rule (linq-examples b ...)
   (examples #:eval (make-linq-eval) b ...))

@title{R-LINQ: .NET's LINQ in Racket}

source code: @url["https://github.com/trajafri/r-linq"]

@defmodule[r-linq]
@author[@author+email["Turab Jafri" "syajafri@iu.edu"]
        @author+email["Paulette Koronkevich" "pkoronke@iu.edu"]]

This package provides a simple implementation of
@hyperlink["https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/"]{.NET's LINQ}
in Racket.

@section{Query Syntax}
LINQ's grammar (as described @hyperlink["https://msdn.microsoft.com/en-us/library/bb308959.aspx"]{here})
is represented in the Racket language as follows:

@racketgrammar*[
 #:literals(from join let where orderby select group by in on into)
 [query-expression (from var in src
                     query-body-clause ...
                     final-query-clause)]
 [query-body-clause
  (from var in src)
  (join var in src on expr comp-func expr)
  (join var in src on expr comp-func expr into var)
  (let var expr)
  (where expr)
  (orderby [expr comp-func] ...+)
  (select expr into var)
  (group expr by expr into var)]
 [final-query-clause (select expr) (group expr by expr)]]

A @racket[_var] is a sequence of characters that can be used as a variable in the Racket language.

A @racket[_src] is an expression that evaluates to a @racket[sequence].

A @racket[_expr] is a Racket expression.

A @racket[_comp-func] is a Racket procedure that can compare two values.

@section{Standard LINQ Query Operators}

Following operators can be used in a LINQ query body (a query body must terminate with final query operator).
The documentation below is based on the
@hyperlink["https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/query-keywords"]{
 official documentation}:

@defform[#:kind "final query operator" (select expr)]{
 In a query expression, the select clause specifies the values that will be produced when the query is
 executed. The result is based on the evaluation of all the previous clauses and on the expressions in
 the @racket[select] clause itself.

 @(linq-examples
   (eval:check (from i in '(1 2 3)
                 (select i)) '(1 2 3))
   (eval:check (from i in '(1 2 3)
                 (select (+ i 3))) '(4 5 6))
   (eval:check (from i in (list (λ (x) (+ x 3))
                                (λ (x) (+ x 4))
                                (λ (x) (+ x 5)))
                 (select (i 1))) '(4 5 6)))

 Since a select operator finalizes a query, it might be helpful to post-process the results
 of a select clause. To do this, the @racket[into] form of select can be used:

 @(linq-examples
   (eval:check (from i in '(1 2 3)
                 (select i into j)
                 (select j))
               '(1 2 3))
   (eval:check (from i in '(1 2 3)
                 (select (+ i 3) into j)
                 (select (- j 3)))
               '(1 2 3)))
 
 More usage of @racket[select] can be seen under other query operator's examples.}

@defform[#:kind "final query operator" #:literals (by) (group expr by expr)]{
 The group clause returns a sequence of @racket[(list X (listof Y))] that contain zero or more items that match the
 key value for the group. For example, you can group a sequence of strings according to the first letter in
 each string. In this case, the first letter is the key, and is the X type of
 @racket[(list X (listof Y))] and the grouped items are of type Y.

 @(linq-examples
   (eval:check (from i in '("hello" "there" "hi" "cow" "car" "cat" "dog" "test")
                 (group i by (substring i 0 1)))
               '(("h" ("hello" "hi")) ("t" ("there" "test")) ("c" ("cow" "car" "cat")) ("d" ("dog"))))
   (eval:check (from i in '(1 2 3 4 5 6 7 8 9)
                 (group i by (modulo i 3)))
               '((1 (1 4 7)) (2 (2 5 8)) (0 (3 6 9))))
   (eval:check (from i in '(1 2 3 4 5 6 7 8 9)
                 (group i by (even? i)))
               '((#f (1 3 5 7 9)) (#t (2 4 6 8))))
   (eval:check (from i in '(1 2 3 4 5 6 7 8 9)
                 (group (+ i 1) by (even? i)))
               '((#f (2 4 6 8 10)) (#t (3 5 7 9))))
   (eval:check (from friendship in `((Turab Paulette) (Turab Joshua) (Paulette Turab) (Lalo Turab)
                                                      (Dana Joe) (Joshua Lalo) (Jacob Nick) (Nick Lalo)
                                                      (Sam Joshua) (Will Jacob) (Brie Dana) (Joe Joshua) (Joe Will))
                 (group (cadr friendship) by (car friendship)))
               '((Turab (Paulette Joshua)) (Paulette (Turab)) (Lalo (Turab))
                                           (Dana (Joe)) (Joshua (Lalo)) (Jacob (Nick))
                                           (Nick (Lalo)) (Sam (Joshua)) (Will (Jacob))
                                           (Brie (Dana)) (Joe (Joshua Will)))))

 Similar to @racket[select], we can post-process the results of a group clause by using an @racket[into] form:

 @(linq-examples
   (eval:check (from i in '("hello" "there" "hi" "cow" "car" "cat" "dog" "test")
                 (group i by (substring i 0 1) into groups)
                 (select (cdr groups) into res)
                 (select (car res)))
               '(("hello" "hi") ("there" "test") ("cow" "car" "cat") ("dog")))
   (eval:check (from i in '(1 2 3 4 5 6 7 8 9)
                 (group i by (even? i) into groups)
                 (select (cadr groups)))
               '((1 3 5 7 9) (2 4 6 8))))}

@defform[#:kind "query operator" (let var expr)]{
 In a query expression, it is sometimes useful to store the result of a sub-expression in order to use it in
 subsequent clauses. You can do this with the let keyword, which creates a new range variable and initializes
 it with the result of the expression you supply.

 @(linq-examples
   (eval:check (from i in '("hello" "there" "hi" "cow" "car" "cat" "dog" "test")
                 (let k (substring i 0 1))
                 (group i by k))
               '(("h" ("hello" "hi")) ("t" ("there" "test")) ("c" ("cow" "car" "cat")) ("d" ("dog"))))
   (eval:check (from i in '(1 2 3)
                 (let j (+ i 1))
                 (select j)) '(2 3 4))
   (eval:check (from i in '(1 2 3)
                 (let k add1)
                 (select (k i))) '(2 3 4))
   (eval:check (from i in '(1 2 3)
                 (let k (> i 1))
                 (select k)) '(#f #t #t))
   (eval:check (from i in '(1 2 3 4 5 6 7 8 9)
                 (let k (odd? i))
                 (group i by k)) '((#t (1 3 5 7 9)) (#f (2 4 6 8)))))
}

@defform[#:kind "query operator" (where expr)]{
 The where clause is used in a query expression to specify which elements from the data source will be
 returned in the query expression. It returns those for which the specified condition is true. A single
 query expression may contain multiple where clauses.

 @(linq-examples
   (eval:check (from i in '(1 2 3)
                 (where (< i 2))
                 (select i))
               '(1))
   (eval:check (from i in '(1 2 3)
                 (where (> i 1))
                 (select (+ i 3)))
               '(5 6))
   (eval:check (from i in (list (λ (x) (+ x 3))
                                (λ (x) (+ x 4))
                                (λ (x) (+ x 5)))
                 (where (>= (i 1) 5))
                 (select (i 1))) '(5 6)))
 More usage of @racket[where] can be seen under other query operator's examples.
}

@defform[#:kind "query operator" #:literals (in) (from var in src)]{
 The from clause specifies the following:

 @itemlist[
 @item["The data source on which the query or sub-query will be run."]
 @item["A local range variable that represents each element in the source sequence."]]

 After initializing a query, the mentioned form of from can be used anywhere in a  query body clause
 to introduce a new data source. The data source referenced in the from clause must be sequence.

 @(linq-examples
   (eval:check (from i in '(1 2 3)
                 (from j in '(4 5 6))
                 (select (list i j)))
               '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)))
   (eval:check (from i in '(1 2 3)
                 (from j in '(4 5 6))
                 (from k in '(1 1 1))
                 (select k))
               (build-list (* 3 3 3) (λ (_) 1)))
   (eval:check (from i in (list 1 2 3)
                 (from j in (list 3 4 5))
                 (where (= (+ i j) 5))
                 (select (list i j)))
               '((1 4) (2 3)))
   (eval:check (from i in (list 1 2 3)
                 (where (> i 1))
                 (from j in '(4 5 6))
                 (where (> j 5))
                 (select (list i j)))
               '((2 6) (3 6)))
   (eval:check (letrec [(! (λ (x) (if (zero? x)
                                      1
                                      (* (! (sub1 x)) x))))]
                 (from i in (list 1 2 3)
                   (let fact (! i))
                   (from j in '(4 5 6))
                   (where (> j 4))
                   (select (list fact j))))
               '((1 5) (1 6) (2 5) (2 6) (6 5) (6 6))))}

@defform[#:kind "query operator" #:literals (in on) (join var in src on expr comp-func expr)]{
 The join clause is useful for associating elements from different source sequences that have no direct
 relationship. For example, a food distributor might have a list of suppliers of a
 certain product, and a list of buyers. A join clause can be used, for example, to create a list of the
 suppliers and buyers of that product who are all in the same specified region.

 A join clause takes two source sequences as input. The elements in each sequence must contain a property
 that can be compared using the function passed in the join clause. The expr on the right
 can not contain any range variable other than the one being introduced. The expr on the left
 can contain any range variable other than the one being introduced.
 
 @(linq-examples
   (eval:check (from i in '(1 2 3)
                 (join j in '(4 5 6) on 1 = 1)
                 (select (list i j)))
               '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)))
   (eval:check (from i in '(1 2 3)
                 (from j in '(4 5 6))
                 (join k in '(1 1 1) on i > (+ 1 k))
                 (select (list k j)))
               '((1 4) (1 4) (1 4) (1 5) (1 5) (1 5) (1 6) (1 6) (1 6)))
   #;(eval:error (from i in '(1 2 3)
                   (from j in '(4 5 6))
                   (join k in '(1 1 1) on k < i)
                   (select k))
                 '())
   (eval:check (from fship in `((Turab Paulette) (Turab Joshua) (Paulette Turab)
                                                 (Lalo Turab) (Dana Joe) (Joshua Lalo)
                                                 (Jacob Nick) (Nick Lalo) (Sam Joshua)
                                                 (Will Jacob) (Brie Dana) (Joe Joshua) (Joe Will))
                 (join j in '(Turab Paulette Joshua Lalo) on (car fship) equal? j)
                 (select (list j (cadr fship))))
               '((Turab Paulette) (Turab Joshua) (Paulette Turab) (Lalo Turab) (Joshua Lalo)))
   (eval:check (from person in (list (cons "Peter" (cons 18 #t))
                                     (cons "Jack" (cons 12 #f))
                                     (cons "Tom" (cons 29 #t))
                                     (cons "Pat" (cons 19 #t)))
                 (join comp-owner in (list (cons "Sam" "Mac")
                                           (cons "Tom" "Dell")
                                           (cons "Jack" "Lenovo")
                                           (cons "Peter" "Toshiba")
                                           (cons "Tom" "Mac")) on (car person)
                                                               string=?
                                                               (car comp-owner))
                 (select (list (car person) (cddr person) (cdr comp-owner))))
               '(("Peter" #t "Toshiba") ("Jack" #f "Lenovo") ("Tom" #t "Dell") ("Tom" #t "Mac"))))

 To perform a group-join, you can use the @racket[into] form of join:

 @(linq-examples
   (eval:check (from person in (list (cons "Peter" (cons 18 #t))
                                     (cons "Jack" (cons 12 #f))
                                     (cons "Tom" (cons 29 #t))
                                     (cons "Pat" (cons 19 #t)))
                 (join comp-owner in (list (cons "Sam" "Mac")
                                           (cons "Tom" "Dell")
                                           (cons "Jack" "Lenovo")
                                           (cons "Peter" "Toshiba")
                                           (cons "Tom" "Mac")) on (car person)
                                                               string=?
                                                               (car comp-owner) into groups)
                 (select (list (car person) (cddr person) (map cdr groups))))
               '(("Peter" #t ("Toshiba")) ("Jack" #f ("Lenovo")) ("Tom" #t ("Dell" "Mac")) ("Pat" #t ()))))
}

@defform[#:kind "query operator" (orderby [key-expr comp-func] ...+)]{
 In a query expression, the orderby clause causes the returned sequence to be sorted. Multiple keys can be
 specified in order to perform one or more secondary sort operations. The sorting is performed by the function
 associated to each expr. The function should not contain any range variables introduced in the linq query.

 @(linq-examples
   (eval:check (from i in '(1 2 3)
                 (orderby [i >])
                 (select i))
               '(3 2 1))
   (eval:check (from i in '(1 2 3)
                 (from j in '(1 2 1 2))
                 (orderby [i >] [j >])
                 (select (list i j)))
               '((3 2) (3 2) (3 1) (3 1) (2 2) (2 2) (2 1) (2 1) (1 2) (1 2) (1 1) (1 1)))
   (eval:check (from i in '(1 2 3)
                 (from j in '(1 2 1 2))
                 (orderby [i >] [j <])
                 (select (list i j)))
               '((3 1) (3 1) (3 2) (3 2) (2 1) (2 1) (2 2) (2 2) (1 1) (1 1) (1 2) (1 2))))
}

@section{Query Functions}

The operators above are implemented using the following functions that are also provided by @racket[r-linq].
The documentation below is based on the
@hyperlink["https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/
           query-expression-syntax-for-standard-query-operators"]{
 official documentation}:

@defproc[(select [sel-func (X -> Y)] [src (sequenceof X)]) (listof Y)]{
 Projects each element of a sequence into a new form.

 @(linq-examples
   (eval:check (select (λ (x) (+ x 2)) '(1 2 3))
               '(3 4 5))
   (eval:check (select add1 4)
               '(1 2 3 4))
   (eval:check (select (λ (x) x) "hello")
               '(#\h #\e #\l #\l #\o)))
}

@defproc[(select-many [sel-func (X -> Y)] [res-func (X Y -> Z)] [src (sequenceof X)]) (listof Z)]{
 Projects each element of the src sequence and the sequence returned by @racket[sel-func] and flattens
 the resulting sequences into one sequence. One can think about this like an abstraction over a
 nester for loop, where for each X in src, and for each Y in @racket[(sel-func X)], a list containing
 @racket[(res-func X Y)] is produced.

 @(linq-examples
   (eval:check (select-many (λ (x) '(4 5 6)) list '(1 2 3))
               '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)))
   (eval:check (select-many add1 list 3)
               '((0 0) (1 0) (1 1) (2 0) (2 1) (2 2)))
   (eval:check (select-many list
                            (λ (x y) (map + x y)) '((1 2 3) (4 5 6) (7 8 9)))
               '((2 4 6) (8 10 12) (14 16 18))))}

@defproc[(where [pred (X -> boolean)] [src (sequenceof X)]) (listof X)]{
 Filters a sequence of values based on a predicate.

 @(linq-examples
   (eval:check (where even? '(1 2 3 4 5))
               '(2 4))
   (eval:check (where (compose even? char->integer) "hello")
               '(#\h #\l #\l))
   (eval:check (where (λ (x) (> (length x) 3)) '(() (1 2) (3 2 4 3 1) (1 3 2 4) (12 3 1)))
               '((3 2 4 3 1) (1 3 2 4))))}

@defproc[(groupby [sel-func (X -> Y)] [part-by-func (X -> Z)] [src (sequenceof X)]) (listof (list Z (listof Y)))]{
 Groups the elements of a sequence. The grouping is done based on the @racket[part-by-func] function, and
 the @racket[sel-func] is applied to each grouped item.

 @(linq-examples
   (eval:check (groupby (λ (x) x) (λ (x) (modulo x 3)) '(1 2 3 4 5 6))
               '((1 (1 4)) (2 (2 5)) (0 (3 6))))
   (eval:check (groupby add1 even? '(1 2 3 4 5 6))
               '((#f (2 4 6)) (#t (3 5 7)))))
}

@defproc[(join [outer-sel-func (A -> X)] [inner-sel-func (B -> Y)] [sel-when (X Y -> boolean)]
               [sel-func (A B -> Z)] [outer-src (sequenceof A)] [inner-src (sequenceof B)]) (listof Z)]{
 Correlates the elements of two sequences based on matching keys. The join is based on the @racket[sel-when]
 function (with @racket[outer-sel-func] and @racket[inner-sel-func] applied), and the resulting list contains
 results of @racket[sel-func] function.

 @(linq-examples
   (eval:check (join (λ (x) x) (λ (x) x)
                     (λ (x y) (= 0 (modulo x y))) list
                     '(1 2 3 4 5) '(1 2 3 4 5))
               `((1 1) (2 1) (2 2) (3 1) (3 3) (4 1) (4 2) (4 4) (5 1) (5 5)))
   (eval:check (join (λ (x) x) (λ (x) x)
                     equal?
                     (λ (x y) (cons x y))
                     "helo" "helo")
               (map (λ (x) (cons x x)) (string->list "helo"))))
}

@defproc[(group-join [outer-sel-func (A -> X)] [inner-sel-func (B -> Y)] [sel-when (X Y -> boolean)]
                     [sel-func (A (listof B) -> Z)] [outer-src (sequenceof A)] [inner-src (sequenceof B)]) (listof Z)]{
 Correlates the elements of two sequences based on key equality, and groups the results. Similar to join,
 but groups all matched items in inner-src in a list.

 @(linq-examples
   (eval:check (group-join (λ (x) x) (λ (x) x)
                           (λ (x y) (= 0 (modulo x y)))
                           (λ (x y) (list x y))
                           '(1 2 3 4 5) '(1 2 3 4 5))
               `((1 (1)) (2 (1 2)) (3 (1 3))(4 (1 2 4)) (5 (1 5)))))
}

@defproc[(orderby [src (sequenceof X)] [sort-seq (nelistof [X X -> boolean])]) (listof X)]{
 Sorts the elements of a sequence with the comparator functions. When provided more than one
 function, the function is first sorted by the first function, then items that were in the
 same ordering by the second function, and so on...

 @(linq-examples
   (eval:check (orderby '((1 . 2) (2 . 3) (3 . 3) (1 . 1) (2 . 2))
                        (list (λ (x y) (> (cdr x) (cdr y)))
                              (λ (x y) (< (car x) (car y)))))
               '((2 . 3) (3 . 3) (1 . 2) (2 . 2) (1 . 1)))
   (eval:check (orderby '((1 . 2) (2 . 3) (3 . 3) (1 . 1) (2 . 2))
                        (list (λ (x y) (> (car x) (car y)))
                              (λ (x y) (< (cdr x) (cdr y)))))
               '((3 . 3) (2 . 2) (2 . 3) (1 . 1) (1 . 2))))}

@section{Literals}

These are the literals used in linq query operators.

@defidform[in]{
 Literal used in a @racket[from] clause.
}

@defidform[on]{
 Literal used in a @racket[join] clause.
}

@defidform[into]{
 Literal used in a @racket[join] or continuation clause.
}

@defidform[by]{
 Literal used in a @racket[group] clause.
}
