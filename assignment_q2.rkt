#lang racket

;A
;Function that puts an element at the start of a list.
(define (ins_beg e list)
;Uses the cons function to put the element to the start of the list.
  (cons e list))
(ins_beg 'a '(b c d))
(ins_beg '(ab) '( c d))

;B
;Function that puts an element at the end of a list.
(define (ins_end e list)
;Uses the append function to put elements at the end of a list.
  (append list e))
(ins_end '(ab) '(cd))
(ins_end 'a '(b c d))

;C
;Function that counts the number of top level elements in a list
(define (count_top_level list)
  (cond
;This is the base case of the recursive function.
;When the end of the list is reached the function returns.
    [(empty? list) 0]
    [(+ 1 (count_top_level (cdr list)))]))

(count_top_level '(1 (2 3) 4 5))

;D
;Function that checks a list for the number of times
;an item occurs in the list, not using tail-recursion.
(define (count_instances e list)
;This is the base case of the recursive function.
;When the end of the list is reached the function returns
  (cond
    [(empty? list) 0]
;This compares e against the 1st element of the list
;and if they are the same +1 is added to the count.
;If they are not the same nothing is added to the count.
;The function is then called on the rest of the list.
    [(equal? e (car list)) (+ 1 (count_instances e (cdr list)))]
    [else (count_instances e (cdr list))]))

(count_instances 1 '(1 2 3 4))
(count_instances 1 '(1 1 2 3))
(count_instances 1 '(2 3 2 3))

;E
;Function that checks a list for the number of times
;an item occurs in the list, not using tail-recursion.
;This helper function initializes count to 0.
;Count is the variable which keeps track of the
;number of times an element occurs in a list.
(define (count_instances_tr e list)
  (tcount e list 0))


(define (tcount e list count)
  (cond
;If the end of the list is reached, the function
;returns the count.
    [(empty? list) count]
;e is compared against the 1st element of the list
;if they're the same 1 is added to the count and the function
;is called again on the rest of the list.
    [(equal? e (car list)) (+ 1 count (tcount e (cdr list) count))]      
;If e and the first element of the list are not the same the
;function just gets called again.
    [else (tcount e (cdr list) count)]))

(count_instances_tr 1 '(1 2 3 4))
(count_instances_tr 1 '(1 1 3 4))
(count_instances_tr 1 '(2 2 3 4))

;F
;Function that counts the number of times an element is in a list. It also checks any sublists for occurences of the element.
(define (count_instances_deep e list)
;Base case. Checks if the list is empty and if it is it returns.
  (cond [(empty? list) 0]
;Checks if the car of the list is a sublist and if it is performs the function on it and if the element being searched for is found it's added to the total. 
        [(list? (car list))
         (+ (count_instances_deep e (car list)) (count_instances_deep e (cdr list)))]
;These lines of code are identical to the ones in the first count instances function back in part D.
        [(equal? e (car list)) (+ 1 (count_instances_deep e (cdr list)))]
        [else (count_instances_deep e (cdr list))]))

(count_instances_deep 1 '(1 (2 3) 4))
(count_instances_deep 1 '(1 (1 (2 1 1)) 3))
