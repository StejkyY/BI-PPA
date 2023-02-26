
#lang racket

(define board                   
  '((0 0 8 0 6 2 0 0 0)
    (0 3 0 8 4 0 9 0 2)
    (9 0 6 0 0 0 0 1 4)
    (0 1 2 0 0 8 6 0 0)
    (3 0 0 0 7 9 0 2 0)
    (0 6 0 1 0 0 0 3 7)
    (0 0 1 7 8 0 3 0 0)
    (6 8 5 2 0 0 7 4 0)
    (4 0 0 0 9 6 0 0 1)))

(define (square board row col)
     ( flatten (extract-elements board (- row (modulo row 3)) (- col (modulo col 3)) ) ) ) 

(define ( extract-elements board a b )
  (cons
   ( take_3 ( nth board a ) b )
   ( cons
    ( take_3 ( nth board ( + a 1 ) ) b )
    ( take_3 ( nth board ( + a 2 ) ) b ))))

(define (take_3 lst n)
  (if (= 0 n)
      (cons (car lst) (cons (car ( cdr lst ) ) (car ( cdr ( cdr lst)))))
      (take_3 (cdr lst) (- n 1 ))))
        
(define (nth lst n)
  (if (= 0 n)
      (car lst)
      (nth (cdr lst) (- n 1 ))))

(define (in-col brd n)
  (map (lambda (lst) (nth lst n)) brd))

(define (find_el board r c)
  (nth (nth board r) c))

(define (in-list n lst)
  (if (null? lst)
      #false
      (if (list? lst)
          (if (= (car lst) n)
              #true
              (in-list n (cdr lst )))
          (if (= lst n)
              #true
              #false))))

(define (tmp lst n e)
   (if (= n 0)
       (cons e (cdr lst))
       (cons (car lst) (tmp (cdr lst) (- n 1) e))))

(define (change-element brd row col e)
  (if ( = row 0)
      (cons (tmp ( car brd ) col e ) (cdr brd ) )
      (cons (car brd) (change-element (cdr brd) ( - row 1) col e))))

(define (end board n)
  (when(> n 0)
    (println (car board))
    (end (cdr board) (- n 1) )))

(define (sudoku board)
  (if (not (check board 0 0))
      (println "incorrect input board")
      (sudoku-solver board 0 0)))

(define (number-control n brd row col)
  (if
     (not
        (or
            ( in-list n (square brd row col))
            ( in-list n (in-col brd col))
            ( in-list n (nth brd row))))
     #true
     #false))
  

(define (check brd row col)
  (if (= row 9)
      #true
      (if (not (= (find_el brd row col) 0))
         (if (not (number-control (find_el brd row col) (change-element brd row col 0) row col))
            #false
           (check brd (if (= col 8) (+ row 1) (+ row 0)) (if (= col 8) (- col 8) (+ col 1))))
      (check brd (if (= col 8) (+ row 1) (+ row 0)) (if (= col 8) (- col 8) (+ col 1))))))
       
       
   
(define (sudoku-solver brd row col)
  (if (= row 9)
      ( begin (println "SOLUTION: " ) (end brd 9))
      ( if (= (find_el brd row col) 0)
           (for ((i (range 1 10 )))
                    (when (not (or
                                ( in-list i (square brd row col))
                                ( in-list i (in-col brd col))
                                ( in-list i (nth brd row))))                     
                      (sudoku-solver (change-element brd row col i )
                                     ( if (= col 8)
                                          ( + row 1)
                                          (+ row 0 ) )
                                     ( if (= col 8) (- col 8) (+ col 1)))))
      (sudoku-solver brd (if (= col 8) (+ row 1) (+ row 0)) (if (= col 8) (- col 8) (+ col 1))))))
      
        
 (sudoku board)    
  
  
  
  

