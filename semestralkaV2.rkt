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


(define board2
  '((0 2 4 0)
    (1 0 0 3)
    (4 0 0 2)
    (0 1 3 0)))

(define board3
  '(( 1 0 0 2 3 4 0 0 12 0 6 0 0 0 7 0 )
    (0 0 8 0 0 0 7 0 0 3 0 0 9 10 6 11 )
    (0 12 0 0 10 0 0 1 0 13 0 11 0 0 14 0)
    (3 0 0 15 2 0 0 14 0 0 0 9 0 0 12 0)
    (13 0 0 0 8 0 0 10 0 12 2 0 1 15 0 0)
    (0 11 7 6 0 0 0 16 0 0 0 15 0 0 5 13)
    (0 0 0 10 0 5 15 0 0 4 0 8 0 0 11 0)
    (16 0 0 5 9 12 0 0 1 0 0 0 0 0 8 0)
    (0 2 0 0 0 0 0 13 0 0 12 5 8 0 0 3)
    (0 13 0 0 15 0 3 0 0 14 8 0 16 0 0 0)
    (5 8 0 0 1 0 0 0 2 0 0 0 13 9 15 0)
    (0 0 12 4 0 6 16 0 13 0 0 7 0 0 0 5)
    (0 3 0 0 12 0 0 0 6 0 0 4 11 0 0 16)
    (0 7 0 0 16 0 5 0 14 0 0 1 0 0 2 0)
    (11 1 15 9 0 0 13 0 0 2 0 0 0 14 0 0)
    (0 14 0 0 0 11 0 2 0 0 13 3 5 0 0 12)))


(define (board-size lst (n 0) )
  (if (null? lst)
      n
      (board-size (cdr lst) (+ n 1))))
  
(define (square board row col size)
     (flatten (extract-elements board (- row (modulo row (sqrt size))) (- col (modulo col (sqrt size))) size 0 ) ) )

(define ( extract-elements board a b s n)
  (if (= n (sqrt s))
      '()
      (cons (take_s ( nth board a ) b s ) (extract-elements board (+ a 1) b s (+ n 1)))))

(define (helper lst s)
  (if (= s 0)
      (if (pair? lst) (car lst) '())
      (cons (car lst) (helper (cdr lst) (- s 1)))))

(define (take_s lst n s)
  (if (= 0 n)
      (helper lst (- (sqrt s) 1))
      (take_s (cdr lst) (- n 1 ) s)))
        
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
  (if (not (check board 0 0 ( board-size board) ))
      (println "incorrect input board")
      (sudoku-solver board 0 0 ( board-size board))))

(define (number-control n brd row col size)
  (if
     (not
        (or
            ( in-list n (square brd row col size))
            ( in-list n (in-col brd col))
            ( in-list n (nth brd row))))
     #true
     #false))
  

(define (check brd row col size)
  (if (= row size)
      #true
      (if (not (= (find_el brd row col) 0))
         (if (not (number-control (find_el brd row col) (change-element brd row col 0) row col size))
            #false
           (check brd (if (= col (- size 1)) (+ row 1) (+ row 0)) (if (= col (- size 1)) (- col (- size 1)) (+ col 1)) size))
      (check brd (if (= col (- size 1)) (+ row 1) (+ row 0)) (if (= col (- size 1)) (- col (- size 1)) (+ col 1)) size))))
   
(define (sudoku-solver brd row col brd-size)
  (if (= row brd-size)
      ( begin (println "SOLUTION: " ) (end brd brd-size))
      ( if (= (find_el brd row col) 0)
           (for ((i (range 1 (+ brd-size 1 ))))
                    (when (not (or
                                ( in-list i (square brd row col brd-size))
                                ( in-list i (in-col brd col))
                                ( in-list i (nth brd row))))
                      (sudoku-solver (change-element brd row col i )
                                     ( if (= col (- brd-size 1 ))
                                          ( + row 1)
                                          (+ row 0 ) )
                                     ( if (= col (- brd-size 1 )) (- col (- brd-size 1 )) (+ col 1))
                                     brd-size)))
      (sudoku-solver brd
                     (if (= col (- brd-size 1 )) (+ row 1) (+ row 0))
                     (if (= col (- brd-size 1 )) (- col (- brd-size 1 )) (+ col 1))
                     brd-size))))
      
(sudoku board)
(sudoku board2)
(sudoku board3)
  
  
  

