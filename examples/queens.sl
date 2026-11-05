;; The N-queens puzzle, by searching with lists. A solution is a list of column numbers, one for
;; each row placed so far, most recent first.

(define (safe? column placed)
  (define (check rest distance)
    (match rest
      (nil true)
      ((cons other more)
       (and (!= other column)
            (!= (abs (- other column)) distance)
            (check more (+ distance 1))))))
  (check placed 1))

;; Every way of placing queens on the first `row` rows of an n-by-n board.
(define (solutions n row)
  (if (= row 0)
      (list nil)
      (concat-map
       (lambda (placed)
         (map (lambda (column) (cons column placed))
              (filter (lambda (column) (safe? column placed))
                      (range 0 n))))
       (solutions n (- row 1)))))

(define (queens n) (solutions n n))

;; Draws one board, a line per row.
(define (draw n solution)
  (define (cell column queen)
    (if (= column queen) "Q" "."))
  (for-each
   (lambda (queen)
     (print (string-join " " (map (lambda (column) (cell column queen)) (range 0 n)))))
   solution))

(for-each
 (lambda (n)
   (print (string-join " " (list (show n) "queens:" (show (length (queens n))) "solutions"))))
 (range 1 9))

(print "")
(draw 6 (head (queens 6)))
