#!/usr/bin/env gosh
(use srfi-27)
(use srfi-42)
(use srfi-43)
(use tk)

(define *allowed-commands* '(w a s d b))
(define *allowed-directions* '(w a s d ))
(define *allowed-fall-time* '(1 2 3 4 5 6))
(define *player-num* 2) 
(define *board-width* 6)
(define *board-height* 6)
(define *board-size* (* *board-width* *board-height*))
(define *limit-time* 60)
(define *sud-time* 100)
(define *random-mode* 0)
(define *non-timed-floor* 1000)
(define *timed-floor* -4)
(define (player-symb player)
  (cond
   ((= player 0) '| |)
   ((= player 1) '!)
   ((= player 2) '@)
   ((= player 3) '$)
   ((= player 4) '%)
   (else p)))
(define (floor-symb floor)
  (cond
   ((= floor *non-timed-floor*) '+)
;   ((= floor *timed-floor*) '-)
   (else floor)))
  
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (nxy n)
  (list (mod n *board-width*) (quotient n *board-height*)))
(define (xyn x y)
  (+ x (* y *board-width*)))

(define (gen-board) 
  (vector-ec
   (: i *board-size*)
   (vector *non-timed-floor* 0)))
  
(define (board-init board)
  (let 
      ((new-board (vector-copy board)))
    (cond
     ((eq? *random-mode* 0)
      (cond
       ((eq? *player-num* 2)
        (vector-set! new-board 0 (vector *non-timed-floor* 1))
        (vector-set! new-board  (- (vector-length new-board) 1) (vector *non-timed-floor* 2)) new-board))))))
   ;; ((eq? *random-mode* 1)))))))
      ;; (letrec
      ;;     ((randomize 
      ;;       (lambda (pl-num lst)
      ;;         (if (zero? pl-num) '() 
      ;;   	  (let 
      ;;   	      ((r (random-integer (length lst)))
      ;;   	       (elt (list-ref r lst))
      ;;   	       ((rem (delete elt lst)))			 			  (cons elt (randomize (dec pl-num) rem))))))))
      ;;   (let ()
      ;;     (randomize *pl-num* (iota (* *row-num* *col-num*) 0))
      ;;     (set! state-fall (iota 9 0 *max-fall-time*))))))

(define (initiative-init)
  (list-ec (: i *player-num*) (inc i)))

(define (find-players-pos players board)
  (map (lambda (p) (find-player-pos p board)) players))

(define (find-player-pos player board)
  (let
      ((f (lambda (x) (= (vector-ref x 1) player))))
    (vector-index f board)))

(define (rotate lst)
   (if (null? lst) '() (append (cdr lst) (list (car lst)))))

(define (move direction player board)
  (let*
     ((new-board (vector-copy board))
      (now-pos-n (find-player-pos player new-board))
      (now-pos-x (car (nxy now-pos-n)))
      (now-pos-y (cadr (nxy now-pos-n)))
      (now-vector (vector-ref new-board now-pos-n))
      (ok? (lambda (f p) (and (= p 0) (< 0 f))))
      (update (lambda (next) (vector-set! next 1 player) (vector-set! now-vector 1 0))))
    (cond 
     ((eq? direction 'w)
      (let*
          ((next-pos-x now-pos-x)
           (next-pos-y (dec now-pos-y))
           (next-pos-n (xyn next-pos-x next-pos-y)))
        (if (<= 0 next-pos-y)
              (let*
                  ((next-vector (vector-ref new-board next-pos-n))
                   (next-pos-floor (vector-ref next-vector 0))
                   (next-pos-player (vector-ref next-vector 1)))
                (if (ok? next-pos-floor next-pos-player)
                    (begin
                      (update next-vector)
                      new-board)
                    (begin
                      (print "cannot move up")
                      #f))) #f)))
     ((eq? direction 's)
      (let*
          ((next-pos-x now-pos-x)
           (next-pos-y (inc now-pos-y))
           (next-pos-n (xyn next-pos-x next-pos-y)))
        (if (< next-pos-y *board-height*)
            (let*
                ((next-vector (vector-ref new-board next-pos-n))
                 (next-pos-floor (vector-ref next-vector 0))
                 (next-pos-player (vector-ref next-vector 1)))
              (if (ok? next-pos-floor next-pos-player)
                    (begin
                      (update next-vector)
                      new-board)
                    (begin
                      (print "cannot move down")
                      #f))) #f)))
     ((eq? direction 'a)
      (let*
          ((next-pos-x (dec now-pos-x))
           (next-pos-y now-pos-y)
           (next-pos-n (xyn next-pos-x next-pos-y)))
        (if (<= 0 next-pos-x)
            (let*
                 ((next-vector (vector-ref new-board next-pos-n))
                  (next-pos-floor (vector-ref next-vector 0))
                  (next-pos-player (vector-ref next-vector 1)))
              (if (ok? next-pos-floor next-pos-player)
                  (begin
                    (update next-vector)
                    new-board)
                  (begin
                    (print "cannot move left")
                    #f))) #f)))
     
     ((eq? direction 'd)
      (let*
          ((next-pos-x (inc now-pos-x))
           (next-pos-y now-pos-y)
           (next-pos-n (xyn next-pos-x next-pos-y)))
        (if (< next-pos-x *board-width*)
            (let*
                ((next-vector (vector-ref new-board next-pos-n))
                 (next-pos-floor (vector-ref next-vector 0))
                 (next-pos-player (vector-ref next-vector 1)))
              (if (ok? next-pos-floor next-pos-player)
                  (begin
                    (update next-vector)
                    new-board)
                  (begin
                    (print "cannot move right")
                    #f)))#f)))
     (else
      (print "cannot hundle the direction")
      #f))))

(define (beam direction fall-time player board)
  (let* 
      ((cell-pos
        (let*
            ((pos (find-player-pos player board))
             (x (car (nxy pos)))
             (y (cadr (nxy pos))))
          (cond
           ((eq? direction 'w)
            (list-ec (: next-y (- y 1) -1 -1) (xyn x next-y)))
           ((eq? direction 's)
            (list-ec (: next-y (+ y 1) *board-height*) (xyn x next-y)))
           ((eq? direction 'a)
            (list-ec (: next-x (- x 1) -1 -1) (xyn next-x y)))
           ((eq? direction 'd)
            (list-ec (: next-x (+ x 1) *board-width*) (xyn next-x y))))))
       (already-fallen?
        (lambda (n)
          (let
              ((floor (vector-ref (vector-ref board n) 0)))
            (and (<= *timed-floor* floor) (< floor 0)))))
       (already-op?
        (lambda (n)
          (let
              ((floor (vector-ref (vector-ref board n) 0)))
            (and (<= floor *non-timed-floor*) (< 0 floor)))))
       (new-board (vector-copy board))

       (target-pos
        (filter (lambda (p) (or (already-fallen? p) (already-op? p)))
           cell-pos)))
    (do-ec
     (: pos target-pos)
     (vector-set! (vector-ref new-board pos) 0 fall-time))
    new-board))

(define (game-eval sexp time player board)
  ;; (if (valid-command? sexp)
  ;;     (eval sexp (interaction-environment))
  ;;     (print "Invalid Command")))
  (let
      ((p (car sexp))
       (args (cdr sexp))
       (valid? 
        (lambda (args)
          (cond
           ((< (length args) 2) #f)
           (and (member (car args) *allowed-directions*) (member (cadr args) *allowed-fall-time*))))))
    (cond
     ((or (eq? p 'w) (eq? p 'a) (eq? p 's) (eq? p 'd))
      (apply move (list p player board)))
     ((and (eq? p 'b) (valid? args))
      (let
          ((dir (car args))
           (ft (cadr args)))
        (apply beam (list dir ft player board))))
     (else #f))))

(define (game-read)
  (let*
      ((cmd (read-from-string 
             (string-append "(" (read-line) ")"))))
    cmd))
       
(define (game-repl time player board)
  ;; (print (game-eval (game-read time player board))))
  (format #t "Player~d >>~%" player)
  (let*
      ((cmd (game-read))
       (new-board (game-eval cmd time player board)))
    (if new-board
        new-board
        (begin
          (print "Invalid command or Cannot Act")
          (game-repl time player board)))))

(define (finish? time initiative board)
  (let 
      ((survivor (vector-count (lambda (dmy x) (< 0 (vector-ref x 1))) board)))
    (or (<= *limit-time* time) (<= survivor 1))))

;; (define (w player board) (move 'w player board))
;; (define (a player board) (move 'a player board))
;; (define (s player board) (move 's player board))
;; (define (d player board) (move 'd player board))
;; (define (b direction fall-time player board) 

(define (game-loop time initiative board)
  (format #t "Round: ~d, Initiative: ~s~%" time initiative)
  (draw-board board)
    (print board)
  (let
      ((new-board (vector-copy board)))
    (do-ec (: player initiative)
           (set! new-board (game-repl time player new-board)))
    (let*
        ((resolved-board (game-resolve time initiative new-board))
         (resolved-initiative (players-resolve initiative resolved-board))
         (next-time (+ time 1)))
      (if (finish? time resolved-initiative resolved-board) (game-over time resolved-initiative resolved-board) (game-loop next-time (rotate resolved-initiative) resolved-board)))))

(define (game-resolve time initiative board)
  (let
      ((new-board (vector-copy board))
       (flooring
        (lambda (cell)
          (let
              ((floor (vector-ref cell 0)))
            (cond
             ((= floor 1) (vector-set! cell 0 *timed-floor*))
             ((= floor -1) (vector-set! cell 0 *non-timed-floor*))
             ((and (< floor *non-timed-floor*) (< 0 floor))
              (vector-set! cell 0 (dec floor)))
             ((and (<= *timed-floor* floor) (< floor 0))
              (vector-set! cell 0 (inc  floor)))
             (else
              (vector-set! cell 0 floor)))))))
    (do-ec
     (: cell new-board)
     (flooring cell))
    (do-ec
     (: cell new-board)
     (let
         ((floor (vector-ref cell 0))
          (player (vector-ref cell 1)))
       (when (and (<= *timed-floor* floor) (< floor 0) (< 0 player))
             (vector-set! cell 1 0))))
    new-board))

(define (players-resolve initiative board)
  (list-ec
   (:parallel
    (: i initiative)
    (: pos (find-players-pos initiative board)))
    (if (number? pos))
    i))

(define (game-over time initiative board)
  (print initiative)
  (print "Game Over"))

(define (run)
  (let ((board (board-init (gen-board)))
        (initiative (initiative-init))
        (time 0))
   (tk-init '())
   (draw-init)
   (game-loop time initiative board)
   (tk-mainloop)))
    
(define (draw-board board)
  (do-ec
   (: x *board-size*)
   (let*
       ((var (string-append "::v" (number->string x)))
        (now (vector-ref board x))
        (floor (vector-ref now 0))
        (player (vector-ref now 1))
        (cell (list (floor-symb floor) (player-symb player))))
     (tk-set! var cell))))

(define (draw-init)
  (do-ec 
   (: x *board-size*)
   (let
       ((button-name (string-append ".b" (number->string x)))
        (var-name (string-append "v" (number->string x)))
        (column (mod x *board-width*))
        (row (quotient x *board-width*)))
     (tk-grid
      (tk-button 
       button-name 
       :text '(0 0) :command (lambda () (++ var-name))
       :textvariable var-name) :row row :column column)))) 
(run)
