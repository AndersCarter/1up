#lang racket


(require test-engine/racket-tests)  ;; For Tests
(require 2htdp/universe)            ;; For World State Handling
(require 2htdp/image)               ;; For Drawing
 
(check-expect (- (+ 2 2) 1) 3) ;; Quick Maths

;; Settings
(define GAME-SPEED .04) ;; How many times per second the game updates
(define CELL-SIZE 10)
(define HEIGHT 500) ;; Must be Divisible by Cell-Size
(define WIDTH 500)  ;; Must be Divisible by Cell-Size
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black"))
(define SEGMENT (square CELL-SIZE "solid" "white"))
(define FOOD (square CELL-SIZE "solid" "red"))

;; World
;; A World is an object that holds all information about the current state
;; of the Snake Game
;; snake -> a snake object
;; food -> a single posn that represents the location of the food
;; scene -> the current image of the world
(struct world (snake food scene) #:mutable)

;; Location
;; Represents the current location of an object in the world
;; x -> a natural number where 0 <= x <= WIDTH / CELL-SIZE
;; y -> a natural number where 0 <= y <= HEIGHT / CEll-SIZE
(struct loc (x y) #:transparent #:mutable)

;; Snake
;; Represents the snake
;; dir -> the direction snake is moving. Either up, down, right, left, or "".
;; The empty string represents the snake not moving.
;; segments -> a list of locations representing the indvidual segments of the snake
(struct snake (dir segments) #:transparent #:mutable)



;; run-game: Controls world and loop

(define (run-game [world (generate-starting-world)])
  (big-bang world
            (to-draw draw-world)
            (on-tick update-world GAME-SPEED)
            (on-key key-update)
            (stop-when game-lost?)))

;; draw-world: draws the current world state
(define (draw-world world)
  (draw-food (world-food world)(draw-snake (world-snake world) BACKGROUND)))

;; update-world: updates the current world one time
(define (update-world a-world)
  (define snk (world-snake a-world))
  (define food (world-food a-world))
  (cond [(snake-ate? (world-snake a-world) (world-food a-world))
         (set! snk (grow-and-move-snake (world-snake a-world)))
         (set! food (random-loc))]
        [else (set! snk (move-snake (world-snake a-world)))])
  (world snk food (world-scene a-world)))

;; game-lost?: determines if the game is over
;; w -> current world state
(define (game-lost? w)
  (or (collided-with-self? (world-snake w))
      (collided-with-walls? (world-snake w))))

;; key-update: handles key events
;; w -> the current world state
;; key -> the current keyEvent
(define (key-update w key)
  (define snk-dir (snake-dir (world-snake w)))
  (define new-world (void))
  (cond [(key=? key "w") (if (not (string=? snk-dir "down"))
                             (set! snk-dir "up")
                             (void))]
        [(key=? key "a") (if (not (string=? snk-dir "right"))
                             (set! snk-dir "left")
                             (void))]
        [(key=? key "s") (if (not (string=? snk-dir "up"))
                             (set! snk-dir "down")
                             (void))]
        [(key=? key "d") (if (not (string=? snk-dir "left"))
                             (set! snk-dir "right")
                             (void))]
        [(key=? key "r") (set! new-world (generate-starting-world))])
  (if (world? new-world)
      new-world
      (world (snake snk-dir (snake-segments (world-snake w)))
                          (world-food w)
                          (world-scene w))) 
  )



;                                                                                                                  
;                                                                                                                  
;                                                                                                                  
;                                                                                                                  
;   ;;;;                                    ;;;;;;                                     ;                           
;    ;  ;                                    ;   ;                           ;                                     
;    ;   ;  ;; ;;;   ;;;;   ;;; ;;;          ; ;    ;;  ;;  ;; ;;    ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;    ;;;;; 
;    ;   ;   ;;     ;    ;   ;   ;           ;;;     ;   ;   ;;  ;  ;   ;;   ;         ;    ;    ;   ;;  ;  ;    ; 
;    ;   ;   ;       ;;;;;   ; ; ;           ; ;     ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;   ;;;;  
;    ;   ;   ;      ;    ;   ; ; ;           ;       ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;       ; 
;    ;  ;    ;      ;   ;;   ; ; ;           ;       ;  ;;   ;   ;  ;    ;   ;   ;     ;    ;    ;   ;   ;  ;    ; 
;   ;;;;    ;;;;;    ;;; ;;   ; ;           ;;;       ;; ;; ;;; ;;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;; ;;;;;  
;                                                                                                                  
;                                                                                                                  
;                                                                                                                  
;                                                                                                                  
                                  

;; draw-snake: draws the snake onto the given scene
;; snake -> current snake object to draw
;; scene -> current image of the world

(define (draw-snake snake scene)
  (define snake-loc (snake-segments snake))
  (foldr (λ (loc img) (place-image/align SEGMENT
                                         (* (loc-x loc) CELL-SIZE)
                                         (* (loc-y loc) CELL-SIZE)
                                         "left"
                                         "top"
                                         img))
         scene snake-loc))

;; draw-food: draws the food onto the given scene
;; loc -> current food location
;; scene -> current image of the world

(define (draw-food loc scene)
  (place-image/align FOOD (* (loc-x loc) CELL-SIZE) (* (loc-y loc) CELL-SIZE) "left" "top" scene))


;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  
;   ;;; ;;;             ;;                                  ;;;;;;                                     ;                           
;    ;   ;               ;           ;                       ;   ;                           ;                                     
;    ;   ;  ;; ;;    ;;; ;   ;;;;   ;;;;;    ;;;;            ; ;    ;;  ;;  ;; ;;    ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;    ;;;;; 
;    ;   ;   ;;  ;  ;   ;;  ;    ;   ;      ;    ;           ;;;     ;   ;   ;;  ;  ;   ;;   ;         ;    ;    ;   ;;  ;  ;    ; 
;    ;   ;   ;   ;  ;    ;   ;;;;;   ;      ;;;;;;           ; ;     ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;   ;;;;  
;    ;   ;   ;   ;  ;    ;  ;    ;   ;      ;                ;       ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;       ; 
;    ;   ;   ;   ;  ;   ;;  ;   ;;   ;   ;  ;                ;       ;  ;;   ;   ;  ;    ;   ;   ;     ;    ;    ;   ;   ;  ;    ; 
;     ;;;    ;;;;    ;;; ;;  ;;; ;;   ;;;    ;;;;;          ;;;       ;; ;; ;;; ;;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;; ;;;;;  
;            ;                                                                                                                     
;           ;;;                                                                                                                    
;                                                                                                                                  
;


;; move-snake: moves the snake in its current direction.

(check-expect (move-snake (snake "up" (list (loc 2 2)))) (snake "up" (list (loc 2 1))))
(check-expect (move-snake (snake "right" (list (loc 2 2)))) (snake "right" (list (loc 3 2))))
(check-expect (move-snake (snake "down" (list (loc 2 2)))) (snake "down" (list (loc 2 3))))
(check-expect (move-snake (snake "left" (list (loc 2 2) (loc 3 2)))) (snake "left" (list (loc 1 2) (loc 2 2))))

(define (move-snake snk)
  (define segments (snake-segments snk))
  (define head (move-segment (first segments) (snake-dir snk)))
  (snake (snake-dir snk) (cons head (reverse (drop (reverse segments) 1)))))

;; move-segment: moves a single segment in given direction

(check-expect (move-segment (loc 2 3) "up") (loc 2 2))
(check-expect (move-segment (loc 2 3) "down") (loc 2 4))
(check-expect (move-segment (loc 2 3) "left") (loc 1 3))
(check-expect (move-segment (loc 2 3) "right") (loc 3 3))

(define (move-segment segment dir)
  (define x (loc-x segment))
  (define y (loc-y segment))
  (cond [(string=? dir "up") (set! y (- y 1))]
        [(string=? dir "down") (set! y (+ y 1))]
        [(string=? dir "left") (set! x (- x 1))]
        [(string=? dir "right") (set! x (+ x 1))])
  (loc x y))

;; grow-snake: adds a segment to the snake
;; snk -> the current snake

(check-expect (grow-and-move-snake (snake "up" (list (loc 2 2)))) (snake "up" (list (loc 2 1) (loc 2 2))))

(define (grow-and-move-snake snk)
  (define head (first (snake-segments snk)))
  (set! head (move-segment head (snake-dir snk)))
  (snake (snake-dir snk) (cons head (snake-segments snk))))

;; snake-ate: determines if the snake has eaten the food

(check-expect (snake-ate? (snake "" (list (loc 2 2))) (loc 2 2)) #t)
(check-expect (snake-ate? (snake "" (list (loc 3 4))) (loc 3 5)) #f)
(check-expect (snake-ate? (snake "" (list (loc 3 4))) (loc 2 4)) #f)

(define (snake-ate? snake food)
  (define head (first (snake-segments snake)))
  (and (= (loc-x head) (loc-x food))
       (= (loc-y head) (loc-y food))))


;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          
;    ;;; ;                                  ;;;;;;                             ;                           
;   ;   ;;   ;                               ;   ;                   ;                                     
;   ;       ;;;;;    ;;;;   ;; ;;            ; ;    ;;  ;;   ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;    ;;;;; 
;    ;;;;    ;      ;    ;   ;;  ;           ;;;     ;   ;  ;   ;;   ;         ;    ;    ;   ;;  ;  ;    ; 
;        ;   ;      ;    ;   ;   ;           ; ;     ;   ;  ;        ;         ;    ;    ;   ;   ;   ;;;;  
;        ;   ;      ;    ;   ;   ;           ;       ;   ;  ;        ;         ;    ;    ;   ;   ;       ; 
;   ;;   ;   ;   ;  ;    ;   ;   ;           ;       ;  ;;  ;    ;   ;   ;     ;    ;    ;   ;   ;  ;    ; 
;   ; ;;;     ;;;    ;;;;    ;;;;           ;;;       ;; ;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;; ;;;;;  
;                            ;                                                                             
;                           ;;;                                                                            
;                                                                                                          
;                                                                                                          

;; collided-with-self?: determines if the snake has collided with itself
;; snk -> the current snake structure

(define (collided-with-self? snk)
  (define head (first (snake-segments snk)))
  (define segments (rest (snake-segments snk)))
  (in-same-location? head segments))

;; in-same-location?: determines if the given location is in the list of locations
;; loc -> given location
;; loc-list -> the list of locations

(check-expect (in-same-location? (loc 2 2) (list (loc 3 3) (loc 4 3) (loc 2 2) (loc 4 5))) #t)
(check-expect (in-same-location? (loc 2 2) (list (loc 3 3) (loc 3 2))) #f)

(define (in-same-location? loc loc-list)
  (list? (member loc loc-list)))

;; collided-with-walls?: determines if the snake has collided with the walls
;; snk -> the current snake structure

(define (collided-with-walls? snk)
  (define head (first (snake-segments snk)))
  (or (or (>= (loc-x head) (/ WIDTH CELL-SIZE))
          (< (loc-x head) 0))
      (or (>= (loc-y head) (/ HEIGHT CELL-SIZE))
          (< (loc-y head) 0))))





;; Random Helper Functions


;; random-loc: returns a random location in the world
(define (random-loc)
  (loc (random (/ WIDTH CELL-SIZE)) (random (/ HEIGHT CELL-SIZE))))

;; generate-starting-world: creates the initial start state of the game

(define (generate-starting-world) (world (snake "" (list (random-loc))) (random-loc) BACKGROUND))





;(test)
(run-game)