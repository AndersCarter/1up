#lang racket


(require test-engine/racket-tests)  ;; For Tests
(require 2htdp/universe)            ;; For World State Handling
(require 2htdp/image)               ;; For Drawing
 
(check-expect (- (+ 2 2) 1) 3) ;; Quick Maths

;; Settings
(define CELL-SIZE 10)
(define HEIGHT 500) ;; Must be Divisible by Cell-Size
(define WIDTH 500)  ;; Must be Divisible by Cell-Size
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" (color 211 211 211)))

;; World
;; A World is an object that holds all information about the current state
;; of the Snake Game
;; snake -> a snake object
;; food -> a single posn that represents the location of the food
;; scene -> the current image of the world
(struct world (snake food scene) #:transparent #:mutable)

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
            (on-tick update-world)
            (on-key key-update)
            (stop-when game-won?)))

;; draw-world: draws the current world state
(define (draw-world world)
  BACKGROUND)

;; update-world: updates the current world one time
(define (update-world world)
  world)

;; game-won?: determines if the game is over
(define (game-won? world)
  false)

;; key-update: handles key events
(define (key-update world key)
  world)




;; Random Helper Functions


;; random-loc: returns a random location in the world
(define (random-loc)
  (loc (random (/ WIDTH CELL-SIZE)) (random (/ HEIGHT CELL-SIZE))))

;; generate-starting-world: creates the initial start state of the game

(define (generate-starting-world) (world (snake "" (list (random-loc))) (random-loc) BACKGROUND))



(test)