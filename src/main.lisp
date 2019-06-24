(defpackage eschess
  (:use :cl))
(in-package :eschess)

(defconstant WHITE 1)
(defconstant BLACK -1)
(defconstant EMPTY "")

(defun make-empty-chess-board ()
  (let ((s-w (make-spec-line WHITE))
        (p-w (make-pawn-line WHITE))
        (s-b (make-spec-line BLACK))
        (p-b (make-pawn-line BLACK))
        (empty-line (repeat 8 EMPTY)))
    (make-array '(8 8) :initial-contents (list s-b p-b empty-line empty-line empty-line empty-line p-w s-w))))

(defun make-pawn-line (color) (instance-pieces (repeat 8 'pawn) color))
(defun make-spec-line (color) (instance-pieces '(tower knight fool queen king fool knight tower) color))
(defun make-pawn-line (color) (instance-pieces (repeat 8 'pawn) color))

;; given symbol list, will return a list of instance according to color
(defun instance-pieces (plist color)
  (loop :for p :in plist :collect (make-instance p :color color)))

;; basic board manipulation
(defun get-board-coords (board coords)
  (apply #'aref board coords))
(defun set-board-coords! (board coords val)
  (setf (apply #'aref board coords) val))

(defun remove-piece! (board coords)
  (set-board-coords! board coords EMPTY))
(defun move-piece! (board init final)
  (let ((p (get-board-coords board init)))
    (remove-piece! board init)
    (set-board-coords! board final p)
    board))

;; this func check that the actual piece is there AND that it can performs the move!
;; TODO check it is the right color that is invoked for the move
(defun move! (board init final turn-color)
  (let ((p-init (get-board-coords board init))
        (p-final (get-board-coords board final)))
    (if (and (not (equal EMPTY p-init))
             (can-move p-init board init final)
             (in-bound? init-coords)
             (in-bound? end-coords)
             (not (equal init-coords end-coords))
             (= (color p-init) turn-color))
        (if (king-capturable? board (color p-init)) ;; checking if the move is actually acceptable before doing it
          (setf (first-move p-init) nil)
          (move-piece! board init final))
        nil)))

(defun capturable? (board target-coords color);; TODO is it necessary to have color here ?
  (let ((opposing-color (* -1 color))
        (all-coords (xrange 8)))
    ;; if any coords have a piece that CAN capture the target-coords then its capturable
    (some (lambda (coord)
            (let ((p (get-board-coords board coord)))
              (if (and (not (equal EMPTY p))
                       (equal (color p) opposing-color))
                  (can-move p board coord target-coords))))
          all-coords)))

(defun king-capturable? (board color)
  (capturable? board (first (remove-if-not
                               (lambda (coord)
                                 (let ((p (get-board-coords board coord)))
                                   (and (not (equal EMPTY p))
                                        (equal (type-of p) 'king)
                                        (equal (color p) color))))
                               (xrange 8)))
               color))

;; just asks until the user input are actually valid
(defun turn! (board turn-color)
  (loop
    (let ((copy-board (alexandria:copy-array board)) ;; TODO change this ugly ass copy
          (move-init (prompt-coords! "Enter coords of piece to move: "))
          (move-end (prompt-coords! "Enter destination of piece: "))) ;; scan the users for move order
      (if (move! copy-board move-init move-end turn-color)
          (return copy-board)))))

;; basic piece verification
(defun coords-empty? (board coords)
  (equal (get-board-coords board coords) EMPTY))
(defun coords-occupied? (board coords)
  (not (coords-empty? board coords)))
(defun coords-occupied-ennemy? (board coords color)
  (not (= (color (get-board-coords board coords)) color)))

(defun between? (lower-bound upper-bound el)
  (and (>= el lower-bound)
       (<= el upper-bound)))
(defun in-bound? (coords)
  (every (lambda (el) (between? 0 7 el)) coords))

;; given move! vector (absed'), will return the nature of the move! among :
;; :diagonal, :straight, :composite
(defun move-type (abs-mv)
  (cond ((= (first abs-mv) (second abs-mv)) :diagonal)
        ((remove-if-not (lambda (el) (= el 0)) abs-mv) :straight)
        (t :composite)))

;;;;;;;;;;;;;;;
;; INTERFACE ;;
;;;;;;;;;;;;;;;

(defun main ()
  (print "TODO"))
