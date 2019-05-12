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

(defun repeat (nb el)
  (loop :for i :from 1 :to nb :collect el))
(defun range (n) (loop :for x :from 0 :to (1- n) :collect x))
(defun xrange (n)
  (mapcan (lambda (x)
            (loop :for y :from 0 :to (1- n) :collect (list x y)))
          (range n)))


;; given symbol list, will return a list of instance according to color
(defun instance-pieces (plist color)
  (loop :for p :in plist :collect (make-instance p :color color)))

(defun make-pawn-line (color) (instance-pieces (repeat 8 'pawn) color))
;; basic board manipulation
(defun get-board-coords (board coords)
  (apply #'aref board coords))
(defun set-board-coords! (board coords val)
  (setf (apply #'aref board coords) val))

;; TODO trash the piece that get eaten if captured
(defun remove-piece! (board coords)
  (set-board-coords! board coords EMPTY))
(defun move-piece! (board init final)
  (let ((p (get-board-coords board init)))
    (remove-piece! board init)
    (set-board-coords! board final p)
    board))
;; this func check that the actual piece is there AND that it can performs the move!
(defun move! (board init final)
  (let ((p (get-board-coords board init))) ;; check the sig of can-move it may be retarded
    (if (and (not (equal EMPTY p))
             (can-move p board init final))
        (progn
          (setf (first-move p) nil)
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

;; TODO factorize with above ??
(defun king-capturable? (board color)
  (capturable? board (first (remove-if-not
                               (lambda (coord)
                                 (let ((p (get-board-coords b coord)))
                                   (and (not (equal EMPTY p))
                                        (equal (type-of p) 'king)
                                        (equal (color p) color))))
                               (xrange 8))) color))

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
(defun is-in-bound (coords)
  (every (lambda (el) (between? 0 7 el)) coords))
(defun is-oob (coords)
  (not (is-in-bound coords)))

;; Vector arithmetic
(defun movement-vector (init end)
  (mapcar #'- end init))
(defun add-vector (coords vector)
  (mapcar #'+ coords vector))
(defun multiply-vector (vector multiplier)
  (mapcar (lambda (el) (* el multiplier)) vector))
(defun divide-vector (vector divider)
  (mapcar (lambda (el) (/ el divider)) vector))
(defun abs-vector (el)
  (mapcar #'abs el))

;; given move! vector (absed'), will return the nature of the move! among :
;; :diagonal, :straight, :composite
(defun move-type (abs-mv)
  (cond ((= (first abs-mv) (second abs-mv)) :diagonal)
        ((remove-if-not (lambda (el) (= el 0)) abs-mv) :straight)
        (t :composite)))

;; filter out illegal move! and lookup if the move! can actually be done by
;; the piece on the board TODO : add detection of who move! at the turn
(defun move-if-allowed (board init-coords end-coords)
  (if (or (is-oob init-coords)
          (is-oob end-coords)
          (equal init-coords end-coords))
      nil
      (print "TODO get the piece and invoke move! from it given the right arguments")))

;;;;;;;;;;;;;;;
;; INTERFACE ;;
;;;;;;;;;;;;;;;

(defun to-upper-case (c)
  (character (format nil "~:(~a~)" c)))

(defun letter-to-number (c)
  (let ((A-value (char-code #\A)))
    (- (char-code (to-upper-case c)) A-value)))

(defun letter-coordinates (c)
  (let ((num (letter-to-number c)))
    (and (between? 0 7 num) num)))

; convert string like "A6" => '(0 6)
(defun string-to-coordinates (s)
  (let* ((slist (coerce s 'list))
         (x (first slist))
         (y (second slist)))
    (list (letter-coordinates x) (digit-char-p y))))

(defun main ()
  (print "TODO"))
