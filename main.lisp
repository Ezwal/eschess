;;;;;;;;;;
;; DATA ;;
;;;;;;;;;;

(defconstant WHITE 1)
(defconstant BLACK -1)

(defun make-empty-chess-board ()
  (make-array '(8 8)))

;; basic board manipulation
(defun get-board-coords (board coords)
  (apply #'aref board coords))
(defun set-board-coords (board coords val)
  (setf (apply #'aref board coords) val))
(defun move-piece (board init final)
  (let ((p (get-board-coords board init)))
    (set-board-coords board init 0)
    (set-board-coords board final p)))

;; basic piece verification
(defun is-coords-empty (board coords)
  (= 0 (get-board-coords board coords)))
(defun is-coords-occupied (board coords)
  (not (is-coords-empty board coords)))
(defun is-coords-occupied-ennemy (board coords color)
  (not (= (color (get-board-coords board coords)) color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIECE SPECIFIC LOGIC ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass piece ()
  ((color
    :initarg :color
    :initform WHITE
    :accessor color)))

;; given the path, will check that actually no piece (ally OR ennemy)
;; lie on it until the actual target of the movement return nil if impossible
;; only to be used for diagonal or straight movement otherwise kittten dies
(defmethod check-path ((object piece) board init final mv)
  (let* ((time (apply #'max (mapcar #'abs mv))) ;; needs to abs as mv can negative
         (step (divide-vector mv time)))
    (and (every #'identity (loop :for i :from 1 :to (1- time)
                                 :collect (is-coords-empty board
                                                           (add-vector init (multiply-vector step i)))))
         (if (is-coords-empty board final)
             :move
             (if (is-coords-occupied-ennemy board final (color object))
                 :capture
                 nil)))))

;; PAWN
(defclass pawn (piece)
  ((first-move
    :initform nil
    :accessor first-move)))

;; verify that the piece is actually moving forward corresponding
;; to its color TODO : check that the direction for color is right AND second just
(defmethod is-forward-move ((object pawn) mv)
  (let ((pred (if (plusp (color object))
                  'plusp 'minusp)))
    (funcall pred (second mv))))

(defmethod can-move ((object pawn) board init final)
  (let ((mv (movement-vector init final)
         (mv-type (move-type mv))
         (dist (apply #'+ mv))
         (mv-char (check-path object board init final mv))))
    (and
     ;; a pawn can ONLY move forward
     (is-forward-move object mv)
     (or
      ;; prise-en-passant
      (and
       (equal mv-type :diagonal)
       (= dist 1)
       (equal mv-char :capture))
      (and (equal mv-type :straight)
           (equal mv-char :move)
           ;; first move can jump 2 tiles
           (or
            (and (= dist 2)
                 (first-move object))
            ;; normal move
            (= dist 1)))))))

(defclass tower (piece) ())
(defclass fool (piece) ())
(defclass knight (piece) ())
(defclass king (piece)
  ((can-rock
    :initform t
    :accessor can-rock)))

(defclass queen (piece) ())

(defun is-between (lower-bound upper-bound el)
  (and
   (>= el lower-bound)
   (<= el upper-bound)))
(defun is-in-bound (coords)
  (every (lambda (el) (is-between 0 7 el)) coords))
(defun is-oob (coords)
  (not (is-in-bound coords)))

(defun movement-vector (init end)
  (mapcar #'- end init))
(defun add-vector (coords vector)
  (mapcar #'+ coords vector))
(defun multiply-vector (vector multiplier)
  (mapcar (lambda (el) ( el multiplier)) vector))
(defun divide-vector (vector divider)
  (mapcar (lambda (el) (/ el divider)) vector))

;; given move vector, will return the nature of the move among :
;; :diagonal, :straight, :composite
(defun move-type (mv)
  (cond ((= (abs (first mv)) (abs (second mv))) :diagonal)
        ((remove-if-not (lambda (el) (= el 0)) mv) :straight)
        (t :composite)))

;; filter out illegal move and lookup if the move can actually be done by
;; the piece on the board
(defun is-move-allowed (board init-coords end-coords)
  (if (or (is-oob init-coords)
          (is-oob end-coords)
          (equal init-coords end-coords))
      nil
      (let ((mov (movement-vector init-coords end-coords)))
        (print "TODO"))))

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
    (and (is-between 0 7 num) num)))

; convert string like "A6" => '(0 6)
(defun string-to-coordinates (s)
  (let* ((slist (coerce s 'list))
         (x (first slist))
         (y (second slist)))
    (list (letter-coordinates x) (digit-char-p y))))

;;;;;;;;;;;;;
;; GRAPHIC ;;
;;;;;;;;;;;;;

;; Helpers

(defun repeat-character (n c)
  (concatenate 'string (make-array n :initial-element c)))

(defun interleave-char-string (s c)
  (coerce
   (interleave-el (coerce s 'list) c)
   'string))

(defun interleave-el (line el)
  (if (not line)
      (list el)
      (concatenate 'list
                   (list el (first line))
                   (interleave-el (rest line) el))))

(defun append-new-line (s)
  (format t "~A~%" s #\return))

;; Impl

; taking an array from y level from chessboard
(defun gen-separator ()
  (repeat-character 10 #\-))

(defun gen-middle-line (line annotation)
  (concatenate 'string annotation (interleave-char-string line #\|)))

(defun generate-top-legends ()
  (let ((ll (loop :for x :from 1 :to 8
                  :collect (write-to-string x))))
    (dolist (i ll)
      (format t "~A " i))))

; this will take 2d array (i.e a chess board) and aggregate it as
; one string printable by the executable easily
(defun print-chess-board (board)
  (print "TODO"))

(defun main ()
  (print "TODO"))
