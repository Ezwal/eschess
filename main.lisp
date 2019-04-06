;;;;;;;;;;
;; DATA ;;
;;;;;;;;;;

(defconstant WHITE 1)
(defconstant BLACK -1)

(defun make-empty-chess-board ()
  (make-array '(8 8)))

;; if the board is unoccupied then the value at point would be 0
(defun is-coords-occupied (board coords)
  (not (= 0 (apply #'aref board coords))))

(defclass piece ()
  ((color
   :initarg :color
   :initform 0
   :accessor color)))

(defclass pawn (piece)
  ((first-move
    :initform nil
    :accessor first-move)))
(defmethod can-move ((object pawn) board init final)
  (let* ((mv (movement-vector init final))
         (type (move-type mv))
         (dist (apply #'+ mv))
         (is-capturing-move (is-coords-occupied final)))
    (or
     ;; prise-en-passant
     (and
      (equal type :diagonal)
      (= dist 1)
      is-capturing-move)
     (and (equal type :straight)
          ;; first move can jump 2 tiles
          (or
           (and (= dist 2)
                (first-move object)
                (and
                 (not is-capturing-move)
                 (not (is-coords-occupied "TODO"))))
           ;; normal move
           (and (= dist 1)
                (not is-capturing-move)))))))

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

(defun movement-vector (init-coords end-coords)
  (mapcar #'- end-coords init-coords))

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

(defun main () (print "TODO"))
