;;;;;;;;;;
;; DATA ;;
;;;;;;;;;;

(defconstant WHITE 0)
(defconstant BLACK 1)

(defun make-empty-chess-board ()
  (make-array '(8 8)))

(defclass piece ()
  ((name
   :initarg :name
   :initform "pawn"
   :accessor name)
  (color
   :initarg :color
   :initform 0
   :accessor color)))

(defclass pawn (piece) ())
(defmethod can-move ((object pawn))
  (print "TODO"))
(defclass tower (piece) ())
(defclass fool (piece) ())
(defclass knight (piece) ())
(defclass king (piece) ())
(defclass queen (piece) ())

(defun is-between (lower-bound upper-bound el)
  (and
   (>= el lower-bound)
   (<= el upper-bound)))

(defun is-in-bound (coords)
    (every (lambda (el) (is-between 0 7 el)) coords))

(defun is-oob (coords)
  (not (is-in-bound coords)))


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
