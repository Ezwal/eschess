;;;;;;;;;;
;; DATA ;;
;;;;;;;;;;

(defconstant WHITE 1)
(defconstant BLACK -1)
(defun color-to-string (c) (if (= c WHITE) "white" "black"))

(defun make-empty-chess-board ()
  (let ((s-w (make-spec-line WHITE))
        (p-w (make-pawn-line WHITE))
        (s-b (make-spec-line BLACK))
        (p-b (make-pawn-line BLACK))
        (empty (repeat 8 0)))
    (make-array '(8 8) :initial-contents (list s-w p-w empty empty empty empty p-b s-b))))

(defun repeat (nb el)
  (loop :for i :from 1 :to nb :collect el))

;; given symbol list, will return a list of instance according to color
(defun instance-pieces (plist color)
  (loop :for p :in plist :collect (make-instance p :color color)))

(defun make-pawn-line (color) (instance-pieces (repeat 8 'pawn) color))
(defun make-spec-line (color) (instance-pieces '(tower knight fool queen king fool knight tower) color))

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

;; TODO look
;; piece specific unicode : https://en.wikipedia.org/wiki/Chess_symbols_in_Unicode

(defclass piece ()
  ((color
    :initarg :color
    :initform WHITE
    :accessor color)
   (first-move
    :initform t
    :accessor first-move)
   (char-repr
    :initform #\?
    :accessor char-repr)))

(defmethod print-object ((object piece) stream)
    (with-accessors ((color color))
        object
      (format stream "~a" (color-to-string color))))
;; TODO find a char to print it like its hot

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
                 ;; in this case a piece is here, not ennemy => illegal move
                 nil)))))

(defmethod move ((object piece) board init final)
  (if (can-move object board init final)
      (progn (setf (first-move object) nil)
             (move-piece board init final))
      (error "can not move yooo")))

(defclass pawn (piece) ())

;; verify that the piece is actually moving forward corresponding to its color
(defmethod is-forward-move ((object pawn) mv)
  (let ((pred (if (plusp (color object))
                  'plusp 'minusp)))
    (funcall pred (second mv))))

(defmethod can-move ((object pawn) board init final)
  (let* ((mv (movement-vector init final))
         (mv-type (move-type mv))
         (dist (apply #'+ mv))
         (mv-charac (check-path object board init final mv)))
    ;; a pawn can ONLY move forward
    (and (is-forward-move object mv)
         ;; prise-en-passant
     (or (and (equal mv-type :diagonal)
           (= dist 1)
           (equal mv-charac :capture))
      (and (equal mv-type :straight)
           (equal mv-charac :move)
       ;; first move can jump 2 tiles
       (or (and (= dist 2)
                (first-move object))
        ;; normal move
           (= dist 1)))))))

(defclass tower (piece) ())

(defmethod can-move ((object tower) board init final)
  (let* ((mv (movement-vector init final))
         (mv-type (move-type mv)))
    (and (equal mv-type :straight)
         (check-path object board inint final mv))))

(defclass fool (piece) ())
(defmethod can-move ((object fool) board init final)
  (let* ((mv (movement-vector init final))
         (mv-type (move-type mv)))
    (and (equal mv-type :diagonal)
         (check-path object board inint final mv))))

(defclass knight (piece) ())
(defmethod can-move ((object knight) board init final)
  (let* ((mv (movement-vector init final))
         (mv-type (move-type mv)))
    (and (equal mv-type :composite)
         ("TODO check that final contains nothing OR contains ennemy piece"))))

(defclass king (piece) ())
;; TODO :before move method to check that it does NOT lead to a check mate condition
(defmethod can-move ((object king) board init final)
  (let* ((mv (movement-vector init final))
         (dist (apply #'+ mv))
         (mv-type (move-type mv)))
    (and (= dist 1)
         (or (equal mv-type :straight)
             (equal mv-type :diagonal))
         ("check that it goes NOT on check")
         (check-path object board inint final mv))))

(defclass queen (piece) ())
(defmethod can-move ((object queen) board init final)
  (let* ((mv (movement-vector init final))
         (dist (apply #'+ mv))
         (mv-type (move-type mv)))
    (and (or (equal mv-type :straight)
             (equal mv-type :diagonal))
         ("check that it goes NOT on check")
         (check-path object board inint final mv))))

(defun is-between (lower-bound upper-bound el)
  (and (>= el lower-bound)
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
  (mapcar (lambda (el) (* el multiplier)) vector))
(defun divide-vector (vector divider)
  (mapcar (lambda (el) (/ el divider)) vector))

;; given move vector, will return the nature of the move among :
;; :diagonal, :straight, :composite
(defun move-type (mv)
  (cond ((= (abs (first mv)) (abs (second mv))) :diagonal)
        ((remove-if-not (lambda (el) (= el 0)) mv) :straight)
        (t :composite)))

;; filter out illegal move and lookup if the move can actually be done by
;; the piece on the board TODO : add detection of who move at the turn
(defun move-if-allowed (board init-coords end-coords)
  (if (or (is-oob init-coords)
          (is-oob end-coords)
          (equal init-coords end-coords))
      nil
      ("TODO get the piece and invoke move from it given the right arguments")))

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
  (concatenate 'string (repeat n c)))

(defun interleave-char-string (s c)
  (coerce (interleave-el (coerce s 'list) c)
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
