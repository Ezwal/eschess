(in-package :eschess)

(defclass piece ()
  ((color
    :initarg :color
    :initform WHITE
    :accessor color)
   (first-move
    :initform t
    :accessor first-move)
   (char-repr
    :initform (pairlis '(WHITE BLACK) '(#\? #\?))
    :accessor char-repr)))

(defmethod print-object ((object piece) stream)
  (with-accessors ((color color)
                   (char-repr char-repr))
        object
      (format stream "~a" (rest (assoc color char-repr)))))

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
      (error "can not move")))

(defclass pawn (piece)
  ((char-repr
    :initform (pairlis (list WHITE BLACK) '(#\♙ #\♟))
    :accessor char-repr)))

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

(defclass tower (piece)
  ((char-repr
    :initform (pairlis (list WHITE BLACK) '(#\♖ #\♜))
    :accessor char-repr)))

(defmethod can-move ((object tower) board init final)
  (let* ((mv (movement-vector init final))
         (mv-type (move-type mv)))
    (and (equal mv-type :straight)
         (check-path object board init final mv))))

(defclass fool (piece)
  ((char-repr
    :initform (pairlis (list WHITE BLACK) '(#\♗ #\♝))
    :accessor char-repr)))

(defmethod can-move ((object fool) board init final)
  (let* ((mv (movement-vector init final))
         (abs-mv (abs-vector mv))
         (mv-type (move-type abs-mv)))
    (and (equal mv-type :diagonal)
         (check-path object board init final mv))))

(defclass knight (piece)
  ((char-repr
    :initform (pairlis (list WHITE BLACK) '(#\♘ #\♞))
    :accessor char-repr)))

(defmethod can-move ((object knight) board init final)
  (let* ((mv (movement-vector init final))
         (abs-mv (abs-vector mv))
         (mv-type (move-type abs-mv)))
    (and (equal mv-type :composite)
         (or (is-coords-empty board final)
             (is-coords-occupied-ennemy board final)))))

(defclass king (piece)
  ((char-repr
    :initform (pairlis (list WHITE BLACK) '(#\♔ #\♚))
    :accessor char-repr)))

;; TODO :before move method to check that it does NOT lead to a check mate condition
(defmethod can-move ((object king) board init final)
  (let* ((mv (movement-vector init final))
         (abs-mv (abs-vector mv))
         (dist (apply #'+ abs-mv))
         (mv-type (move-type abs-mv)))
    (and (= dist 1)
         (or (equal mv-type :straight)
             (equal mv-type :diagonal))
         ;; TODO check that it goes NOT on check
         (check-path object board init final mv))))

(defmethod is-king-check ((object king) board)
  (print "TODO"))

(defclass queen (piece)
  ((char-repr
    :initform (pairlis (list WHITE BLACK) '(#\♕ #\♛))
    :accessor char-repr)))

(defmethod can-move ((object queen) board init final)
  (let* ((mv (movement-vector init final))
         (abs-mv (abs-vector mv))
         (mv-type (move-type abs-mv)))
    (and (or (equal mv-type :straight)
             (equal mv-type :diagonal))
         (check-path object board init final mv))))
