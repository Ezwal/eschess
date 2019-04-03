; useless shit, to remove
(defun factoriel (n)
  (if (<= n 2)
      n
      (* n (factoriel (1- n)))))

;;;;;;;;;;
;; DATA ;;
;;;;;;;;;;

(defun make-empty-chess-board ()
  (make-array '(8 8)))

(defstruct )
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
