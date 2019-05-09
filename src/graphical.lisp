(defpackage eschess
  (:use :cl))
(in-package :eschess)

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
