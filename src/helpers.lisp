(defpackage eschess
  (:use :cl))
(in-package :eschess)

;; VECTORS HELPERS

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

;; CHAR AND STRING HELPERS

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

;; list helpers
(defun repeat (nb el)
  (loop :for i :from 1 :to nb :collect el))
(defun range (n) (loop :for x :from 0 :to (1- n) :collect x))
(defun xrange (n)
  (mapcan (lambda (x)
            (loop :for y :from 0 :to (1- n) :collect (list x y)))
          (range n)))


;; tui methods

;; user input
(defun char-digit (c)
  (let ((digit (- (char-code c) 48)))
    (and (between? 0 9 digit) digit)))
(defun prompt-coords! (prompt-msg)
  (format t prompt-msg)
  (let ((x (char-digit (read-char)))
        (y (char-digit (read-char))))
    (list x y)))


;; text output
(defun gen-separator ()
  (repeat-character 10 #\-))
(defun gen-middle-line (line annotation)
  (concatenate 'string annotation (interleave-char-string line #\|)))
(defun generate-top-legends ()
  (let ((ll (loop :for x :from 1 :to 8
                  :collect (write-to-string x))))
    (dolist (i ll)
      (format t "~A " i))))
