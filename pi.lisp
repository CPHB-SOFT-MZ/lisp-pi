
;;
;; Preamble: Lisp prerequisits
;;

;; These two lines sets the number of binary digits used to represent a float
;; in Lisp. This is necessary because you'll be working with tiny numbers
;; TL;DR ignore these two lines
(setf (EXT:LONG-FLOAT-DIGITS) 35000)
(setf *read-default-float-format* 'long-float)

;; This method rounds a number to a certain precision
;; It takes two arguments: the number to round and the number of digits to
;; round in decimals
;;
;; Example: (roundToPrecision 10.0044 3) -> 10.004
(defun roundToPrecision (number precision)
  (let
    ((p (expt 10 precision)))
    (/ (round (* number p)) p)
  )
)

;;
;; Exercise
;;

;; Exercise
;; Your task is to implement the Gauss-Legendre algorithm for calculating pi
;; and extract 10.000 (ten thousand) digits

;; Gauss-Legendre algorithm on Wikipedia
;; https://en.wikipedia.org/wiki/Gauss%E2%80%93Legendre_algorithm


;(defun Piep()
;  (pip a b tt p pie)
;)

(defun nextA (pA pB)
  (/(+ pA pB) 2)
)

(defun nextB (pA pB)
  (sqrt (* pA pB))
)

(defun nextT (pT pP pA nA)
  (- pT (expt (* pP (- pA nA)) 2))
)

(defun nextP (pP)
  (* 2 pP)
)

(defun myPi (a b tt p pPie)

  (let
    (
      (aNext (nextA a b)) (bNext (nextB a b)) (pNext (nextP p))
    )
    (let
      (
        (tNext (nextT tt p a aNext))
      )
      (let
        (
          (nPie (roundToPrecision ( / (expt(+ aNext bNext) 2) (* 4 tNext) ) 10000))
        )
        (if (equal nPie pPie) nPie (myPi aNext bNext tNext pNext nPie))

      )
    )
  )
)

(write (coerce(myPi 1L0 (/ 1L0 (sqrt 2L0)) (/ 1L0 4L0) 1L0 3L0) 'long-float))
;;(write(pip a b tt p pie))
