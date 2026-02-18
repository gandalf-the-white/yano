(in-package :yano/proxy)

(defun byte-not (b)
  "Inverse 8 bits : 0..255 -> 0..255"
  (logxor b #xFF))

(defun make-byte-xor (key)
  "Retourne une fonction (byte)->byte appliquant XOR key (0..255)."
  (let ((k (logand key #xFF)))
    (lambda (b)
      (logxor b k))))
