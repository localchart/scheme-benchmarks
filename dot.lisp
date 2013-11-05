;; benchmark: dot-product on two large lists
;; language: Common Lisp
;; implementation: Clozure Common Lisp 1.9-r15757

(defvar *size* 40000000)

(defun range1 (n res)
  (cond
    ((= n 0) res)
    (t (range1 (- n 1) (cons (- n 1) res)))))

(defun range (n)
  (range1 n '()))

(defun bench ()
  (let ((ls (range *size*)))
    (time (reduce '+ (map 'list '* ls ls)))))

(bench)


;; Ran on: 64-core AMD Opteron 6276 (2300 MHz), 512G RAM
;; cpu time: 53,403 ms, gc time: 45,506 ms

;; Ran on Macbook Pro 13" (with 2-core i5 Haswell 2.6G processor, 8G RAM)
;; cpu time: 27,751 ms, gc time: 21,795 ms
;; peak memory: 1.23G
