(define *size* 40000000)

(define (range1 n res)
  (cond
    ((= n 0) res)
    (else
     (range1 (- n 1) (cons (- n 1) res)))))

(define (range n)
  (range1 n '()))

(define (bench)
  (let ((ls (range *size*)))
    (time (apply + (map * ls ls)))))

(bench)

;; Petite:
;; Ran on Macbook Pro 13" (with 2-core i5 Haswell 2.6G processor, 8G RAM)
;; cpu time: 11721, gc time: 7480
;; 3.59G peak memory

;; Racket:
;; Ran on Macbook Pro 13" (with 2-core i5 Haswell 2.6G processor, 8G RAM)
;; cpu time: 13459, gc time: 7910
;; 5G peak memory

;; Ran on: 64-core AMD Opteron 6276 (2300 MHz), 512G RAM
;; cpu time: 24822, gc time: 13840

;; Chez Scheme
;; Ran on: 64-core AMD Opteron 6276 (2300 MHz), 512G RAM
;; cpu time: 23666 ms, gc time: 16922 ms

;; Ikarus 0.0.3
;; Ran on Macbook Pro 13" (with 2-core i5 Haswell 2.6G processor, 8G RAM)
;; ikarus(76330,0xa0aff1a8) malloc: *** mach_vm_map(size=8388608) failed (error code=3)
;; *** error: can't allocate region
;; *** set a breakpoint in malloc_error_break to debug
;; Mapping failed: (null)
;; Process scheme exited abnormally with code 255
