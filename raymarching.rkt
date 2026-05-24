#lang racket

(require racket/flonum
         racket/unsafe/ops
         racket/draw)

(define EPS 0.0001)
(define T-MAX 15)
(define MAX-STEPS 128)
(define CAM-POS (list 0 0 2))
(define BACKGROUND (list 0.2694 0.4078 0.4615))

;(define SCREEN-WIDTH 320)
;(define SCREEN-HEIGHT 240)

(define SCREEN-WIDTH 1080)
(define SCREEN-HEIGHT 720)

;(define SCREEN-WIDTH 1920)
;(define SCREEN-HEIGHT 1080)

(define (ray-magnitude r)
  (sqrt (apply + (map (lambda (x) (* x x)) r))))
  
(define (ray-normalize r)
  (map (lambda (x) (/ x (ray-magnitude r))) r))

(define (rays-sum r1 r2)
  (map (lambda (x y) (+ x y)) r1 r2))

(define (rays-diff r1 r2)
  (map (lambda (x y) (- x y)) r1 r2))

(define (ray-scalar-mult r s)
  (map (lambda (x) (* x s)) r))

(define (rays-dot-product r1 r2)
  (apply + (map (lambda (x y) (* x y)) r1 r2)))

(define (ray-abs r)
  (map abs r))

(define (ray-scalar-max r s)
  (map (lambda (x) (max x s)) r))

(define (ray-x-axis-rot r t)
  (list (- (* (first r) (cos t)) (* (second r) (sin t)))
        (+ (* (first r) (sin t)) (* (second r) (cos t)))
        (third r)))

(define (ray-y-axis-rot r t)
  (list (+ (* (first r) (cos t)) (* (third r) (sin t)))
          (second r)
          (+ (* (- (first r)) (sin t)) (* (third r) (cos t)))))

(define (flmod x m)
    (- x (* (floor (/ x m)) m)))

(define (repeat-ray r rep-rate)
  (map (lambda (x) (- (flmod x rep-rate) (/ rep-rate 2))) r))

(define (sdf-sphere p r)
  (- (ray-magnitude p) r))

(define (sdf-cube p b)
  (let ([q (rays-diff (ray-abs p) b)])
    (+ (ray-magnitude (ray-scalar-max q 0)) (min (max (first q) (max (second q) (third q))) 0))))

(define (render p)
  (min (sdf-sphere (rays-diff p (list 0 0 -0.8)) 0.6)
       (sdf-cube (ray-x-axis-rot (ray-y-axis-rot (repeat-ray p 1.75) 4) 3) (list 0.3 0.3 0.3))))

(define (calc-normal p)
  (let ([h (list EPS 0)])
    (ray-normalize (list (- (render (rays-sum p (list EPS 0 0))) (render (rays-diff p (list EPS 0 0))))
                         (- (render (rays-sum p (list 0 EPS 0))) (render (rays-diff p (list 0 EPS 0))))
                         (- (render (rays-sum p (list 0 0 EPS))) (render (rays-diff p (list 0 0 EPS))))))))
    
(define (color-to-argb-bytes color)
  (let ([v (flvector (exact->inexact (first color)) (exact->inexact (second color)) (exact->inexact (third color)))])
  (bytes 255
         (exact-round (unsafe-fl* 255.0 (unsafe-flvector-ref v 0)))
         (exact-round (unsafe-fl* 255.0 (unsafe-flvector-ref v 1)))
         (exact-round (unsafe-fl* 255.0 (unsafe-flvector-ref v 2))))))

(define (color-ray-trim r)
  (map (lambda (x) (max 0 (min 1 x))) r))

(define (ray-marcher uv)
  (define (ray-marcher-iter uv t steps)
    (let* ([ray-pos (list 0 0 2)]
           [ray (ray-normalize (append uv (list -1)))]
           [act-pos (rays-sum ray-pos (ray-scalar-mult ray t))]
           [h (render act-pos)])
      (if [or (< h EPS) (> h T-MAX) (< steps 0)]
          t
          (ray-marcher-iter uv (+ t h) (- steps 1)))))
      
  (let ([t (ray-marcher-iter uv 0 MAX-STEPS)])
    (if [< t T-MAX]
        (let* ([ray-pos (list 0 0 2)]
              [ray (ray-normalize (append uv (list -1)))]
              [act-pos (rays-sum ray-pos (ray-scalar-mult ray t))]
              [diff (rays-dot-product (list 0.1 0 1) (calc-normal act-pos))])
        (color-ray-trim (list diff diff diff)))
        BACKGROUND)))
  
(define (draw-pixel pic x y)
  (let* ([uv (list (/ (- (* 2 x) SCREEN-WIDTH) SCREEN-HEIGHT) (/ (- (* 2 y) SCREEN-HEIGHT) SCREEN-HEIGHT))]
         [color (ray-marcher uv)])
  (send pic set-argb-pixels
        x y 1 1
        (color-to-argb-bytes color)))
  (if [< x SCREEN-WIDTH] (draw-pixel pic (+ x 1) y) 1))

(define (draw-row pic y)
  (if [= (modulo y 50) 0] (displayln (list y '/ SCREEN-HEIGHT)) 0)
  (draw-pixel pic 0 y)
  (if [< y SCREEN-HEIGHT] (draw-row pic (+ y 1)) 1))

(define (main)
  (define pic (make-bitmap SCREEN-WIDTH SCREEN-HEIGHT))
  (draw-row pic 0)
  (send pic save-file "result.png" 'png)
  pic)      

(define timer (current-seconds))
(main)
(- (current-seconds) timer)