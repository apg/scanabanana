#lang racket/base

(require racket/draw
         racket/gui)

(define files (for/list [(i (in-range 7))]
                (format "./img/banana~a.png" i)))

(define raw-frames (map (lambda (x) (read-bitmap x)) files))
(define frames (map (lambda (x) (new bitmap-dc% [bitmap x])) raw-frames))
(define-values [w h] (send (car frames) get-size))
(define width (inexact->exact w))
(define height (inexact->exact h))
(define output (make-bitmap width height))
(define out-dc (new bitmap-dc% [bitmap output]))

(define mask-output (make-bitmap width height))
(define mask-dc (new bitmap-dc% [bitmap mask-output]))

(define vframes (apply vector frames))
(define buffer (make-bytes (* height 4)))

(for ([i (in-range width)])
   (let* ([tmp-dc (vector-ref vframes (remainder i (vector-length vframes)))])
      (send tmp-dc get-argb-pixels i 0 1 height buffer)
      (send output set-argb-pixels i 0 1 height buffer)))

(for ([i (in-range width)])
  (when (= 0 (remainder i (vector-length vframes)))
    (send mask-dc set-brush "black" 'solid)
    (send mask-dc set-pen "black" 0 'solid)
    (send mask-dc draw-rectangle i 0 (sub1 (vector-length vframes)) height)))

(define frame (new frame%
                   [label "Scanabanana"]
                   [width (inexact->exact width)]
                   [height (inexact->exact height)]))
(define mouse-x 0)

(define scana-canvas%
  (class canvas%
    (define/override (on-event event)
      (set! mouse-x (send event get-x)))
    (super-new)))

(define scana
  (new scana-canvas% [parent frame]
       [paint-callback
        (lambda (canvas d)
          (send d draw-bitmap output 0 0)
          (send d draw-bitmap mask-output (- mouse-x (/ width 2)) 0))]))

(send frame show #t)

(define timer
  (new timer%
       (interval 10)  ;; update every 10 ms
       (notify-callback
        (lambda ()
          (send scana refresh)))))
