#lang racket
;; artificial neural network builder
(define (make-matrix row col)
  (letrec [(output (make-vector row))
           (helper (lambda [count]
                     (if (= count row)
                         output
                         (begin (vector-set! output count (make-vector col))
                                (helper (+ count 1))))))]
    (helper 0)))

;; hardlim function
(define (neuron/hardlim v)
  (if (> v 0)
      1
      0))
(define (neuron/purelin v) v)

;; make neuron
(define (make-neuron)
  (define weight '())
  (define bias 0)
  (define neurons-that-connected-to-self '()) ;; save neurons that connect to self
  (define transfer-function neuron/purelin) ;; transfer function
  (lambda [msg]
    (cond [(eq? msg 'add-input-neuron)
           (lambda [input-neuron]
             (set! neurons-that-connected-to-self (cons input-neuron
                                                        neurons-that-connected-to-self))
             (set! weight (cons 1 
                                weight)))]
          [(eq? msg 'activate) ;; activate current neuron
           (lambda [input-data]
             )]
          [else (error "make-neuron Invalid Command")]))
  )
;; make network
;; (make-network 3 1) means 3 inputs 1 output
;; (make-network 3 2 1) means 3 inputs, 2 neurons in hidden layer, 1 output
(define (make-network . args)
  (define layer-num (length args)) ;; get layer num
  (define (make-#num-neurons num) ;; make #num neurons for that layer
    (letrec [(layer (make-vector num))
             (helper (lambda [i]
                       (if (= i num)
                           layer
                           (begin (vector-set! layer i (make-neuron))
                                  (helper (+ i 1))))))]))
  (define (set-up-network) ;; set up neural network
    (letrec [(output (make-vector layer-num))
             (helper (lambda [args i]
                       (if (null? args)
                           output
                           (begin (vector-set! output i (make-#num-neurons (car args)))
                                  (helper (cdr args)
                                          (+ i 1))))))]
      (helper args 0)))
  
  (define network (set-up-network)) ;; create network
  
  (lambda [msg]
    (cond [(eq? msg 'connect) ;; connect two neurons. eg ((n 'connect) 0 0 1 0) means connect neuron 0 at layer 0 to neuron 0 at layer 1
           (lambda [l0 p0 l1 p1]
             (let [(neuron (vector-ref (vector-ref network l1) p1))]
               ((neuron 'add-input-neuron) (vector-ref (vector-ref network l0) p0))))]
          
          [(eq? msg 'activate-neuron) ;; activate one neuron at one position
           (lambda [l p input-data]
             (let [(neuron (vector-ref (vector-ref network l) p))]
               ((neuron 'activate) input-data)))]
          ))
  )










