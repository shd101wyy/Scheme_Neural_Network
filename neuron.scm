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
          [(eq? msg 'type)
           'neuron]
          [(eq? msg 'activate) ;; activate current neuron
           (lambda []
             (letrec [(helper (lambda [neurons-that-connected-to-self result weight]
                                (if (null? neurons-that-connected-to-self) ;; finish
                                    (transfer-function (+ result bias))
                                    (let [(r (((car neurons-that-connected-to-self) 'activate)))]
                                      (helper (cdr neurons-that-connected-to-self)
                                              (+ result 
                                                 (* (car weight) r))
                                              (cdr weight))))))]
               (helper neurons-that-connected-to-self 0 weight))
             )]
          [(eq? msg 'set-transfer-function)
           (lambda [fn]
             (set! transfer-function fn))]
          [else (error "make-neuron Invalid Command")]))
  )
;; make input neurons
(define (make-input-neuron)
  (define input 0)
  (define type 'input-neuron)
  (define transfer-function neuron/purelin)
  
  (define weight 1)
  (define bias 0)
  
  (lambda [msg]
    (cond [(eq? msg 'set-input) ;; set input data
           (lambda [n] (set! input n))]
          [(eq? msg 'type) ;; get neuron type
           'input-neuron]
          [(eq? msg' activate) ;; activate input neuron
           (lambda [] (transfer-function (+ (* input weight) 
                                            bias)))]
          [(eq? msg 'set-transfer-function)
           (lambda [fn]
             (set! transfer-function fn))]
          [(eq? msg 'add-input-neuron)
           (error "Input Neurons couldn't add input neurons")]
          [else (error "make-neuron Invalid Command")])
    )
  )

;; make network
;; (make-network 3 1) means 3 inputs 1 output
;; (make-network 3 2 1) means 3 inputs, 2 neurons in hidden layer, 1 output
(define (make-network . args)
  (define layer-num (length args)) ;; get layer num
  (define (make-#num-neurons num pass-fn) ;; make #num neurons for that layer
    (letrec [(layer (make-vector num))
             (helper (lambda [i]
                       (if (= i num)
                           layer
                           (begin (vector-set! layer i (pass-fn))
                                  (helper (+ i 1)))
                           )))]
      (helper 0)))
  (define (set-up-network) ;; set up neural network
    (letrec [(output (make-vector layer-num))
             (helper (lambda [args i]
                       (if (null? args)
                           output
                           (cond [(eq? i 0) ;; input neurons layer
                                  (vector-set! output 0 (make-#num-neurons (car args)
                                                                           make-input-neuron))
                                  (helper (cdr args)
                                          (+ i 1))]
                                 [else ;; non-input neurons layer
                                  (vector-set! output i (make-#num-neurons (car args)
                                                                           make-neuron))
                                  (helper (cdr args)
                                          (+ i 1))])
                           )))]
      (helper args 0)))
  
  (define network (set-up-network)) ;; create network
  
  (define (set-input-data input-data i) ;; set input data to input neuron
    (if (null? input-data)
        'Done
        (begin (((vector-ref (vector-ref network 0) i) 'set-input)
                (car input-data))
               (set-input-data (cdr input-data) 
                               (+ i 1)))))
  
  (define (activate-layer layer-#num) ;; activate no. layer-#num layer eg ((n 'activate-layer) 1) activate layer 1
      (let* [(output-layer (vector-ref network layer-#num))
             (output-layer-length (vector-length output-layer))]
        (letrec [(helper (lambda [i]
                           (if (= i output-layer-length)
                               '()
                               (let [(r (((vector-ref output-layer i) 'activate)))]
                                 (cons r 
                                       (helper (+ i 1)))))))]
          (helper 0))
        ))
  (lambda [msg]
    (cond [(eq? msg 'connect) ;; connect two neurons. eg ((n 'connect) '(0 0) '(1 0)) means connect neuron 0 at layer 0 to neuron 0 at layer 1
           (lambda [v0 v1]
             (let* [(l0 (car v0))
                    (p0 (cadr v0))
                    (l1 (car v1))
                    (p1 (cadr v1))
                    (neuron (vector-ref (vector-ref network l1) p1))]
               ((neuron 'add-input-neuron) (vector-ref (vector-ref network l0) p0))))]
          
          [(eq? msg 'activate-neuron) ;; activate one neuron at one position ((n 'activate-neuron) '(1 0)) ;; activate neuron at layer 1 0
           (lambda [v]
             ;; (set-input-data input-data 0) ;; set input-data
             (let* [(l (car v))
                    (p (cadr v))
                    (neuron (vector-ref (vector-ref network l) p))] ;; activate that neuron and get activation result
               ((neuron 'activate))))]
          [(eq? msg 'activate) ;; activate all and get outputs from output layers
           (lambda []
             (let* [(output-layer (vector-ref network (- layer-num 1)))
                   (output-layer-length (vector-length output-layer))]
               (letrec [(helper (lambda [i]
                               (if (= i output-layer-length)
                                   '()
                                   (let [(r (((vector-ref output-layer i) 'activate)))]
                                     (cons r 
                                           (helper (+ i 1)))))))]
                 (helper 0)))
             )
           ]
          [(eq? msg 'activate-layer) ;; activate one layer
           activate-layer]
          [(eq? msg 'set-input-data) ;; set input data to input neurons 
           (lambda [input-data]
             (set-input-data input-data 0))]
          [(eq? msg 'get-neuron)     ;; get neuron at position. eg ((n 'get-neuron) '(1 0)) get neuron at layer 1 index 0
           (lambda [v]
             (let [(l (car v))
                   (p (cadr v))]
               (vector-ref (vector-ref network l) p)))]
          ))
  )


;; test network
(define x (make-network 2 2)) ;; 2 input neurons and 1 output neurons
(display x)
((x 'connect) '(0 0) '(1 0))  ;; connect (0 0) and (1 0)
((x 'connect) '(0 1) '(1 0))  ;; connect (0 1) and (1 0)
((x 'connect) '(0 0) '(1 1))  ;; connect (0 0) and (1 1)

((x 'set-input-data) '(3 4)) ;; set input data
((x 'activate-neuron) '(1 0)) ;; activate neuron at layer 1 index 0
((x 'activate-layer) 0) ;; activate layer 0
((x 'activate)) ;; activate output layer





