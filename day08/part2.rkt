#lang racket
(require racket/cmdline)
(require racket/dict)

(define (solve file-name)
    (define file 
        (open-input-file file-name
            #:mode 'text
        )
    )

    (let ([steps (string->list (read-line file))])
        (read-line file)
        (let ([map (read-map file)])
            (close-input-port file)
            
            (printf "Steps: ~A\n" steps)
            (printf "Map: ~A\n" map)
            (let ([start (find-start map)])
                (printf "Start: ~A\n" start)
                (printf "Steps: ~A\n" (walk steps map start))
            )
        )
    )
)

(define (find-start map [acc '()])
    (if (= (length map) 0)
        acc
        (let ([next (car map)]
            [remaining (cdr map)])
        
            (let ([key (car next)])
                (if 
                    (char=? 
                        (last (string->list key))
                        #\A
                    )

                    (find-start remaining (append acc (list key)))
                    (find-start remaining acc)
                )
            )
        )
    )
)

(define (move-forward step map from [acc '()])
    (if (= (length from) 0)
        acc
        (let ([next-from (take-step step map (car from))])
            (move-forward step map (cdr from) (append acc (list next-from)))
        )
    )
)

(define (take-step step map from)
    (let ([entry (dict-ref map from)])
        (cond
            [(char=? step #\L) (car entry)]
            [(char=? step #\R) (car (cdr entry))]
            [else (error "Unrecognised step")]
        )
    )
)

(define (walk steps map from [count 0])
    (printf "From: ~A\n" from)
    (if (can-stop? from)
        count
        (let ([next-step (car steps)]
                [remaining-steps (cdr steps)]
            )

            (walk 
                (append remaining-steps (list next-step))
                map
                (move-forward next-step map from)
                (+ count 1)
            )
        )
    )
)

(define (can-stop? positions)
    (if (<= (length positions) 0)
        #t
        (let ([next (car positions)]
            [remaining (cdr positions)])
        
            (if 
                (char=?
                    (last (string->list next))
                    #\Z
                )
                (can-stop? remaining)
                #f
            )
        )
    )
)

(define (read-map file [map-acc '()])
    (let ([line (read-line file)])
        (if (eof-object? line)
            map-acc
            ; (read-map file (append (list line) map-acc))
            (let ([key (substring line 0 3)]
                [left (substring line 7 10)]
                [right (substring line 12 15)])
            
                (read-map 
                    file 
                    (dict-set map-acc key (list left right))
                )
            )
        )
    )
)

(println (>= (vector-length (current-command-line-arguments)) 1))

(if (>= (vector-length (current-command-line-arguments)) 1) 
    (solve (vector-ref (current-command-line-arguments) 0))
    (println "Missing file name")
)

