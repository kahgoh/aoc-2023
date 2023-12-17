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
            (let ([count (walk steps map)])
                (printf "Steps: ~A\n" count)
            )
        )
    )
)

(define (take-step step map from)
    (let ([entry (dict-ref map from)])
        (printf "Step: ~A\n" step)
        (cond
            [(char=? step #\L) (car entry)]
            [(char=? step #\R) (car (cdr entry))]
            [else (error "Unrecognised step")]
        )
    )
)

(define (walk steps map [from "AAA"] [count 0])
    (printf "From: ~A\n" from)
    (if (string=? from "ZZZ")
        count
        (let ([next-step (car steps)]
                [remaining-steps (cdr steps)]
            )

            (walk 
                (append remaining-steps (list next-step))
                map
                (take-step next-step map from)
                (+ count 1)
            )
        )
    )
)

(define (read-map file [map-acc '()])
    (let ([line (read-line file)])
        (if (eof-object? line)
            map-acc

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

