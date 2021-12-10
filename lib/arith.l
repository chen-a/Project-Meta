(define +
    (lambda (args)
        (if (eq? args nil)
            0
            (add (fst args) (+ (snd args))))))

(define -
    (lambda (args)
        if (eq? (snd args) nil)
            sub 0 (fst args)
            ())))
(define -                           -- not finished (should only deal with lists as args)
    (lambda (args)
        (if (list? args)
            (if (eq? args nil)
                0
                (sub (fst args) (- (snd args))))
            (sub 0 args))))

(define *
    (lambda (args)
        (if (eq? args nil)
            1
            (mult (fst args) (* (snd args))))))

(define /                        -- not finished (should only deal with lists as args)
    (lambda (args)
        (if (list? args)
            (if (eq? args nil)
                1
                (div (fst args) (/ (snd args))))
            (div 1 args))))