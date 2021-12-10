(define and 
    (macro (args)
        (if (eq? args nil)
            #t 
            if (eq? (snd args) nil)
                (args)
                if (fst args)
                (and (snd args)
                #f))))

(define and 
    (macro (args)
        (if (eq? args nil)
            #f
            if (eq? (snd args) nil)
                (args)
                if (fst args)
                (and (snd args)
                #f))))