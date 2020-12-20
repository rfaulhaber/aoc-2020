#lang racket

(define (parse-input input)
  (let* ([lines (string-split input "\n")]
         [pairs (map
                 (lambda (line)
                   (let* ([pair (string-split line " ")]
                          [instr (car pair)]
                          [val (string->number (cadr pair))])
                     (cons instr val)))
                 lines)])
    pairs))

(define input (parse-input (file->string "./input.txt")))
(define practice (parse-input (file->string "./practice.txt")))

(define stack%
  (class object%
    (init items)
    (super-new)
    (define internal-list items)
    (define/public (pop)
      (cond
        [(stack-empty?) null]
        [else
         (let ([top (car internal-list)]
               [rest (cdr internal-list)])
           (set! internal-list rest)
           top)]))
    (define/public (get-items)
      internal-list)
    (define/public (nth n)
      (list-ref internal-list n))
    (define/public (stack-empty?)
      (empty? internal-list))
    (define/public (add-item! new-item)
      (set! internal-list (cons new-item internal-list))
    )))

(define machine%
  (class object%
    (init instructions)
    (super-new)
    (define insts instructions)
    (define stack (new stack% [items '()]))
    (define acc 0)
    (define pointer 0)
    (define pointer-flip 0)
    (define target (length insts))
    (define visited (mutable-set))
    (define last-jmp-idx (last (indexes-where insts (lambda (instr) (equal? (car instr) "jmp")))))
    (define/public (eval-once)
      (let ([next-instruction (list-ref insts pointer)])
        (if (set-member? visited pointer)
            acc
            (begin
              (set-add! visited pointer)
              (set-pointer! (eval-inst next-instruction))
              (eval-once)))))
    (define/public (eval-with-fix start)
      (cond
        [(>= pointer target) acc]
        [(set-member? visited pointer) (begin
                                         (set! pointer-flip 0)
                                         (set-clear! visited)
                                         (eval-with-fix pointer-flip))]
        [else (let ([next-instruction (list-ref insts pointer)])
            (begin
              (set-add! visited pointer)
              (set-pointer! (eval-inst-with-fix next-instruction))
              (eval-with-fix pointer)))]))
    (define/private (eval-inst instruction)
      (let ([instr (car instruction)]
            [arg (cdr instruction)])
        (cond
          [(equal? "acc" instr) (begin
                                  (set! acc (+ acc arg))
                                  (inc-pointer 1))]
          [(equal? "nop" instr) (inc-pointer 1)]
          [(equal? "jmp" instr) (inc-pointer arg)])))
    (define/private (eval-inst-with-fix instruction)
      (let ([instr (car instruction)]
            [arg (cdr instruction)])
        (cond
          [(equal? "acc" instr) (begin
                                  (set! acc (+ acc arg))
                                  (inc-pointer 1))]
          [(equal? "nop" instr) (if (= pointer-flip pointer)
                                    (inc-pointer arg)
                                    (inc-pointer 1))]
          [(equal? "jmp" instr) (if (= pointer-flip pointer)
                                    (inc-pointer 1)
                                    (inc-pointer arg))])))
    (define/private (inc-pointer amt)
      (+ pointer amt))
    (define/private (set-pointer! val)
      (set! pointer val))
    (define/private (is-last-jump jmp-inst)
      (let ([idx (index-of insts jmp-inst)])
        (>= idx last-jmp-idx))
      )
    ))

(define m1 (new machine% [instructions practice]))
(define m2 (new machine% [instructions input]))
(printf "Part 1: ~a" (send m1 eval-once))
(printf "Part 2: ~a" (send m2 eval-with-fix 0))

;; (define s (new stack% [items (list 1 2 3)]))
;; (send s add-item 4)
