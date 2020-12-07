#lang racket

(require racket/string)

(struct passport (byr iyr eyr hgt hcl ecl pid cid))

(define (hash->passport h)
  (passport
   (if (hash-has-key? h "byr") (hash-ref h "byr") null)
   (if (hash-has-key? h "iyr") (hash-ref h "iyr") null)
   (if (hash-has-key? h "eyr") (hash-ref h "eyr") null)
   (if (hash-has-key? h "hgt") (hash-ref h "hgt") null)
   (if (hash-has-key? h "hcl") (hash-ref h "hcl") null)
   (if (hash-has-key? h "ecl") (hash-ref h "ecl") null)
   (if (hash-has-key? h "pid") (hash-ref h "pid") null)
   (if (hash-has-key? h "cid") (hash-ref h "cid") null)))

(define (string->passport s)
  (let ([fields (string-split s " ")])
    (hash->passport (foldl
     (lambda (f h)
       (let* ([pair (string-split f ":")]
              [field-name (first pair)]
              [field-val (second pair)])
         (hash-set h field-name field-val)))
     (hash)
     fields))))

(define (passport-valid? p)
  (and
   (not (null? (passport-byr p)))
   (not (null? (passport-iyr p)))
   (not (null? (passport-eyr p)))
   (not (null? (passport-hgt p)))
   (not (null? (passport-hcl p)))
   (not (null? (passport-ecl p)))
   (not (null? (passport-pid p)))))

(define (parse-input input)
  (let* ([passport-lines (string-split input "\n\n")]
         [passport-lists (map
                          (lambda (line)
                            (string-join (string-split line "\n") " ")) passport-lines)])
    passport-lists))


(define input (file->string "./input.txt"))
(define passports (map string->passport (parse-input input)))

(print (format "Part 1: ~a" (length (filter-map passport-valid? passports))))
