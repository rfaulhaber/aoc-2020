#lang racket

(require srfi/13) 

(struct passport (byr iyr eyr hgt hcl ecl pid cid))

(define (between test mn mx)
  (and (>= test mn) (<= test mx)))

(define (valid-byr byr)
  (between byr 1920 2002))

(define (valid-iyr iyr)
  (between iyr 2010 2020))

(define (valid-eyr eyr)
  (between eyr 2020 2030))

(define (valid-hgt hgt)
  (let* ([idx (or (string-contains hgt "in") (string-contains hgt "cm"))]
        [val  (if idx (string->number (substring hgt 0 idx)) null)]
        [ty (if idx (substring hgt idx) null)])
    (if (not idx)
        #f
        (cond
          [(string=? ty "cm") (between val 150 193)]
          [(string=? ty "in") (between val 59 76)]
          )
        )))

(define (valid-hcl hcl)
  (regexp-match-exact? #px"#([a-z0-9]{6})" hcl))

(define (valid-ecl ecl)
  (member ecl (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth")))

(define (valid-pid pid)
  (regexp-match-exact? #px"([0-9]{9})" pid))

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

(define (passport-valid2? p)
  (and
   (and (not (null? (passport-byr p))) (valid-byr (string->number (passport-byr p))))
   (and (not (null? (passport-iyr p))) (valid-iyr (string->number (passport-iyr p))))
   (and (not (null? (passport-eyr p))) (valid-eyr (string->number (passport-eyr p))))
   (and (not (null? (passport-hgt p))) (valid-hgt (passport-hgt p)))
   (and (not (null? (passport-hcl p))) (valid-hcl (passport-hcl p)))
   (and (not (null? (passport-ecl p))) (valid-ecl (passport-ecl p)))
   (and (not (null? (passport-pid p))) (valid-pid (passport-pid p)))))

(define (parse-input input)
  (let* ([passport-lines (string-split input "\n\n")]
         [passport-lists (map
                          (lambda (line)
                            (string-join (string-split line "\n") " ")) passport-lines)])
    passport-lists))


(define input (file->string "./input.txt"))
(define passports (map string->passport (parse-input input)))

(println (format "Part 1: ~a" (length (filter-map passport-valid? passports))))
(println (format "Part 2: ~a" (length (filter-map passport-valid2? passports))))
