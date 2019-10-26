;; read-file produces a list whose elements are the expressions in the file.
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go: read in the database.
(define source (with-input-from-file "units.dat" read-file))


;;removes the repeated units from the uncompressed unit list
(define (remove-val val seq)
  (define (remove a b c)
    (cond ((null? b) c)
          ((equal? a (caar b)) (remove a (cdr b) c))
          (else (remove a (cdr b) (append c (list(car b)))))))
  (remove val seq (list) )) 


;;returns an exponent from a list
(define (get-exp quantity-list)
  (cadar quantity-list))


;;takes in a uncompressed unit list with multiple units of the
;;same unit and differt units and compresses
;;them by adding exponents and removing any extras from the list
(define (combine-U-norm-list U-norm-list comb-list)
  (cond ((null? U-norm-list) comb-list)
        ((list? (assoc (caar U-norm-list) comb-list))
         (combine-U-norm-list
          (cdr U-norm-list)
          (append(list
                  (append
                   (list (caar U-norm-list))
                   (list (+ (cadr (assoc (caar U-norm-list) U-norm-list))
                            (cadr (assoc (caar U-norm-list) comb-list))))))
                 (remove-val (caar U-norm-list) comb-list))))
        
        (else (combine-U-norm-list
               (cdr U-norm-list)
               (append comb-list
                       (list (car U-norm-list)) )))))


;;takes a ratio and expoenetiates them and returns a list 
(define (exponentiate-ratio ratio exponent)
  (map expt ratio exponent))


;;multiplys the unit list and creates a normalized list 
(define (multiply-unit unit exponent new-norm)
  (cond ((null? unit)
         new-norm)
        (else (multiply-unit
               (cdr unit)  exponent   
               (append
                (list  (list (caar unit)
                             (* (cadar unit) exponent))) new-norm)))))


;;creates a list of exponenets from the user
;;input passes the list of units before converted
;;to its base units
(define (exponent-list input exp-list)
  (cond ((null? input)
         exp-list)
        (else  (exponent-list
                (cdr input)
                (append exp-list (list (get-exp input)) )))))


;;gets the ratio from the unit list 
(define (get-u unit-list)
  (if (equal? (assoc (caar unit-list) source) #f)
      1
      (caadr (assoc (caar unit-list) source))))



;;get the quantity from the user input
(define (get-a unit-list)
  (if (not(list? (car unit-list)))
      (car unit-list)
      1))


;;returns a list of metric ratios takes in unit ratio
;; list and empty list
(define (unit-ratio expression ratio-list)
  (cond ((null? expression)
         ratio-list)
        (else (unit-ratio
               (cdr expression)
               (append ratio-list (list (get-u  expression)))))))


;;returns a normalized list uncondensed repeated
;;units (m 1)(m 1) and crates a new list multiplying the unit
;;values depending on the unit exponent
(define (normalize-U alist normlist)  
  (cond ((null? alist)
         normlist)
        
        ((not(list? (car alist)))
         (normalize-U  (cdr alist) normlist))
        ((equal? (assoc (caar alist) source) #f)
         (normalize-U
          (cdr alist) (append (list (car  alist)) normlist)))
        
        (else  (normalize-U
                (cdr alist)
                
                (multiply-unit 
                 (cdadr (assoc (caar alist) source))
                 (car (exponent-list alist (list)))
                 normlist)))))



;;takes a list of ratios and multiplys them togethe
;;returns the final ratio of all the units combined
(define (combine-ratios comb val)
  (if (null? comb) val
      (combine-ratios (cdr comb) (* (car comb) val))))



;;multiplys and divides au by v and returns the final
;;resul from the conversion calls the combine function
;; with the unit ratio and an exponent-list 
(define (get-au-over-v quan uni)
  
  (/ (* (get-a quan)  
        (combine-ratios
         (exponentiate-ratio
          (unit-ratio (cdr quan) (list))            
          (exponent-list (cdr quan) (list))) 1))
     
     (combine-ratios
      (exponentiate-ratio
       (unit-ratio  uni (list))
       (exponent-list  uni (list))) 1)))




;;checks to see if the units of both list are the same
;;if they are returns true and are valid units if not
;;returns false 
(define (are-u-v-equal? u-list v-list) 
  (cond ((null? u-list) #t)
        ((equal?      (list? (member (car u-list) v-list))  #t)
         (are-u-v-equal?  (cdr u-list)   v-list))
        (else #f)))


;;checks to see if the length of the combined normalize list
;;are of the same length. If they are not then they are not
;;compatible
(define (same-length? u-list v-list)
  (if (= (length u-list) (length v-list))
      (are-u-v-equal? u-list v-list)
      #f))


(define (convert quantity unit-list)
  (if (are-u-v-equal?
       (combine-U-norm-list  (normalize-U  quantity   (list)) (list))   
       (combine-U-norm-list  (normalize-U  unit-list   (list) ) (list)))   
      (append (list (get-au-over-v quantity unit-list))  unit-list)))

;;(convert '(25 (BTU 2)(dyn -1)(m -2)) '((n 1)))
;;(convert '(3 (lbm 1)(furlong 2)(min -2)) '((BTU 1)))
;;(convert '(2 (ft 1)) '((in 1)))
;;(convert '(10 (mph 1)) '((km 1)(hr -1)))
