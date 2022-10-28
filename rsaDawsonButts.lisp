;;; Dawson Butts
;;; The following is boilerplate to get the testing framework working
;;; with the new version of ProofPad

(include-book "doublecheck" :uncertified-okp t :dir :teachpacks)
(encapsulate nil
  (set-state-ok t)
  (program)
  (defun check-expect-fn (left right sexpr state)
    (if (equal left right)
        (prog2$ (cw "check-expect succeeded: ~x0~%" sexpr)
          (mv nil :invisible state))
      (er soft nil "check-expect failed: ~x0
      Expected: ~x1
      Actual:   ~x2" sexpr right left)))
  (defmacro check-expect (&whole sexpr left right)
    `(check-expect-fn ,left ,right (quote ,sexpr) state))            
  (defun random-element-fn (lst state)
    (mv-let (i state)
            (random-between 0 (1- (len lst)))
            (mv (nth i lst) state)))
  (defmacro random-element (lst)
    `(random-element-fn ,lst state))
  (logic))

;;; End boilerplate

(include-book "arithmetic-5/top" :dir :system)

;;; Reminder: You must have at least 5 unit tests for each function you define.


;;; PROBLEM DESCRIPTION
;;;
;;; In this assignment, you will use modular arithmetic to implement encryption
;;; and decryption functions using the RSA algorithm (the backbone of encryption
;;; in the internet until fairly recently).

;;; The encryption algorithm uses prime numbers extensively. Here are some prime
;;; numbers that you can use for testing.

(defconst *primes* '(2 3 5 7 11 13 17 19 23 29 31
   37 41 43 47 53 59 61 67 71 73 79 83 89
   97 101 103 107 109 113 127 131 137 139
   149 151 157 163 167 173 179 181 191 193
   197 199 211 223 227 229 233 239 241 251
   257 263 269 271 277 281 283 293 307 311
   313 317 331 337 347 349 353 359 367 373
   379 383 389 397 401 409 419 421 431 433
   439 443 449 457 461 463 467 479 487 491
   499 503 509 521 523 541 547 557 563 569
   571 577 587 593 599 601 607 613 617 619
   631 641 643 647 653 659 661 673 677 683
   691 701 709 719 727 733 739 743 751 757
   761 769 773 787 797 809 811 821 823 827
   829 839 853 857 859 863 877 881 883 887
   907 911 919 929 937 941 947 953 967 971
   977 983 991 997 1009 1013 1019 1021 1031
   1033 1039 1049 1051 1061 1063 1069 1087
   1091 1093 1097 1103 1109 1117 1123 1129
   1151 1153 1163 1171 1181 1187 1193 1201
   1213 1217 1223 1229 1231 1237 1249 1259
   1277 1279 1283 1289 1291 1297 1301 1303
   1307 1319 1321 1327 1361 1367 1373 1381
   1399 1409 1423 1427 1429 1433 1439 1447
   1451 1453 1459 1471 1481 1483 1487 1489
   1493 1499 1511 1523 1531 1543 1549 1553
   1559 1567 1571 1579 1583 1597 1601 1607
   1609 1613 1619 1621 1627 1637 1657 1663
   1667 1669 1693 1697 1699 1709 1721 1723
   1733 1741 1747 1753 1759 1777 1783 1787
   1789 1801 1811 1823 1831 1847 1861 1867
   1871 1873 1877 1879 1889 1901 1907 1913
   1931 1933 1949 1951 1973 1979 1987 1993
   1997 1999 2003 2011 2017 2027 2029 2039
   2053 2063 2069 2081 2083 2087 2089 2099
   2111 2113 2129 2131 2137 2141 2143 2153
   2161 2179 2203 2207 2213 2221 2237 2239
   2243 2251 2267 2269 2273 2281 2287 2293
   2297 2309 2311 2333 2339 2341 2347 2351
   2357 2371 2377 2381 2383 2389 2393 2399
   2411 2417 2423 2437 2441 2447 2459 2467
   2473 2477 2503 2521 2531 2539 2543 2549
   2551 2557 2579 2591 2593 2609 2617 2621
   2633 2647 2657 2659 2663 2671 2677 2683
   2687 2689 2693 2699 2707 2711 2713 2719
   2729 2731 2741 2749 2753 2767 2777 2789
   2791 2797 2801 2803 2819 2833 2837 2843
   2851 2857 2861 2879 2887 2897 2903 2909
   2917 2927 2939 2953 2957 2963 2969 2971
   2999 3001 3011 3019 3023 3037 3041 3049
   3061 3067 3079 3083 3089 3109 3119 3121
   3137 3163 3167 3169 3181 3187 3191 3203
   3209 3217 3221 3229 3251 3253 3257 3259
   3271 3299 3301 3307 3313 3319 3323 3329
   3331 3343 3347 3359 3361 3371 3373 3389
   3391 3407 3413 3433 3449 3457 3461 3463
   3467 3469 3491 3499 3511 3517 3527 3529
   3533 3539 3541 3547 3557 3559 3571 3581
   3583 3593 3607 3613 3617 3623 3631 3637
   3643 3659 3671 3673 3677 3691 3697 3701
   3709 3719 3727 3733 3739 3761 3767 3769
   3779 3793 3797 3803 3821 3823 3833 3847
   3851 3853 3863 3877 3881 3889 3907 3911
   3917 3919 3923 3929 3931 3943 3947 3967
   3989 4001 4003 4007 4013 4019 4021 4027
   4049 4051 4057 4073 4079 4091 4093 4099
   4111 4127 4129 4133 4139 4153 4157 4159
   4177 4201 4211 4217 4219 4229 4231 4241
   4243 4253 4259 4261 4271 4273 4283 4289
   4297 4327 4337 4339 4349 4357 4363 4373
   4391 4397 4409 4421 4423 4441 4447 4451
   4457 4463 4481 4483 4493 4507 4513 4517
   4519 4523 4547 4549 4561 4567 4583 4591
   4597 4603 4621 4637 4639 4643 4649 4651
   4657 4663 4673 4679 4691 4703 4721 4723
   4729 4733 4751 4759 4783 4787 4789 4793
   4799 4801 4813 4817 4831 4861 4871 4877
   4889 4903 4909 4919 4931 4933 4937 4943
   4951 4957 4967 4969 4973 4987 4993 4999
   5003 5009 5011 5021 5023 5039 5051 5059
   5077 5081 5087 5099 5101 5107 5113 5119
   5147 5153 5167 5171 5179 5189 5197 5209
   5227 5231 5233 5237 5261 5273 5279 5281
   5297 5303 5309 5323 5333 5347 5351 5381
   5387 5393 5399 5407 5413 5417 5419 5431
   5437 5441 5443 5449 5471 5477 5479 5483
   5501 5503 5507 5519 5521 5527 5531 5557
   5563 5569 5573 5581 5591 5623 5639 5641
   5647 5651 5653 5657 5659 5669 5683 5689
   5693 5701 5711 5717 5737 5741 5743 5749
   5779 5783 5791 5801 5807 5813 5821 5827
   5839 5843 5849 5851 5857 5861 5867 5869
   5879 5881 5897 5903 5923 5927 5939 5953
   5981 5987 6007 6011 6029 6037 6043 6047
   6053 6067 6073 6079 6089 6091 6101 6113
   6121 6131 6133 6143 6151 6163 6173 6197
   6199 6203 6211 6217 6221 6229 6247 6257
   6263 6269 6271 6277 6287 6299 6301 6311
   6317 6323 6329 6337 6343 6353 6359 6361
   6367 6373 6379 6389 6397 6421 6427 6449
   6451 6469 6473 6481 6491 6521 6529 6547
   6551 6553 6563 6569 6571 6577 6581 6599
   6607 6619 6637 6653 6659 6661 6673 6679
   6689 6691 6701 6703 6709 6719 6733 6737
   6761 6763 6779 6781 6791 6793 6803 6823
   6827 6829 6833 6841 6857 6863 6869 6871
   6883 6899 6907 6911 6917 6947 6949 6959
   6961 6967 6971 6977 6983 6991 6997 7001
   7013 7019 7027 7039 7043 7057 7069 7079
   7103 7109 7121 7127 7129 7151 7159 7177
   7187 7193 7207 7211 7213 7219 7229 7237
   7243 7247 7253 7283 7297 7307 7309 7321
   7331 7333 7349 7351 7369 7393 7411 7417
   7433 7451 7457 7459 7477 7481 7487 7489
   7499 7507 7517 7523 7529 7537 7541 7547
   7549 7559 7561 7573 7577 7583 7589 7591
   7603 7607 7621 7639 7643 7649 7669 7673
   7681 7687 7691 7699 7703 7717 7723 7727
   7741 7753 7757 7759 7789 7793 7817 7823
   7829 7841 7853 7867 7873 7877 7879 7883
   7901 7907 7919 7927 7933 7937 7949 7951
   7963 7993 8009 8011 8017 8039 8053 8059
   8069 8081 8087 8089 8093 8101 8111 8117
   8123 8147 8161 8167 8171 8179 8191 8209
   8219 8221 8231 8233 8237 8243 8263 8269
   8273 8287 8291 8293 8297 8311 8317 8329
   8353 8363 8369 8377 8387 8389 8419 8423
   8429 8431 8443 8447 8461 8467 8501 8513
   8521 8527 8537 8539 8543 8563 8573 8581
   8597 8599 8609 8623 8627 8629 8641 8647
   8663 8669 8677 8681 8689 8693 8699 8707
   8713 8719 8731 8737 8741 8747 8753 8761
   8779 8783 8803 8807 8819 8821 8831 8837
   8839 8849 8861 8863 8867 8887 8893 8923
   8929 8933 8941 8951 8963 8969 8971 8999
   9001 9007 9011 9013 9029 9041 9043 9049
   9059 9067 9091 9103 9109 9127 9133 9137
   9151 9157 9161 9173 9181 9187 9199 9203
   9209 9221 9227 9239 9241 9257 9277 9281
   9283 9293 9311 9319 9323 9337 9341 9343
   9349 9371 9377 9391 9397 9403 9413 9419
   9421 9431 9433 9437 9439 9461 9463 9467
   9473 9479 9491 9497 9511 9521 9533 9539
   9547 9551 9587 9601 9613 9619 9623 9629
   9631 9643 9649 9661 9677 9679 9689 9697
   9719 9721 9733 9739 9743 9749 9767 9769
   9781 9787 9791 9803 9811 9817 9829 9833
   9839 9851 9857 9859 9871 9883 9887 9901
   9907 9923 9929 9931 9941 9949 9967 9973))

;;; TODO #1
;;; In the last assignment, you wrote the function modinv that computes the
;;; inverse of a number modulo another. Copy the definition here. You will also
;;; need to copy any functions that modinv depends on. Be sure to copy the
;;; unit tests, but you do not have to copy any little theories.
(defun egcd-aux (r1 r2 s1 s2 t1 t2)
  (if (zp r2) 
      (list r1 s1 t1)
      (if (integerp r1)
   	 	 (egcd-aux r2 (- r1 (* (floor r1 r2) r2)) 
                    	s2 (- s1 (* (floor r1 r2) s2)) 
                    	t2 (- t1 (* (floor r1 r2) t2)))
      	  (list r1 s1 t1))))
(check-expect (egcd-aux 26 20 1 0 0 1) '(2 -3 4))
(check-expect (egcd-aux 30 21 1 2 -3 4) '(3 4 18))
(check-expect (egcd-aux 42 34 5 6 7 2) '(2 10 -18))
(check-expect (egcd-aux 12 4 3 -1 3 4) '(4 -1 4))
(check-expect (egcd-aux 60 3 12 5 8 10) '(3 5 10))


(defun egcd (a b)
  (egcd-aux a b 1 0 0 1)
  )
(check-expect (egcd 26 20) '(2 -3 4))
(check-expect (egcd 30 21) '(3 -2 3))
(check-expect (egcd 42 34) '(2 -4 5))
(check-expect (egcd 12 4) '(4 0 1))
(check-expect (egcd 60 3) '(3 0 1))


(deflabel todo-1)
(defun modinv (b a)
  (mod (third (egcd a b)) a)
  )
(check-expect (modinv 2 5) 3)
(check-expect (modinv 4 6) 5)
(check-expect (modinv 12 4) 0)
(check-expect (modinv 20 13) 2)
(check-expect (modinv 20 20) 1)


;;; TODO #2
;;; The function modinv is important, because it calculates its value quickly.
;;; There is another mathematical operation we need to compute quickly, and that
;;; is x^n mod k, where x^n is "x raised to the n". The ACL2 function expt computes
;;; x^n and the function mod computes mod k, so this is a one-liner in ACL2 -- but
;;; that would be too slow. So next we will implement a fast algorithm to compute
;;; this.
;;;
;;; The idea is simple. To compute x^n, you could multiply x by itself n times. Or,
;;; you can compute x^(n//2) and then square that. That assumes that n is even, but
;;; if it's not, you just need to multiply the result by x.
;;;
;;; E.g., to compute 2^6, you would do this:
;;; 1) 2^6 = (2^3)^2, then square that
;;; 2) 2^3 = 2*(2^1)^2
;;; 3) 2^1 = 2
;;; All told, that is 3 multiplications instead of the naive 5 multiplications it 
;;; takes to multiply 2 by itself 6 times. Not too impressive, but try it with
;;; 2^100 instead: that's 8 multiplications as opposed to 99. It's even more
;;; impressive the bigger n gets.
;;;
;;; There's another trick that you should do. Instead of calculating x^n and then
;;; taking the mod k part, do the mod k after every multiplication. The point here
;;; is to keep the numbers small. For example, what is 2^10 mod 3? You could calculate
;;; 2^10=1024 and then take the mod 3 of that, which is 1. Or, you could do this:
;;; 2^10 mod 3 = (2^5 mod 3)^2 mod 3
;;;            = (2*(2^2 mod 3)^2 mod 3)^2 mod 3
;;;            = (2*(1)^2 mod 3)^2 mod 3
;;;            = (2)^2 mod 3
;;;            = 1
;;; The biggest number I had to do in my head in that last computation was 4. This is
;;; very important, actually. Suppose you want to compute x^n mod k with 10-bit numbers.
;;; If you compute x^n first, how many bits do you need? x has 10 bits, so x^2 has 20
;;; bits, x^3 has 30 bits, and so on. That means x^n has 10*(2^10), or about 1,000 bits.
;;; x^n is roughly 10000...00000 with about 300 zeros. To put that in perspective, there
;;; are only 10000...000 with only 86 zeros protons in the universe. So even computing
;;; x^n may be prohibitive, just because the numbers are too large.
;;;
;;; Combining these observations gives this algorithm:
;;;
;;; x^n mod k = 1 if n = 0
;;;           = x mod k if n = 1
;;;           = (x^(n//2) mod k)^2 mod k if n is even
;;;           = x*(x^(n-1) mod k) mod k if n is odd
(deflabel todo-2)
(defun expt-mod (x n k)
  (if (zp n)
      1
      (if (equal n 1)
          (mod x k)
      		(if (equal 0 (mod n 2))
         		 (mod (expt (expt-mod x (floor n 2) k) 2) k )
         		 (mod (* x (expt-mod x (- n 1) k)) k)
            )
      )
   )   
 )
(check-expect (expt-mod 4 8 7) 2)
(check-expect (expt-mod 2 10 3) 1)
(check-expect (expt-mod 8 0 3) 1)
(check-expect (expt-mod 4 8 7) 2)
(check-expect (expt-mod 6 5 9) 0)
(check-expect (expt-mod 12 4 11) 1)


;;; TODO #3
;;; We want to test that expt-mod works properly, so write a little theory that the value
;;; it returns is equal to x^n mod k, where you use the ACL2 functions expt and mod to
;;; compute the result. Based on our discussion above, we have to keep the values of x,
;;; n, and k small, say from 1-1000.
(deflabel todo-3)
(defproperty-program expt-mod-works
  (x :value (random-between 1 1000)
   n :value (random-between 1 1000)
   k :value (random-between 1 1000))
   (equal (mod (expt x n) k)
          (expt-mod x n k))
  )

;;; TODO #4
;;; We can also test expt-mod indirectly, which is one of the powerful ideas behind
;;; little theories. The great mathematician Pierre de Fermat discovered that when
;;; p is a prime number and a is a positive integer less than p, then a^(p-1) mod p = 1.
;;; Write a little theory in ACL2 to test Fermat's discovery. 
;;; Hint: Remember to use (random-element L) to select a random element from the list L.
(deflabel todo-4)
(defproperty-program fermats-little-theorem
  ( p :value (random-element *primes*)
    a :value (random-between 1 (- p 1)))
  (equal (mod (expt a (- p 1)) p)
         1)
  )

;;; Observation: Fermat's Little Theorem gives us yet another way to compute the inverse
;;; of a mod p. Can you see the trick?

;;; Now, we turn our attention to encryption. 

;;; TODO #5 and #6
;;; We start with functions that map between characters and numbers. We will only
;;; have the characters A, B, ..., Z, and the special character _ (underscore), for a
;;; total of 27 possible characters. These first two functions map between a single
;;; character and number, and vice versa.
(deflabel todo-5)
(defun char-to-number (ch)
  (if (equal ch 'a)
      0
  (if (equal ch 'b)
      1
  (if (equal ch 'c)
      2
  (if (equal ch 'd)
      3
  (if (equal ch 'e)
      4
  (if (equal ch 'f)
      5
  (if (equal ch 'g)
      6
  (if (equal ch 'h)
      7
  (if (equal ch 'i)
      8
  (if (equal ch 'j)
      9
  (if (equal ch 'k)
      10
  (if (equal ch 'l)
      11
  (if (equal ch 'm)
      12
  (if (equal ch 'n)
      13
  (if (equal ch 'o)
      14
  (if (equal ch 'p)
      15
  (if (equal ch 'q)
      16
  (if (equal ch 'r)
      17
  (if (equal ch 's)
      18
  (if (equal ch 't)
      19
  (if (equal ch 'u)
      20
  (if (equal ch 'v)
      21
  (if (equal ch 'w)
      22
  (if (equal ch 'x)
      23
  (if (equal ch 'y)
      24
  (if (equal ch 'z)
      25
      26))))))))))))))))))))))))))
  )

(check-expect (char-to-number 'a) 0)
(check-expect (char-to-number 'e) 4)
(check-expect (char-to-number '_) 26)
(check-expect (char-to-number 'm) 12)
(check-expect (char-to-number 'q) 16)

(deflabel todo-6)
(defun number-to-char (n)
  (if (equal n 0)
      'a
  (if (equal n 1)
      'b
  (if (equal n 2)
      'c
  (if (equal n 3)
      'd
  (if (equal n 4)
      'e
  (if (equal n 5)
      'f
  (if (equal n 6)
      'g
  (if (equal n 7)
      'h
  (if (equal n 8)
      'i
  (if (equal n 9)
      'j
  (if (equal n 10)
      'k
  (if (equal n 11)
      'l
  (if (equal n 12)
      'm
  (if (equal n 13)
      'n
  (if (equal n 14)
      'o
  (if (equal n 15)
      'p
  (if (equal n 16)
      'q
  (if (equal n 17)
      'r
  (if (equal n 18)
      's
  (if (equal n 19)
      't
  (if (equal n 20)
      'u
  (if (equal n 21)
      'v
  (if (equal n 22)
      'w
  (if (equal n 23)
      'x
  (if (equal n 24)
      'y
  (if (equal n 25)
      'z
      '_))))))))))))))))))))))))))
  )

(check-expect (number-to-char 0) 'a)
(check-expect (number-to-char 4) 'e)
(check-expect (number-to-char 26) '_)
(check-expect (number-to-char 12) 'm)
(check-expect (number-to-char 19) 't)

;;; TODO #7 and #8
;;; Now we generalize the functions above to go from lists of characters to lists
;;; of numbers and vice versa.
(deflabel todo-7)
(defun charlist-to-numberlist (chars)
  (if (consp chars)
      (cons (char-to-number (first chars)) (charlist-to-numberlist (rest chars)))
      nil)
  )
(check-expect (charlist-to-numberlist '(h e l l o _ t h e r e)) '(7 4 11 11 14 26 19 7 4 17 4))
(check-expect (charlist-to-numberlist '(d a w s o n b u t t s)) '(3 0 22 18 14 13 1 20 19 19 18))
(check-expect (charlist-to-numberlist '(d o g s _ o r _ c a t s)) '(3 14 6 18 26 14 17 26 2 0 19 18))
(check-expect (charlist-to-numberlist '(a e _)) '(0 4 26))
(check-expect (charlist-to-numberlist '(a l a n _ m _ t u r i n g))
              '(0 11 0 13 26 12 26 19 20 17 8 13 6))

(deflabel todo-8)
(defun numberlist-to-charlist (numbers)
 (if (consp numbers)
     (cons (number-to-char (first numbers)) (numberlist-to-charlist (rest numbers)))
     nil)
  )
(check-expect (numberlist-to-charlist '(0 4 26)) '(a e _))
(check-expect (numberlist-to-charlist '(2 14 17 14 13 0)) '(c o r o n a))
(check-expect (numberlist-to-charlist '(2 14 12 15 20 19 4 17)) '(c o m p u t e r))
(check-expect (numberlist-to-charlist '(18 2 8 4 13 2 4)) '(s c i e n c e))
(check-expect (numberlist-to-charlist '(12 20 18 8 2)) '(m u s i c))


;;; TODO #9 and #10
;;; Now, encryption works on single numbers, so we need a function to combine
;;; a list of numbers into a single number. We'll be doing this in base 27 (since 
;;; we have 27 possible "digits"). But this is exactly like converting '(1 5 2) 
;;; to the number 251 and 173 to the list '(3 7 1) in base 10. Just use base 27 
;;; instead.
;;; Hint: Look at the videos on converting to base 10 and converting to base 2
;;; under computer arithmetic if you need help with these functions.
(deflabel todo-9)
(defun numberlist-to-number (numbers)
   (if (consp numbers)
       (+ (first numbers) (* 27 (numberlist-to-number (rest numbers))))
       0)
  )
(check-expect (numberlist-to-number '(0 4 26)) (+ 0 (* 27 4) (* 27 27 26)))
(check-expect (numberlist-to-number '(0 11 0 13 26 12 26 19 20 17 8 13 6))
              974618220957852213) ; Note, this result is too big to fit in 32 bits
(check-expect (numberlist-to-number '(2 3 12 8)) (+ 2 (* 27 3) (* 27 27 12) (* 27 27 27 8)))
(check-expect (numberlist-to-number '(22 3)) (+ 22 (* 27 3)))
(check-expect (numberlist-to-number '(2 2 2 2 2 2)) (+ 2 (* 27 2) (* 27 27 2) 
                                                       (* 27 27 27 2) (* 27 27 27 27 2)
                                                       (* 27 27 27 27 27 2)))

               
(deflabel todo-10)
(defun number-to-numberlist (number)
  (if (zp number)
      nil
      (cons (mod number 27) (number-to-numberlist (floor number 27))))
  )
(check-expect (number-to-numberlist 19062) '(0 4 26))
(check-expect (number-to-numberlist 20001) '(21 11 0 1))
(check-expect (number-to-numberlist 43) '(16 1))
(check-expect (number-to-numberlist 678) '(3 25))
(check-expect (number-to-numberlist 3048) '(24 4 4))


;;; TODO #11 and #12
;;; Next, we need to compute public and private keys for encryption. These are based on
;;; three magic numbers (which can be chosen at random):
;;;   * P (a prime number)
;;;   * Q (another prime number)
;;;   * K (a number with no factors in common with (P-1)*(Q-1) )
;;; From now on, we will always just choose K=457 (but usually other values are chosen).
;;; (Aside, not all values of K work, but K=457 works for all the primes in the list at
;;; the top of this file)
;;;
;;; Once you choose these values P, Q, and K, you can compute the following:
;;;
;;;   * N = P*Q
;;;   * D = 1/K (mod (P-1)*(Q-1))
;;;
;;; With those, you can define the public and private keys as follows:
;;;
;;;   * Public key  = (list N K)
;;;   * Private key = (list N D)
;;;
;;; You can give your public key to anybody, then they can encrypt a message using that
;;; public key. The encrypted message can be unencrypted, but only with the private key.
;;; So don't give your private key to anybody, and only you will be able to decrypt the
;;; messages people send you with your public key. Cool, huh?
;;;
;;; Hint: Use the fast version of modinv and expt-mod you defined previously.
(deflabel todo-11)
(defun public-key (p q)
  (list (* p q) 457)
  )
(check-expect (public-key 61 67) '(4087 457))
(check-expect (public-key 151 23) '(3473 457))
(check-expect (public-key 347 431) '(149557 457))
(check-expect (public-key 19 13) '(247 457))
(check-expect (public-key 197 7) '(1379 457))

(deflabel todo-12)
(defun private-key (p q)
   (list (* p q) (modinv 457 (* (- p 1) (- q 1))))
  )

(check-expect (private-key 61 67) '(4087 1993))
(check-expect (private-key 151 23) '(3473 1993))
(check-expect (private-key 347 431) '(149557 103853))
(check-expect (private-key 19 13) '(247 121))
(check-expect (private-key 197 7) '(1379 193))

;;; TODO #13 and #14
;;; Now that we have public and private keys, we can implement the encrypt and decrypt
;;; functions. 
;;;
;;; Anyone can encrypt a message for you with your public key. Here, "message" is a number M that
;;; must be smaller than the value N (=P*Q). The encrypted value is another number, equal to
;;; M^K mod N.  Note that K and N are in the public key, and M is the message that is being encrypted.
;;;
;;; Hint: Use the fast version of modinv and expt-mod you defined previously.
(deflabel todo-13)
(defun encrypt-message (message public-key)
  (expt-mod message (second public-key) (first public-key))
  )
(check-expect (encrypt-message 42 '(979 457)) 466)
(check-expect (encrypt-message 1066 '(3843 457)) 1066)
(check-expect (encrypt-message 68 '(2018 697)) 932)
(check-expect (encrypt-message 45 '(979 233)) 67)
(check-expect (encrypt-message 943 '(3843 3313)) 313)


;;; Only you can decrypt a message encrypted for you, because only you have your private key.
;;; Messages can be encrypted for you using your public key, then only you can decrypt them
;;; with your private key. The decryption algorithm is nearly identical. If you have an encrypted
;;; message C, it is decrypted as C^D mod N. Notice that the private key has all the information
;;; you need to recreate. 
;;; Hint: Don't be surprised if the encryption and decryption functions look very similar.
(deflabel todo-14)
(defun decrypt-message (crypt private-key)
  (expt-mod crypt (second private-key) (first private-key))
  )
(check-expect (decrypt-message 466 '(979 233)) 42)
(check-expect (decrypt-message 1066 '(3843 3313)) 1066)
(check-expect (decrypt-message 503 '(2018 697)) 1067)
(check-expect (decrypt-message 300 '(40837 12793)) 1464)
(check-expect (decrypt-message 987 '(26023 19793)) 16398)

;;; TODO #15
;;; If you take a close look at those check-expects, you will see that the decrypted messages
;;; simply reveal the previously encrypted message. This should generalize to any public-key
;;; and associated private-key, and any message.  Write a little theory that tests this.
;;; I.e., if you encrypt a message using the public key, then decrypt that using the private
;;; key, you should have the original message.
;;; Hint: I've taken care of choosing the primes (P, Q), and the message (M) in the defproperty
;;; below.
(deflabel todo-15)
(defproperty-program decrypt-inverts-encrypt
  (P :value (random-element *primes*)
   Q :value (random-element *primes*) :where (not (equal P Q))
   M :value (random-between 0 (1- (* (1- P) (1- Q)))))
   (equal m
          (decrypt-message (encrypt-message m (public-key p q)) (private-key p q)))
  )

;;; TODO #16 and #17
;;; Now let's use the functions defined previously to actually encode strings (as in lists of
;;; characters). Simply convert the list of characters to a list of numbers, then to a single
;;; number, then encrypt that number, turn back to list of numbers, then a list of characters. 
;;; And similarly for decryption.
;;; Hint: When you choose your primes P and Q for the check-expects, make sure the
;;; message is small enough to fit in the range 0 .. P*Q. Otherwise, it won't
;;; encrypt/decrypt properly. E.g., for Turing's name, I used P=1,450,476,077 and Q=2,450,476,447
;;; which are 32-bit primes. For context, practical encryption uses primes with 512 to 2,048 bits!
(deflabel todo-16)
(defun encrypt-charlist (charlist public-key)
  (numberlist-to-charlist (number-to-numberlist (encrypt-message 
               (numberlist-to-number (charlist-to-numberlist charlist)) public-key)))
  )

(check-expect (encrypt-charlist '(a e _) 
                                '(471899 457))
              '(I F G M))
(check-expect (encrypt-charlist '(a l a n _ m _ t u r i n g) 
                                (public-key 1450476077 2450476447))
              '(T D U U _ _ E Z T N _ C R))
(check-expect (encrypt-charlist '(d o g) 
                                (public-key 2576113159 503194313))
              '(G H Y X R B M Y P J U Z H))                                
(check-expect (encrypt-charlist '(c a t) 
                                (public-key 3266159929 1390334537))
              '(Z N S D Q E R J K S C M B))
(check-expect (encrypt-charlist '(c a m e l) 
                                (public-key 996563149 4158539029))
              '(C L C W C K T X X Z P L Y))


(deflabel todo-17)
(defun decrypt-charlist (charlist private-key)
 	(numberlist-to-charlist (number-to-numberlist (decrypt-message 
               (numberlist-to-number (charlist-to-numberlist charlist)) private-key)))
  )


(check-expect (decrypt-charlist '(I F G M) '(471899 147193))
              '(a e _))
(check-expect (decrypt-charlist '(T D U U _ _ E Z T N _ C R)
                                (private-key 1450476077 2450476447))
              '(a l a n _ m _ t u r i n g))
(check-expect (decrypt-charlist '(G H Y X R B M Y P J U Z H)
                                (private-key 2576113159 503194313))
              '(d o g))
(check-expect (decrypt-charlist '(Z N S D Q E R J K S C M B)
                                (private-key 3266159929 1390334537))
              '(c a t))
(check-expect (decrypt-charlist '(C L C W C K T X X Z P L Y)
                                (private-key 996563149 4158539029))
              '(c a m e l))

;;; Note: The above unit tests wouldn't have a chance to work if it weren't for the fast 
;;; implementations of modinv and expt-mod you wrote earlier.
;;; proof
