;Write a single Boolean LISP function, called TREE-CONTAINS, 
;which takes two arguments N and TREE, and checks whether number
;N appears in the ordered tree TREE. 

;(TREE-CONTAINS 3 '((1 2 3) 7 8)) returns T  
;(TREE-CONTAINS 4 '((1 2 3) 7 8)) returns NIL 

(defun TREE-CONTAINS (N TREE)
	(cond 
		((numberp TREE) (= N TREE))                             ; check if the tree has only 1 number
    ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))    ; if N is greater than m, then go right
		((< N (second TREE)) (TREE-CONTAINS N (first TREE)))    ; if N is less than m, then go left
		(t nil)))                                               ; tree is empty then return NIL

;Write a single LISP function, called TREE-MAX, which takes one
;argument TREE, and returns the maximum number appearing in the 
;ordered tree TRE

;(TREE-MAX '((1 2 3) 7 8)) returns 8 

(defun TREE-MAX (TREE)
  (cond
    ((numberp TREE) TREE)                                    ; if the TREE has only 1 number return than value
    (t (TREE-MAX (third TREE)))))                            ; since the right number is greater then go right


;Write a single LISP function, called TREE-ORDER, which takes 
;one argument TREE, and returns an in-ordered list of the numbers
; appearing in the ordered tree TREE. 

;(TREE-ORDER 3) returns (3)  
;(TREE-ORDER '((1 2 3) 7 8)) returns (1 2 3 7 8)

(defun TREE-ORDER (TREE)
  (cond
    ((NULL TREE) nil)                                       ; check if the input is nil
    ((numberp TREE) (list TREE))                            ; check if the tree has only 1 number
    (t (append                                              ; append the tree in the pre order
              (TREE-ORDER (first TREE))
              (TREE-ORDER (second TREE))
              (TREE-ORDER (third TREE))))))

;Write a single LISP function, called SUB-LIST, that takes a list L and
;two non-negative integers START and LEN, and returns the sub-list of L 
;starting at position START and having length LEN. Assume that the first 
;element of L has position 0. 

; (SUB-LIST '(a b c d) 0 3) returns (a b c) 
; (SUB-LIST '(a b c d) 3 1) returns (d)  
; (SUB-LIST '(a b c d) 2 0) return s NIL 

(defun SUB-LIST (L START LEN)	
	(cond 
			((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))						; if the start is greater than 0 then decrease 
		  (t 				
			(cond 
					((> LEN 0) 																								; append to the list
				  (cons (first L) (SUB-LIST (cdr L) START (- LEN 1))))
				  (t NIL)))))

; Write a single LISP function, called SPLIT-LIST, that takes a list L,
; and returns a list of two lists L1 and L2, in that order, such that
; - L is the result of appending L1 and L2; 
; - Length of L2 minus length of L1 is 0 or 1

; (SPLIT-LIST '(a b c d)) returns ((a b) (c d))  
; (SPLIT-LIST '(a b c d e)) returns ((a b) (c d e))    NOTE: ((a b c) (d e)) is incorrect;  
; (SPLIT-LIST '(a b c d e f)) returns ((a b c) (d e f)) 

(defun SPLIT-LIST (L)
	(cond 
				((oddp (length L))																			; if the length is odd
		  	(list
				(SUB-LIST L 0 (/ (- (length L) 1) 2)) 														; get the length (l-1)/2
			    (SUB-LIST L (/ (- (length L) 1) 2) (- (length L) (/ (- (length L) 1) 2))
				)
			)
		  )
		  (t																								; if the length is even
			(list																						
				(SUB-LIST L 0 (/ (length L) 2))
				(SUB-LIST L (/ (length L) 2) (/ (length L) 2))												; length is l/2
			) 
		  )
	)
)

;Write a single LISP function, called BTREE-HEIGHT, which takes a binary tree TREE,
;and returns the height of TREE. Note that the height of a binary tree is defined 
;as the length of the longest path from the root node to the farthest leaf node. 
  
;(BTREE-HEIGHT 1) returns 0 
;(BTREE-HEIGHT '(1 2)) returns 1  
;(BTREE-HEIGHT '(1 (2 3))) returns 2 
;(BTREE-HEIGHT '((1 2) (3 4))) returns 2 
;(BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) returns 3 
;(BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) returns 3 

(defun BTREE-HEIGHT (TREE)
	(cond ((atom TREE) 0) 																			; if it is a leaf return 0
		((> (BTREE-HEIGHT (first TREE)) (BTREE-HEIGHT (second TREE)))								; if left has greater height go left
		 (+ 1 (BTREE-HEIGHT(first TREE))))	 
		(t 																						    ; if the right has greater height go right
		(+ 1 (BTREE-HEIGHT(second TREE)))) 
	)
)

;Write a single LISP function, called LIST2BTREE, that takes a non-empty list 
; of atoms LEAVES, and returns a binary tree such that 
 
;- The tree leaves are the elements of LEAVES; - For any internal (non-leaf) node 
;in the tree, the number of leaves in its right branch minus the   
; number of leaves in its left branch is 0 or 1.

;(LIST2BTREE '(1)) returns 1 
;(LIST2BTREE '(1 2)) returns (1 2)  
;(LIST2BTREE '(1 2 3)) returns (1 (2 3))  
;(LIST2BTREE '(1 2 3 4)) returns ((1 2) (3 4))  
;(LIST2BTREE '(1 2 3 4 5 6 7)) returns ((1 (2 3)) ((4 5) (6 7)))  
;(LIST2BTREE '(1 2 3 4 5 6 7 8)) returns (((1 2) (3 4)) ((5 6) (7 8))) 

(defun LIST2BTREE (LEAVES)
    (cond 
        ((= (length LEAVES) 1) (first LEAVES))														; if the LEAVES only length is 1 then return that element
        (t 
             (list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST LEAVES)))))))		; call the SPLIT function

;  Write a single LISP function, called BTREE2LIST, that takes a binary tree TREE as input,
; and returns a list of atoms (assume TREE follows the constraints we defined earlier). 
 
; - As the input is a binary tree, each node has at most 2 children; 
; - This function is the inverse of LIST2BTREE. That is, (BTREE2LIST (LIST2BTREE X)) = X for all lists of atoms X. 

; (BTREE2LIST 1) returns (1)
; (BTREE2LIST '(1 2)) returns (1 2) 
; (BTREE2LIST '(1 (2 3))) returns (1 2 3) 
; (BTREE2LIST '((1 2) (3 4))) returns (1 2 3 4) 
; (BTREE2LIST '((1 (2 3)) ((4 5) (6 7)))) returns (1 2 3 4 5 6 7) 
; (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) returns (1 2 3 4 5 6 7 8) 

(defun BTREE2LIST (TREE)
	(cond
		((atom TREE) (cons TREE NIL))												; check the statue that it only has 1 element
		(t
			(append (BTREE2LIST(first TREE))										; append everything to a list
			 (BTREE2LIST(second TREE)))
			)))

;Write a single Boolean LISP function, called IS-SAME, that takes two LISP expressions E1 and 
;E2 whose atoms are all numbers, and checks whether the expressions are identical. In this question
;, you can only use ‘=‘ to test equality (you cannot use ‘equal’). Recall that a LISP expression
; is either an atom or a list of LISP expressions.

;(IS-SAME  '((1 2 3) 7 8)  '((1 2 3) 7 8)) returns T 
;(IS-SAME  '(1 2 3 7 8)  '((1 2 3) 7 8)) returns NIL 

(defun IS-SAME (E1 E2)
    (cond 
		  ((and (NULL E1) (NULL E2)) t)														; if E1 and E2 are NULL then true
          ((and (numberp E1) (numberp E2)) (= E1 E2))										; number must be the same
          ((and (listp E1) (listp E2))																	
           (and (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))				; if they are list then check each parts
          	(t nil)))