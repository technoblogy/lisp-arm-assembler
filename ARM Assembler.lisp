; ARM Thumb Assembler for uLisp - Version 9 - 6th August 2024
; see http://www.ulisp.com/show?2YRU
;

; Extract register number
(defun regno (sym)
  (case sym (sp 13) (lr 14) (pc 15)
    (t (read-from-string (subseq (string sym) 1)))))

; Pack arguments into bit fields
(defun emit (bits &rest args)
  (let ((word 0) (shift -28))
    (mapc #'(lambda (value)
              (let ((width (logand (ash bits shift) #xf)))
                (incf shift 4)
                (unless (zerop (ash value (- width))) (error "Won't fit"))
                (setq word (logior (ash word width) value))))
          args)
    word))

(defun offset (label) (ash (- label *pc* 4) -1))

; data

(defun $word (val)
  (append
   (unless (zerop (mod *pc* 4)) (list ($nop)))
   (list (logand val #xffff) (logand (ash val -16) #xffff))))

; Shared routines, ordered by first four bits

; lsl lsr 0

(defun lsl-lsr-0 (op immed5 argm argd)
  (emit #x41533000 0 op immed5 (regno argm) (regno argd)))

; asr 0

(defun asr-0 (op immed5 argm argd)
  (emit #x41533000 1 op immed5 (regno argm) (regno argd)))

; add sub 1

(defun add-sub-1 (op argd argn argm)
  (cond
   ((numberp argm)
    (emit #x61333000 #b000111 op argm (regno argn) (regno argd)))
   ((null argm)
    (emit #x61333000 #b000110 op (regno argn) (regno argd) (regno argd)))
   (t
    (emit #x61333000 #b000110 op (regno argm) (regno argn) (regno argd)))))

; mov sub 2 3

(defun mov-sub-2-3 (op2 op argd immed8)
  (emit #x41380000 op2 op (regno argd) immed8))

; add mov 4

(defun add-mov-4 (op argd argm)
  (let ((rd (regno argd))
        (rm (regno argm)))
    (cond
     ((and (>= rd 8) (>= rm 8))
      (emit #x61333000 #b010001 op #b011 (- rm 8) (- rd 8)))
     ((>= rm 8)
      (emit #x61333000 #b010001 op #b001 (- rm 8) rd))
     ((>= rd 8)
      (emit #x61333000 #b010001 op #b010 rm (- rd 8))))))

; reg-reg

(defun reg-reg (op argd argm)
  (emit #xa3300000 op (regno argm) (regno argd)))

; bx blx 4

(defun bx-blx (op argm)
  (emit #x81430000 #b01000111 op (regno argm) 0))

; str ldr 4, 6, 9

(defun str-ldr (op argd arg2)
  (cond
   ((numberp arg2)
    (when (= op 0) (error "str not allowed with label"))
    (let ((arg (- (truncate (+ arg2 2) 4) (truncate *pc* 4) 1)))
      (emit #x41380000 4 1 (regno argd) (max 0 arg))))
   ((listp arg2)
    (let ((argn (first arg2))
          (immed (or (eval (second arg2)) 0)))
      (unless (zerop (mod immed 4)) (error "not multiple of 4"))
      (cond
       ((eq (regno argn) 15)
        (when (= op 0) (error "str not allowed with pc"))
        (emit #x41380000 4 1 (regno argd) (truncate immed 4)))
       ((eq (regno argn) 13)
        (emit #x41380000 9 op (regno argd) (truncate immed 4)))
       (t
        (emit #x41533000 6 op (truncate immed 4) (regno argn) (regno argd))))))
   (t (error "illegal argument"))))

(defun str-ldr-5 (op argd arg2)
  (cond
   ((listp arg2)
    (let ((argn (first arg2))
          (argm (second arg2)))
      (emit #x43333000 5 op (regno argm) (regno argn) (regno argd))))
   (t (error "illegal argument"))))

; add-10

(defun add-10 (op argd immed8)
  (emit #x41380000 #b1010 op (regno argd) (truncate immed8 4)))

; add-sub-11

(defun add-sub-11 (op immed7)
  (emit #x81700000 #b11010000 op (truncate immed7 4)))

; push pop 11

(defun push-pop (op lst)
  (let ((byte 0)
        (r 0))
    (mapc #'(lambda (x) 
              (cond
               ((and (= op 0) (eq x 'lr)) (setq r 1))
               ((and (= op 1) (eq x 'pc)) (setq r 1))
               (t (setq byte (logior byte (ash 1 (regno x))))))) lst)
    (emit #x41218000 11 op 2 r byte)))

; b cond 13

(defun b-cond-13 (cnd label)
  (let ((soff8 (logand (offset label) #xff)))
    (emit #x44800000 13 cnd soff8)))

(defun cpside (op aif)
  (emit #xb1130000 #b10110110011 op 0 aif))

; Alphabetical list of mnemonics

(defun $adc (argd argm)
  (reg-reg #b0100000101 argd argm))

(defun $add (argd argn &optional argm)
  (cond
   ((numberp argm)
    (cond
     ((eq (regno argn) 15)
      (add-10 0 argd argm))
     ((eq (regno argn) 13)
      (add-10 1 argd argm))
     (t (add-sub-1 0 argd argn argm))))
   ((and (numberp argn) (null argm))
    (cond
     ((eq (regno argd) 13)
      (add-sub-11 0 argn))
     (t
      (mov-sub-2-3 3 0 argd argn))))
   (t
    (cond
     ((or (>= (regno argd) 8) (>= (regno argn) 8))
      (add-mov-4 0 argd argn))
     (t
      (add-sub-1 0 argd argn argm))))))

(defun $and (argd argm)
  (reg-reg #b0100000000 argd argm))

(defun $asr (argd argm &optional arg2)
  (cond
   ((numberp arg2)
    (asr-0 0 arg2 argm argd))
   ((numberp argm)
    (asr-0 0 argm argd argd))
   (t
    (reg-reg #b0100000100 argd argm))))

(defun $b (label)
  (emit #x41b00000 #xe 0 (logand (offset label) #x7ff)))

(defun $bcc (label)
  (b-cond-13 3 label))

(defun $bcs (label)
  (b-cond-13 2 label))

(defun $beq (label)
  (b-cond-13 0 label))

(defun $bge (label)
  (b-cond-13 10 label))

(defun $bgt (label)
  (b-cond-13 12 label))

(defun $bhi (label)
  (b-cond-13 8 label))

(defun $bhs (label)
  (b-cond-13 2 label))

(defun $ble (label)
  (b-cond-13 13 label))

(defun $blo (label)
  (b-cond-13 3 label))

(defun $blt (label)
  (b-cond-13 11 label))

(defun $bmi (label)
  (b-cond-13 4 label))

(defun $bne (label)
  (b-cond-13 1 label))

(defun $bpl (label)
  (b-cond-13 5 label))

(defun $bic (argd argm)
  (reg-reg #b0100001110 argd argm))

(defun $bl (label)
  (list
   (emit #x5b000000 #b11110 (logand (ash (offset label) -11) #x7ff))
   (emit #x5b000000 #b11111 (logand (offset label) #x7ff))))

(defun $blx (argm)
  (bx-blx 1 argm))

(defun $bx (argm)
  (bx-blx 0 argm))

(defun $cmn (argd argm)
  (reg-reg #b0100001011 argd argm))

(defun $cmp (argd argm)
  (cond
   ((numberp argm)
    (mov-sub-2-3 2 1 argd argm))
   (t
    (reg-reg #b0100001010 argd argm))))

(defun $cpsid (aif)
  (cpside 1 aif))

(defun $cpsie (aif)
  (cpside 0 aif))
    
(defun $eor (argd argm)
  (reg-reg #b0100000001 argd argm))

(defun $ldr (argd arg2)
  (str-ldr 1 argd arg2))

(defun $ldrb (argd arg2)
  (str-ldr-5 6 argd arg2))

(defun $ldrh (argd arg2)
  (str-ldr-5 5 argd arg2))

(defun $ldrsb (argd arg2)
  (str-ldr-5 3 argd arg2))

(defun $ldrsh (argd arg2)
  (str-ldr-5 7 argd arg2))

(defun $lsl (argd argm &optional arg2)
  (cond
   ((numberp arg2)
    (lsl-lsr-0 0 arg2 argm argd))
   ((numberp argm)
    (lsl-lsr-0 0 argm argd argd))
   (t
    (reg-reg #b0100000010 argd argm))))

(defun $lsr (argd argm &optional arg2)
  (cond
   ((numberp arg2)
    (lsl-lsr-0 1 arg2 argm argd))
   ((numberp argm)
    (lsl-lsr-0 1 argm argd argd))
   (t
    (reg-reg #b0100000011 argd argm))))

(defun $mov (argd argm)
  (cond
   ((numberp argm)
    (mov-sub-2-3 2 0 argd argm))
   ((or (>= (regno argd) 8) (>= (regno argm) 8))
    (add-mov-4 1 argd argm))
   (t ; Synonym of LSLS Rd, Rm, #0
    (lsl-lsr-0 0 0 argm argd))))

(defun $mul (argd argm)
  (reg-reg #b0100001101 argd argm))

(defun $mvn (argd argm)
  (reg-reg #b0100001111 argd argm))

(defun $neg (argd argm)
  (reg-reg #b0100001001 argd argm))

(defun $nop () ; mov r8,r8
  (add-mov-4 1 'r8 'r8))

(defun $orr (argd argm)
  (reg-reg #b0100001100 argd argm))

(defun $push (lst)
  (push-pop 0 lst))

(defun $pop (lst)
  (push-pop 1 lst))

(defun $rev (argd argm)
  (reg-reg #b1011101000 argd argm))

(defun $rev16 (argd argm)
  (reg-reg #b1011101001 argd argm))

(defun $revsh (argd argm)
  (reg-reg #b1011101010 argd argm))

(defun $ror (argd argm)
  (reg-reg #b0100000111 argd argm))

(defun $sbc (argd argm)
  (reg-reg #b0100000110 argd argm))

(defun $str (argd arg2)
  (str-ldr 0 argd arg2))

(defun $strb (argd arg2)
  (str-ldr-5 2 argd arg2))

(defun $sub (argd argn &optional argm)
  (cond
   ((not (numberp argn))
    (add-sub-1 1 argd argn argm))
   ((eq (regno argd) 13)
      (add-sub-11 1 argn))
   (t
    (mov-sub-2-3 3 1 argd argn))))

(defun $sxtb (argd argm)
  (reg-reg #b1011001001 argd argm))

(defun $sxth (argd argm)
  (reg-reg #b1011001000 argd argm))

(defun $tst (argd argm)
  (reg-reg #b0100001000 argd argm))

(defun $uxtb (argd argm)
  (reg-reg #b1011001011 argd argm))

(defun $uxth (argd argm)
  (reg-reg #b1011001010 argd argm))