;
; ARM Thumb Assembler for ARM uLisp 3.1 - 20th February 2020
; see http://www.ulisp.com/show?2YRU
;

; Print 16-bit number in hex
(defun x (n)
  (princ "#x")
  (dotimes (j 4 nothing)
    (let ((d (logand (ash n (- (* j 4) 12)) #xf)))
      (princ
       (code-char (+ d (if (< d 10) (char-code #\0) (char-code #\W))))))))

; Print 32-bit number in hex
(defun x32 (n)
  (princ "#x")
  (dotimes (j 8 nothing)
    (let ((d (logand (ash n (- (* j 4) 28)) #xf)))
      (princ
       (code-char (+ d (if (< d 10) (char-code #\0) (char-code #\W))))))))

; Extract register number
(defun regno (sym)
  (case sym (sp 13) (lr 14) (pc 15)
    (t (read-from-string (subseq (string sym) 1)))))

; Pack arguments into bit fields
(defun emit (bits &rest args)
  (let ((word 0))
    (mapc #'(lambda (width value)
              (unless (zerop (ash value (- width))) (error "Won't fit"))
              (setq word (logior (ash word width) value)))
          bits args)
    word))

; Errors
(defun error (txt) (princ "(pc=") (x *pc*) (princ ") ") (princ txt) (terpri))

(defun offset (label) (ash (- label *pc* 4) -1))

; data

(defun $word (val)
  (append
   (unless (zerop (mod *pc* 4)) (list ($nop)))
   (list
    (emit '(16) (logand val #xffff))
    (emit '(16) (logand (ash val -16) #xffff)))))

; Shared routines, ordered by first four bits

; lsl lsr 0

(defun lsl-lsr-0 (op immed5 argm argd)
  (emit '(4 1 5 3 3) 0 op immed5 (regno argm) (regno argd)))

; add sub 1

(defun add-sub-1 (op argd argn argm)
  (let ((fields '(4 1 1 1 3 3 3)))
    (cond
   ((numberp argm)
    (emit fields 1 1 1 op argm (regno argn) (regno argd)))
   ((null argm)
    (emit fields 1 1 0 op (regno argn) (regno argd) (regno argd)))
   (t
    (emit fields 1 1 0 op (regno argm) (regno argn) (regno argd))))))

; mov sub 2 3

(defun mov-sub-2-3 (op2 op argd immed8)
  (emit '(4 1 3 8) op2 op (regno argd) immed8))

; add mov 4

(defun add-mov-4 (op argd argm)
  (let ((rd (regno argd))
        (rm (regno argm))
        (fields '(4 2 1 1 2 3 3)))
    (cond
     ((and (>= rd 8) (>= rm 8))
      (emit fields 4 1 op 0 3 (- rm 8) (- rd 8)))
     ((>= rm 8)
      (emit fields 4 1 op 0 1 (- rm 8) rd))
     ((>= rd 8)
      (emit fields 4 1 op 0 2 rm (- rd 8))))))

; and to mvn 4

(defun and-mvn-4 (op argd argm)
  (emit '(4 1 5 3 3) 4 0 op (regno argm) (regno argd)))

; bx blx 4

(defun bx-blx (op argm)
  (emit '(4 1 3 1 4 3) 4 0 7 op (regno argm) 0))

; str ldr 4, 6, 9

(defun str-ldr (op argd arg2)
  (let ((fields '(4 1 3 8)))
    (cond
     ((numberp arg2)
      (when (= op 0) (error "str not allowed with label"))
      (let ((arg (- (truncate (+ arg2 2) 4) (truncate *pc* 4) 1)))
        (emit fields 4 1 (regno argd) (max 0 arg))))
     ((listp arg2)
      (let ((argn (first arg2))
            (immed (second arg2)))
        (unless (zerop (mod immed 4)) (error "not multiple of 4"))
        (cond
         ((eq (regno argn) 15)
          (when (= op 0) (error "str not allowed with pc"))
          (emit fields 4 1 (regno argd) (truncate immed 4)))
         ((eq (regno argn) 13)
          (emit fields 9 op (regno argd) (truncate immed 4)))
         (t
          (emit '(4 1 5 3 3) 6 op (truncate immed 4) (regno argn) (regno argd))))))
     (t (error "illegal argument")))))

(defun str-ldr-5 (op argd arg2)
  (cond
   ((listp arg2)
    (let ((argn (first arg2))
          (argm (second arg2)))
      (emit '(4 3 3 3 3) 5 op (regno argm) (regno argn) (regno argd))))
   (t (error "illegal argument"))))

; add-sub-11

(defun add-sub-11 (op immed7)
  (emit '(4 4 1 7) 11 0 op (truncate immed7 4)))

; sxth-uxtb-11

(defun sxth-uxtb-11 (op argd argm)
  (emit '(4 4 2 3 3) 11 2 op (regno argm) (regno argd)))

; rev rev16 revsh 11

(defun rev-revsh-11 (op argd argm)
  (emit '(4 4 2 3 3) 11 10 op (regno argm) (regno argd)))

; push pop 11

(defun push-pop (op lst)
  (let ((byte 0)
        (r 0))
    (mapc #'(lambda (x) 
              (cond
               ((and (= op 0) (eq x 'lr)) (setq r 1))
               ((and (= op 1) (eq x 'pc)) (setq r 1))
               (t (setq byte (logior byte (ash 1 (regno x))))))) lst)
    (emit '(4 1 2 1 8) 11 op 2 r byte)))

; b cond 13

(defun b-cond-13 (cnd label)
  (let ((soff8 (logand (offset label) #xff)))
    (emit '(4 4 8) 13 cnd soff8)))

; Alphabetical list of mnemonics

(defun $adc (argd argm)
  (and-mvn-4 5 argd argm))

(defun $add (argd argn &optional argm)
  (cond
   ((numberp argn)
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
  (and-mvn-4 0 argd argm))

(defun $asr (argd argm)
  (and-mvn-4 4 argd argm))

(defun $b (label)
  (emit '(4 1 11) #xe 0 (logand (offset label) #x7ff)))

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
  (and-mvn-4 14 argd argm))

(defun $bl (label)
  (list
   (emit '(4 1 11) #xf 0 (logand (ash (offset label) -11) #x7ff))
   (emit '(4 1 11) #xf 1 (logand (offset label) #x7ff))))

(defun $blx (argm)
  (bx-blx 1 argm))

(defun $bx (argm)
  (bx-blx 0 argm))

(defun $cmn (argd argm)
  (and-mvn-4 11 argd argm))

(defun $cmp (argd argm)
  (cond
   ((numberp argm)
    (mov-sub-2-3 2 1 argd argm))
   (t
    (and-mvn-4 10 argd argm))))
    
(defun $eor (argd argm)
  (and-mvn-4 1 argd argm))

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
    (and-mvn-4 2 argd argm))))

(defun $lsr (argd argm &optional arg2)
  (cond
   ((numberp arg2)
    (lsl-lsr-0 1 arg2 argm argd))
   ((numberp argm)
    (lsl-lsr-0 1 argm argd argd))
   (t
    (and-mvn-4 3 argd argm))))

(defun $mov (argd argm)
  (cond
   ((numberp argm)
    (mov-sub-2-3 2 0 argd argm))
   ((or (>= (regno argd) 8) (>= (regno argm) 8))
    (add-mov-4 1 argd argm))
   (t ; Synonym of LSLS Rd, Rm, #0
    (lsl-lsr-0 0 0 argm argd))))

(defun $mul (argd argm)
  (and-mvn-4 13 argd argm))

(defun $mvn (argd argm)
  (and-mvn-4 15 argd argm))

(defun $neg (argd argm)
  (and-mvn-4 9 argd argm))

(defun $nop () ; mov r8,r8
  (add-mov-4 1 'r8 'r8))

(defun $orr (argd argm)
  (and-mvn-4 12 argd argm))

(defun $push (lst)
  (push-pop 0 lst))

(defun $pop (lst)
  (push-pop 1 lst))

(defun $rev (argd argm)
  (rev-revsh-11 0 argd argm))

(defun $rev16 (argd argm)
  (rev-revsh-11 1 argd argm))

(defun $revsh (argd argm)
  (rev-revsh-11 2 argd argm))

(defun $ror (argd argm)
  (and-mvn-4 7 argd argm))

(defun $sbc (argd argm)
  (and-mvn-4 6 argd argm))

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
  (sxth-uxtb-11 1 argd argm))

(defun $sxth (argd argm)
  (sxth-uxtb-11 0 argd argm))

(defun $tst (argd argm)
  (and-mvn-4 8 argd argm))

(defun $uxtb (argd argm)
  (sxth-uxtb-11 3 argd argm))

(defun $uxth (argd argm)
  (sxth-uxtb-11 2 argd argm))