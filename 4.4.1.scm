;; 4.55
;; a Ben Bitiddleに監督されているすべての人々
(supervisor ?x (Bitdiddle Ben))

;; b 経理部門のすべての人々の名前と担当
(job ?x (accounting . ?type))

;; c Slumervilleに住む人すべての名前と住所
(address ?x (Slumerville . ?y))

;; 4.56
;; 合成質問を形成する
;; a Ben Bitdiddleが監督している人すべての名前とその住所
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y))

;; b Ben Bitdiddleより給料が少ない人と，Ben Bitdiddleの給料
(and (salary (Bitdiddle Ben) ?Ben-amount)
     (salary ?person ?amount)
     (lisp-value > ?Ben-amount ?amount))

;; c 計算機部門にいない人が監督している人すべてと，その監督者の名前と担当．
(and (job ?person ?type)
     (and (not (job ?person (computer . ?rest)))
          (supervisor ?person ?supervisor)))


(rule (lives-near? ?person ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;; 4.57
;; jiroの仕事をtaroができるかどうか
(rule (replacible ?person1 ?person2)
      (and (or (and (job ?person2 ?job2)
                    (job ?person1 ?job2)) ;person2とperosn1の仕事が同じ
               (and (job ?person1 ?job1)
                    (can-do-job ?job1 ?job2))) ;person1はperson2の仕事job2もできる
           (not (same ?person1 ?person2))))

;; a Cy D. Fectに代われる人すべて
(replacible ?person (Fect Cy D))

;; b
(and (salary ?person ?salary)
     (replacible ?person ?somebody)
     (salary ?somebody ?somebody-salary)
     (lisp-value > ?somebody-salary ?salary))

;; 4.58
(rule (bishot ?person)
      ())
