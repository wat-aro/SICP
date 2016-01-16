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
(rule (big-shot ?person)
      (and (job ?person (?division . rest))
           (supervisor ?person ?boss)
           (job ?boss (?boss-division . rest2))
           (not (same ?division ?boss-division))))

;; 4.59
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))

;; a 金曜の朝に今日ある会議をすべて質問する
(meeting ?all (Friday ?time))

;; b ある人の会議は，全社会議とその人の部門会議をすべて含む
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?division . ?rest))
               (meeting ?division ?day-and-time))))

;; c Alyssaが水曜に会議の時間を質問する
(meeting-time (Hacker Alyssa P) (Wednesday ?time))


;; 4.60
(lives-near ?person-1 ?person2)


(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

(and (lives-near ?person1 ?person2)
     (not (lives-near ?person2 ?person1)))

;; 各人にIDを割り振る．
;; 例
(id (Bitdiddle Ben) 0)

;; そしてlives-nearをidの若いほうから表示するように書き換える
(rule (lives-near? ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))
           (id ?person1 ?id1)
           (id ?person2 ?id2)
           (lisp-value < ?id1 ?id2)))

(append-to-form x y z)

(rule (append-to-form () ?y ?y))

(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

;; 4.61
;; 先頭の２つの隣接関係
(rule (?x next-to ?y in (?x ?y . ?u)))

;; リストのcdrの隣接関係
;; (1 2 3 4 5)だとvが1,zが(2 3 4 5).２行目で，zに対してもnext-toをやると読める．
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

(?x next-to ?y in (1 (2 3) 4))
;; =>
(1 next-to (2 3) in (1 (2 3) 4))
(2 3) next-to 4 in (1 (2 3) 4)

(?x next-to 1 in (2 1 3 1))
;; =>
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))


;; 4.62
;; last-pairに該当するルールを作る
(rule (last-pair (?x) (?x)))

(rule (last-pair? (?x . ?y) ?z)
      (last-pair? ?y ?z))

;; 質問
(last-pair (3) (?x))
;; ひとつ目の質問にマッチして
;; ?x=3となるので
(last-pair (3) (3))
;; と出力されるはず．


;; 質問
(last-pair (1 2 3) ?x)
;; まずふたつ目の質問にマッチする．
;; ?y=1, ?z=(2 3).二行目で(last-pair (2 3) ?x)となる．
;; 次にまた二つ目の質問にマッチ．
;; ?y=2,?z=(3).二行目で(last-pair (3) ?x)
;; ひとつ目の質問にマッチ．(last-pair (3) (3))
(last-pair (1 2 3) (3))

;; 質問
(last-pair (2 ?x) (3))
;; まず二つ目の質問にマッチする．
;; ?y=2, ?z=?x, ?x=3.
;; ２行目で(last-pair ?x (3))となる．
;; 実装がわからないのでここから想像．
;; まずひとつ目の質問を評価機は試す．
;; すると(last-pair (3) (3))になる．
;; 次に二つ目の質問を試す．
;; すると(last-pair (?x . ?y) (3))かつ(last-pair ?y (3))
;; なのでどの組み合わせを試しても成り立つ．
;; ここで無限ループになる．
;; 仮にすべての組み合わせを試して，それに対して
;; 二行目の質問もさらにすべての組み合わせに対して成り立つ．

;; 4.63
(rule (?son son-of ?dad)
      (or (son ?dad ?son)
          (and (wife ?dad ?mam)
               (son ?mam ?son))))

(rule (?grandson grandson-of ?granddad)
      (and (?parent son-of ?grandson)
           (?grandson son-of ?parent)))

;; 4.64
