;; 4.64
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)

まずoutranked-byの?staff-personにBitdiddle Benが束縛される．
次に(supervisor ?staff-person ?boss)でBitdiddle Benの上司が?bossに束縛される．これを仮にAとする．
そしてoutranked-byが?bossがAとして?middle-managerを探す．※
ここから二周目．
supervisor行で?bossをAとして部下Bが?staff-personに束縛される．
andのoutranked-byで?bossをAとして?middle-managerを探す．
※印をつけたところ同じところを探し始めているのでここで無限ループに陥る．
正しいoutranked-byは以下の通り．

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;; 4.65
(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

wheelはまず?personにデータベースの先頭から人を束縛して，and以下を満たすかを試していく．
なので
Ben -> Oliver -> X
alyssa -> Ben -> Oliver
Fect -> Ben -> Oliver
Lem -> Ben -> Oliver
Louis -> Alyssa Ben
Oliver -> X
Eben -> Oliver -> X
Robert -> Eben -> Oliver
Dewitt -> Oliver -> X
となり，Wawrbucks Oliverが４回出力される．

;; 4.66
重複しているのでただアキュムレートして計算するだけではダメだとわかった．
重複を削除すればよい．

;; 4.67
フレームに質問の履歴をつけていく．
入力ストリームと出力ストリームの間で同じ質問(4.64でいう(outranked-by ?staff-person Boss)のような)があれば
ループしていると判断して処理を中止するようにする．

;; 4.68
(define (my-reverse lst)
  (let iter ((lst lst) (result '()))
    (if (null? lst)
        result
        (iter (cdr lst) (cons (car lst) result)))))

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(reverse (?x) (?x))は当然成り立つ．
(?x . ?y) ?zの関係で考える．
?zの末尾は(?x)なので
(append something (?x) ?z)
somethingは残った?yをreverseしたものなので
(reverse ?y something)
規則として書いてみると

(assert! (rule (reverse () ())))

(assert! (rule (reverse (?x . ?y) ?z)
               (and (reverse ?y ?something)
                    (append-to-form ?something (?x) ?z))))

実際にリストを入れて確かめてみる．
(reverse (1 2 3 4) ?z)
まず，appendで(append-to-form ?something (1) ?z)となる．
次の行で(reverse (2 3 4) ?something)
appendに進み(append-to-form ?something2 (2) ?something)
(reverse (3 4) ?something2)
appendにいき(append-to-form ?something3 (3) ?something2)
(reverse (4) ?something3)
ひとつ目の定義から
?something3 = (4)

(append-to-form ?something3 (3) ?something2)なので
?something2 = (4 3)

(append-to-form ?something2 (2) ?something)なので
?something = (4 3 2)

(append-to-form ?something (1) ?z)なので
?z = (4 3 2 1)

これでうまくいくはず．

次は逆を考えてみる．
(reverse (?x . ?y) (1 2 3 4))
(and (append-to-form ?something ?x ?z))
候補は(append (1 2 3 4) '())(append (1 2 3) (4)) (append (1 2) (3 4)) (append (1) (2 3 4))(append-to-form () (1 2 3 4))
ここで?x = ()とすると
?somethingは(1 2 3 4)
(reverse ?y (1 2 3 4))
ここで?z = (1 2 3 4)と同じになるので無限ループになる．
他のルートから進んでもかならずこのパターンもチェックするので無限ループは避けられない．

;; 4.69
(rule (greatson-end ?x)
      (append-to-form ?u (grandson) ?x))

(rule ((grandson) ?x)
      (grandson ?x))
(rule ((great . ?rel) ?x ?y)
      (and (greatson-end ?rel)
           (son-of ?x ?z)
           (?rel ?z ?y)))
