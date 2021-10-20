-------------------------------- MODULE wire --------------------------------

EXTENDS Integers

\* 1行コメント
(* コメントブロック *)
\* [File] -> [Translate PlusCal Algorithm]
\* [TLC Model Checker] -> [New Model]
\* TLCは状態空間全体を徹底的に検査している

(*  
AliceとBobはBankgroupに口座を持っている
それぞれの講座には0ドル以上の預金がある
Bankgroupはユーザー間の送金ができるようにするために、新たに「電信振替」機能を追加したいと間変えている

要件
- 各電信振替はBankgroupにに口座を持つ2人のユーザーの間で行わなければならず、
  最低でも1 ドルを送金しなければならない。
- 電信振替が成功した場合は、送金人口座からその金額を引き落とし、受取人口座に振り込む。
- 電信振替が失敗した場合は、どちらの口座も変更しない。
- 口座の残高がマイナスになるような電信振替を行うことはできない。
- スケーラビリティを確保するために、複数の電信振替を同時に行うことができる。
*)
(*--algorithm wire
variables
    \* people集合
    people = {"alice", "bob"},
    \* 関数は辞書やマッピング(写像)に近いもの
    \* acc関数
    \* = {"alice": 5, "bob": 5}
    acc = [p \in people |-> 5],
    double = [x \in 1..10 |-> 2*x],
    sender = "alice",
    receiver = "bob",
    \* 残高が3のとき
    \* amount = 3;
    \* 残高が1..6の状態をとるとき
    amount \in 1..6

\* 不変条件
(* 集合people内のすべてpにおいて、それらの口座の残高が0以上でないといけない *)
define 
    NoOverdrafts == \A p \in people: acc[p] >= 0
end define;

begin
    \* Withdraw, Depositはラベル
    \* 残高の引き出し
    Withdraw:
        acc[sender] := acc[sender] - amount;
    \* 残高の預入れ
    Deposit:
        acc[receiver] := acc[receiver] + amount;
end algorithm;*)
\* BEGIN TRANSLATION (chksum(pcal) = "1fbf2c12" /\ chksum(tla) = "f988c613")
VARIABLES people, acc, double, sender, receiver, amount, pc

(* define statement *)
NoOverdrafts == \A p \in people: acc[p] >= 0


vars == << people, acc, double, sender, receiver, amount, pc >>

Init == (* Global variables *)
        /\ people = {"alice", "bob"}
        /\ acc = [p \in people |-> 5]
        /\ double = [x \in 1..10 |-> 2*x]
        /\ sender = "alice"
        /\ receiver = "bob"
        /\ amount = 3
        /\ pc = "Withdraw"

Withdraw == /\ pc = "Withdraw"
            /\ acc' = [acc EXCEPT ![sender] = acc[sender] - amount]
            /\ pc' = "Deposit"
            /\ UNCHANGED << people, double, sender, receiver, amount >>

Deposit == /\ pc = "Deposit"
           /\ acc' = [acc EXCEPT ![receiver] = acc[receiver] + amount]
           /\ pc' = "Done"
           /\ UNCHANGED << people, double, sender, receiver, amount >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == Withdraw \/ Deposit
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 

=============================================================================
\* Modification History
\* Last modified Thu Oct 21 02:13:03 JST 2021 by iMac
\* Created Wed Oct 20 01:50:18 JST 2021 by iMac
