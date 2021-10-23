-------------------------------- MODULE wire --------------------------------

EXTENDS Integers

\* 1行コメント
(* コメントブロック *)
\* [File] -> [Translate PlusCal Algorithm]
\* [TLC Model Checker] -> [New Model]
\* TLCは状態空間全体を徹底的に検査している
\* ⌘+t: Translate PlusCal Algorithm

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
    \* people = {"alice", "bob"},
    \* 関数は辞書やマッピング(写像)に近いもの
    \* acc関数
    \* = {"alice": 5, "bob": 5}
    \* acc = [p \in people |-> 5],
    \* double = [x \in 1..10 |-> 2*x],
    \* sender = "alice",
    \* receiver = "bob",
    \* 残高が3のとき
    \* amount = 3;
    \* 残高が1..6の状態をとるとき
    \* amount \in 1..6
    people = {"alice", "bob"},
    acc = [p \in people |-> 5],

\* 不変条件
define 
    (* 集合people内のすべてpにおいて、それらの口座の残高が0以上でないといけない *)
    NoOverdrafts == \A p \in people: acc[p] >= 0
    \* 口座の最終的残高は開始時点の残高と同じである
    \* 時相特性(temporal property)
    \* 演算子
    \*   []<> 最終的には常に
    EventuallyConsistent == <>[](acc["alice"] + acc["bob"] = 10)
    
end define;

\* プロセス
(*
  複数プロセスを定義して同様の動作をした時にどうなるかを調べれる
  例:
  複数の電信振替を同時に行ったときにどうなるか
  プロセスは独自のコードと独自のローカル変数を持てる
  PlusCalの機能?
  状態とは値を変数に割り当てること
*)

process Wire \in 1..2
    variables
        sender = "alice",
        receiver = "bob",
        amount \in 1..acc[sender];
begin
    \* ラベル
    CheckAndWithdraw:
        if amount <= acc[sender] then
                acc[sender] := acc[sender] - amount;
            \* 残高の預入れ
            Deposit:
                acc[receiver] := acc[receiver] + amount;
        end if

\* process は end processで囲む
end process;

end algorithm;*)
\* BEGIN TRANSLATION (chksum(pcal) = "93f22a7a" /\ chksum(tla) = "9b2bfebc")
VARIABLES people, acc, pc

(* define statement *)
NoOverdrafts == \A p \in people: acc[p] >= 0




EventuallyConsistent == <>[](acc["alice"] + acc["bob"] = 10)

VARIABLES sender, receiver, amount

vars == << people, acc, pc, sender, receiver, amount >>

ProcSet == (1..2)

Init == (* Global variables *)
        /\ people = {"alice", "bob"}
        /\ acc = [p \in people |-> 5]
        (* Process Wire *)
        /\ sender = [self \in 1..2 |-> "alice"]
        /\ receiver = [self \in 1..2 |-> "bob"]
        /\ amount \in [1..2 -> 1..acc[sender[CHOOSE self \in  1..2 : TRUE]]]
        /\ pc = [self \in ProcSet |-> "CheckAndWithdraw"]

CheckAndWithdraw(self) == /\ pc[self] = "CheckAndWithdraw"
                          /\ IF amount[self] <= acc[sender[self]]
                                THEN /\ acc' = [acc EXCEPT ![sender[self]] = acc[sender[self]] - amount[self]]
                                     /\ pc' = [pc EXCEPT ![self] = "Deposit"]
                                ELSE /\ pc' = [pc EXCEPT ![self] = "Done"]
                                     /\ acc' = acc
                          /\ UNCHANGED << people, sender, receiver, amount >>

Deposit(self) == /\ pc[self] = "Deposit"
                 /\ acc' = [acc EXCEPT ![receiver[self]] = acc[receiver[self]] + amount[self]]
                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED << people, sender, receiver, amount >>

Wire(self) == CheckAndWithdraw(self) \/ Deposit(self)

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == (\E self \in 1..2: Wire(self))
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 

=============================================================================
\* Modification History
\* Last modified Sat Oct 23 17:32:18 JST 2021 by iMac
\* Created Wed Oct 20 01:50:18 JST 2021 by iMac
