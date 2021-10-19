-------------------------------- MODULE wire --------------------------------

EXTENDS Integers

\* 1行コメント
(* コメントブロック *)

(*--algorithm
variables
    \* people集合
    people = {"alice", "bob"},
    \* 関数は辞書やマッピング(写像)に近いもの
    \* acc関数
    \* = {"alice": 5, "bob": 5}
    acc = [p \in people |-> 5];
    double = [x \in 1..10 |-> 2*x]
    sender = "alice"
    receiver = "bob"
    amount = 3

\* 不変条件
define 
    NoOverdrafts == \A p \in people: acc[p] >= 0
end define;

begin
    skip;
end algorithm;*)

=============================================================================
\* Modification History
\* Last modified Wed Oct 20 01:57:52 JST 2021 by iMac
\* Created Wed Oct 20 01:50:18 JST 2021 by iMac
