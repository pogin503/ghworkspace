module sample/HelloWorld

// sig: Javaでいうクラス
// シグニチャは複数書ける
//<sigQual> sig <name> {<decl>}
// <sigQual>はアクセス修飾子
// <decl>はJavaでいうメンバ変数
one sig Display {
    console : one Console
}

// コンソールはいくつかのメッセージを持つ
one sig Console {
    message : some Message
}

// abstract: 実体を持たないシグニチャ
// 抽象シグニチャ
abstract sig Message {}

// Messageを継承した具象シグニチャ
sig HelloWorld extends Message {}

// 事実
/** 全てのメッセージはコンソールに表示される */
fact {
    all m : Message | m in Console.message
}

// 実行関数
pred show{}
// 実行する
run show
