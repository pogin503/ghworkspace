# Indicates that Windows PowerShell returns an exception if a variable is referenced before a value is assigned to the variable.
Set-PSDebug -strict

# enable debugger
# 0 - Turn script tracing off
# 1 - Trace script lines as they are executed
# 2 - Trace script lines, variable assignments, function calls, and scripts.
Set-PSDebug -trace 2

# step
Set-PSDebug -step

 # Establishes and enforces coding rules in expressions, scripts, and script blocks.
Set-StrictMode -Version 1.0
Set-StrictMode -Version 2.0
Set-StrictMode -Version 3.0
Set-StrictMode -Version Latest

# 規則の解除
Set-StrictMode -Off

# Restricted - 実行できるスクリプトはありません。Windows PowerShell は対話型モードでのみ使用できます。
# AllSigned - 信頼できる発行元が署名したスクリプトのみを実行できます。
# RemoteSigned - ダウンロードしたスクリプトは信頼できる発行元が署名した場合にのみ実行できます。
# Unrestricted - 制限なし。すべての Windows PowerShell スクリプトを実行できます。
Set-ExecutionPolicy RemoteSigned
Get-ExecutionPolicy

# コマンドレットでエラーがあった場合、処理を中断させる
$ErrorActionPreference = "Stop"

# print environment value
dir env:*
$env:Path

# create current directory
# date +'%Y/%m/&d'
mkdir ("{0:yyyyMMdd}" -f Get-Date)

# array
$arr1 = @()
$arr2 = 1,2,3

$arr_len = arr2.Length
$arr2 += 4
$arr3 = 5,6,7
$arr4 = $arr2 + $arr3
$arr2 -contains 2

# hash table
$hash = @{hoge="HOGE"; fuga="FUGA"; piyo="PIYO"}
echo $hash.GetType().Name #=> Hashtable
echo ("hoge = " + hoge["hoge"])
echo ("fuga = " + hoge.Item["fuga"])

# hash table
echo $hash.Keys
echo $hash.Values
foreach ($key in $hash.Keys) {
    $key + ":" + $hash.[$key]
}

# Set
# @see https://msdn.microsoft.com/en-us/library/bb359438.aspx
$set = New-Object System.Collections.Generic.HashSet[int]

# more
# @see https://www.simple-talk.com/sysadmin/powershell/powershell-one-liners-collections-hashtables-arrays-and-strings/

# regexp
$result = "abc" -replace "c","d"

Get-Process | Where-Object {$_.handles -gt 500} | Select-Object -first 5

# if
<#
if (...) {

} elseif (...) {

} else {

}
#>

# while
$x = 0
while ($x -lt 5) {
    write-host $x++
}

# foreach
$items = 1..10
foreach ($item in $items) {
    write-host $item
}

# switch
$i = 3
switch ($i) {
    1 {"1"; break}
    2 {"2"; break}
    {$_ -lt 5} {"5より小さい"; break}
    default {"default句"; break}
}

# operator
$num1 = 1
$num2 = 2
$num1 -eq $num2 # == $num1は$num2と等しい
$num1 -ne $num2 # != $num1は$num2は等しくない
$num1 -lt $num2 # <  $num1は$num2より小さい
$num1 -gt $num2 # >  $num1は$num2より大きい
$num1 -le $num2 # <= $num1は$num2以下
$num1 -ge $num2 # >= $num1は$num2以上

# logical operator
$ret = -not $true
$ret = !$true
$ret = $true -and $false
$ret = $true -or  $false
$ret = $true -xor $false

# grep
gc somefile.txt | where { $_ -match “expression”}

# head
gc log.txt | select -first 10
gc -TotalCount 10 log.txt

# tail
gc log.txt | select -last 10
gc -Tail 10 log.txt # tail (since PSv3), also much faster than above option

# sed
cat somefile.txt | % { $_ -replace "expression","replace" }

# find .
ls -r
ls -Recursive

# find . -type f
ls -r -File -Name

# find . -type d
ls -r -Directory -Name

# find . -type f -name "expression"
ls -r -File -i "expression"

# touch
New-Item -ItemType file newfile

# @see http://qiita.com/opengl-8080/items/bb0f5e4f1c7ce045cc57
# @see http://stackoverflow.com/questions/9682024/how-to-do-what-head-tail-more-less-sed-do-in-powershell

# ============================
# JSON ファイルパス
$jsonFile = "C:¥Path¥to¥jsonfile.json"

# ライブラリの参照を追加
[System.Reflection.Assembly]::LoadWithPartialName ("System.Web.Extension") 

# メソッド参照用のオブジェクトを定義
$ser = New-Object System.Web.Script.Serialization.JavaScriptSerializer

# オブジェクトにデシリアライズする
$json = $ser.DeserializeObject((Get-Content $jsonFile -encoding utf8))

# JSONデータの参照
$json[‘Directory']
# ============================
