Set-PSDebug -strict

# enable debugger
Set-PSDebug -trace 2

# step
Set-PSDebug -step

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
