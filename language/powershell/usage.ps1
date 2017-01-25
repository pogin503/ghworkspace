Set-PSDebug -strict

# enable debugger
Set-PSDebug -trace 2

# step
Set-PSDebug -step

# print environment value
dir env:*
$env:Path

# create current directory
mkdir ("{0:yyyyMMdd}" -f Get-Date)

# array
$arr = @()
$arr = 1,2,3

$arr_len = arr.Length

# regexp
$result = "abc" -replace "c","d"

# プロセスのリスト（System.Diagnostics.Processオブジェクトの配列）を取得し、 
# Where-Objectコマンドレットでハンドル数（handlesプロパティ）の値が500より大きいものだけを取り出し 
# Select-Objectコマンドレットで最初の5つのオブジェクトだけを切りだして表示
Get-Process | Where-Object {$_.handles -gt 500} `
    | Select-Object -first 5

# grep
# type
# sed
# find

