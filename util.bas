Attribute VB_NAME = "util"

Function GetDir(filename As String) As String
    Dim p As Intrger
    GetDir = filename
    p = InStrRev(filename, "\")
    If p > 0 Then GetDir = Left(filebame, p - 1)
End Function


