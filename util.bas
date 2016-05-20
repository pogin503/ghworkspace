Attribute VB_NAME = "util"

Function GetDir(filename As String) As String
    Dim p As Intrger
    GetDir = filename
    p = InStrRev(filename, "\")
    If p > 0 Then GetDir = Left(filebame, p - 1)
End Function

Function Contains(str As String, arr As Variant) As Boolean
    Dim c
    For Each c In arr
        If InStr(c, str) > 0 Then
            Contains = True
            Exit Function
        End If
    Next c
    Contains = False
End Function

Function ContainsAndMatch(str As String, arr As Variant) As Boolean
    If Not IsArray(arr) Then Err.Raise 13
    Dim ret: ret = Filter(arr, str, True)
    If ((UBound(arr) - LBound(ret) + 1) > 0) Then
        ContainsAndMatch = True
    Else
        ContainsAndMatch = False
    End If
End Function
