Attribute VB_NAME = "util"

Option Explicit

' Dim fso, oD
' Set fso = CreateObject("Scripting.FileSystemObject")
' Set oD = fso.GetDrive(fso.GetDriveName(WScript.ScriptFullName))

Public Function GetDir(filename As String) As String
    Dim p As Intrger
    GetDir = filename
    p = InStrRev(filename, "\")
    If p > 0 Then GetDir = Left(filebame, p - 1)
End Function

Public Function WorksheetExists(wkbook as Workbook, SheetName As String) As Boolean
    Dim ws As WorkSheet
    For Each ws In wkBook.WorkSheets
        If ws.Name = SheetName Then Goto escape
    Next
    WookbookExists = False
    Exit Function
escape:
    WorkbookExists = True
End Function

Public Function Contains(str As String, arr As Variant) As Boolean
    Dim c
    For Each c In arr
        If InStr(c, str) > 0 Then
            Contains = True
            Exit Function
        End If
    Next c
    Contains = False
End Function

Public Function ContainsAndMatch(str As String, arr As Variant) As Boolean
    If Not IsArray(arr) Then Err.Raise 13
    Dim ret: ret = Filter(arr, str, True)
    If ((UBound(arr) - LBound(ret) + 1) > 0) Then
        ContainsAndMatch = True
    Else
        ContainsAndMatch = False
    End If
End Function

' @param Text As String
' @see http://stackoverflow.com/questions/14219455/excel-vba-code-to-copy-a-specific-string-to-clipboard?answertab=active#tab-top
Public Sub CopyText(Text As String)
    'VBA Macro using late binding to copy text to clipboard.
    'By Justin Kay, 8/15/2014
    Dim MSForms_DataObject As Object
    Set MSForms_DataObject = CreateObject("new:{1C3B4210-F441-11CE-B9EA-00AA006B1A69}")
    MSForms_DataObject.SetText Text
    MSForms_DataObject.PutInClipboard
    Set MSForms_DataObject = Nothing
End Sub

' =====================
' require Ariawase

' @param var As Variant
' @see https://github.com/vbaidiot/Ariawase
Sub p(var As Variant)
    Debug.Print Dump(var)
End Sub
