Attribute VB_NAME = "usage"

Private Sub Main()
    Dim str as String

    Dim arr() As String
    For i = LBound(arr) to UBound(arr)
        Debug.Print arr(i)
    Next

    ' Sample Selection
    With Selection
        For i = 1 to .Rows.Count
            For j = 1 to .Coulumns.Count
                .Cells(i, j) = Replace(.Cells(i, j).Value, "!!!")
            Next
        Next
    End With

    ' Copy
    Worksheets("Sheet2").Range("A1").Value = Worksheets("Sheet1").Range("A1").Value
End Sub

Sub GetLastCellRow()
    ' 最終セルを表示
    GetLastCellRow ActiveSheet.Cells.SpecialCells(xlLastCell).Address
End Sub
