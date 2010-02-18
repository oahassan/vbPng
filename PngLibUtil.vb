Option Strict On
Option Explicit On

Public Class PngLibUtil
    Friend Shared Function compareByteArrays(ByVal array1 As Byte(), ByVal array2 As Byte()) As Boolean
        Dim indicator As Boolean = True

        If (array1 Is Nothing) Xor (array2 Is Nothing) Then
            indicator = False
        ElseIf (array1 Is Nothing) = False And (array2 Is Nothing) = False Then
            If array1.Length = array2.Length Then
                For byteIndex As Integer = 0 To array1.Length - 1
                    If array1(byteIndex) <> array2(byteIndex) Then
                        indicator = False
                        Exit For
                    End If
                Next
            End If
        End If

        Return indicator
    End Function

    Friend Shared Function byteArrayToLong(ByVal bytes As Byte()) As Long
        Dim rtnVal As Long = 0
        Dim binaryPlace As Long = CLng(Math.Pow(2, 8 * (bytes.Length - 1)))

        For idx As Integer = 0 To bytes.Length - 1
            rtnVal += binaryPlace * bytes(idx)
            binaryPlace >>= 8
        Next

        Return rtnVal
    End Function

    Public Shared Sub writeBytesToConsole(ByVal stream As IO.Stream)
        Dim position As Long = stream.Position
        Dim buffer As Byte() = New Byte(4096) {}
        Dim numRead As Integer
        numRead = stream.Read(buffer, 0, buffer.Length)

        Do While numRead <> 0
            For i = 0 To numRead - 1
                Console.Write(Convert.ToString(buffer(i), 2) + Space(1))
            Next

            numRead = stream.Read(buffer, 0, buffer.Length)
        Loop

        'Return stream to original position
        stream.Seek(position, IO.SeekOrigin.Begin)
    End Sub

    Public Shared Sub writeBytesToFile(ByVal stream As IO.Stream, ByVal filePath As String)
        Dim outputFile As New IO.StreamWriter(filePath, False)

        Dim position As Long = stream.Position
        Dim buffer As Byte() = New Byte(4096) {}
        Dim numRead As Integer
        numRead = stream.Read(buffer, 0, buffer.Length)
        Dim writeCount As Integer = 0

        Do While numRead <> 0
            For i = 0 To numRead - 1
                If buffer(i) < &H10 Then
                    outputFile.Write("0" & String.Format("{00:x}", buffer(i)))
                Else
                    outputFile.Write(String.Format("{00:x}", buffer(i)))
                End If

                writeCount += 1

                If writeCount Mod 4 = 0 Then
                    outputFile.Write(Space(1)) 'outputFile.Write(ControlChars.Tab)
                Else
                    outputFile.Write(Space(1))
                End If

                If writeCount Mod ((112 * 4) + 1) = 0 Then
                    outputFile.WriteLine()
                End If
            Next

            numRead = stream.Read(buffer, 0, buffer.Length)
        Loop

        'Return stream to original position
        stream.Seek(position, IO.SeekOrigin.Begin)
        outputFile.Close()
    End Sub

    Public Shared Function countNonOpaqueEntries(ByVal palette As PngPalette) As Integer
        Dim count As Integer = 0

        For Each entry As PngColor In palette.Entries
            If entry.AlphaSample Is Nothing Then
                'nothing
            Else
                If entry.AlphaSample(0) <> 255 Then
                    count += 1
                End If
            End If
        Next

        Return count
    End Function

    Public Shared Function countColorTypePngs(ByVal pngs As List(Of PngImage)) As Integer
        Dim counter As Integer = 0

        For Each png As PngImage In pngs
            If png.ColorType = PngImage.colorTypes.INDEXED Then
                counter += 1
            End If
        Next

        Return counter
    End Function

    Public Shared Function countNonGreyscaleEntries(ByVal palette As PngPalette) As Integer
        Dim count As Integer = 0

        For Each entry As PngColor In palette.Entries
            Dim differenceIndicator As Boolean = False

            For sampleIndex = 0 To entry.B.Length - 1
                If entry.B(sampleIndex) <> entry.R(sampleIndex) _
                Or entry.B(sampleIndex) <> entry.G(sampleIndex) Then
                    differenceIndicator = True
                    Exit For
                End If
            Next

            If differenceIndicator = True Then
                count += 1
            End If
        Next

        Return count
    End Function

    Public Shared Function countRedundantEntries(ByVal palette As PngPalette) As Integer
        Dim count As Integer = 0

        Dim countedColors As New List(Of PngColor)

        For entryIndex As Integer = 0 To palette.Entries.Length - 1
            If listContainsColor(countedColors, palette.Entries(entryIndex)) Then
                'nothing
            Else
                For nextEntryIndex As Integer = entryIndex + 1 To palette.Entries.Length - 1
                    If PngColor.compare(palette.Entries(entryIndex), palette.Entries(nextEntryIndex)) Then
                        count += 1
                    End If
                Next
            End If

            countedColors.Add(palette.Entries(entryIndex))
        Next

        Return count
    End Function

    Private Shared Function listContainsColor(ByVal list As List(Of PngColor), ByVal testColor As PngColor) As Boolean
        Dim indicator As Boolean = False

        For Each color As PngColor In list
            If PngColor.compare(color, testColor) Then
                indicator = True
                Exit For
            End If
        Next

        Return indicator
    End Function
End Class
