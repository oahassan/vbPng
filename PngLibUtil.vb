Option Strict On
Option Explicit On

Friend Class PngLibUtil
#Region "Debugging Functions"
    Friend Shared Sub writePngImgToFile( _
            ByVal filePath As String, _
            ByVal png As PngImage _
        )
        Dim writer As New IO.StreamWriter(filePath)

        For Each scanline As PngScanline In png.Scanlines
            writer.Write(New Byte() {CByte(scanline.Filter)})

            writer.Write(Space(1))

            For Each pixel As PngPixel In scanline.Pixels
                If pixel.Color Is Nothing Then
                    writer.Write(CByte(pixel.PaletteIndex))
                Else
                    For Each sample As Byte In pixel.Color.Samples
                        writer.Write(sample)
                    Next
                End If
                writer.Write(Space(1))
            Next
            writer.WriteLine()
        Next

        writer.Close()
    End Sub

    Public Shared Sub writeBytesToConsole(ByVal stream As IO.Stream)
        Dim position As Long = stream.Position

        stream.Seek(0, IO.SeekOrigin.Begin)

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

    Public Shared Sub writeBytesToFile(ByVal stream As IO.Stream, ByVal filePath As String, ByVal scanlineByteLength As Integer)
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

                outputFile.Write(Space(1))

                If writeCount Mod (scanlineByteLength) = 0 Then
                    outputFile.WriteLine()
                End If
            Next

            numRead = stream.Read(buffer, 0, buffer.Length)
        Loop

        'Return stream to original position
        stream.Seek(position, IO.SeekOrigin.Begin)
        outputFile.Close()
    End Sub

    Public Shared Sub writeScanlineBytesToFile( _
        ByVal scanlines As List(Of List(Of Byte())), _
        ByVal filePath As String, _
        ByVal scanlineByteLength As Integer)
        Dim outputFile As New IO.StreamWriter(filePath, False)

        For Each scanline As List(Of Byte()) In scanlines
            For Each bytes As Byte() In scanline
                For Each lineByte As Byte In bytes
                    If lineByte < &H10 Then
                        outputFile.Write("0" & String.Format("{00:x}", lineByte))
                    Else
                        outputFile.Write(String.Format("{00:x}", lineByte))
                    End If
                    outputFile.Write(Space(1))
                Next
            Next
            outputFile.WriteLine()
        Next

        outputFile.Close()
    End Sub

    Public Shared Function countNonOpaqueEntries(ByVal palette As PngPalette) As Integer
        Dim count As Integer = 0

        For Each entry As PngColor In palette.Entries
            If entry.Alpha Is Nothing Then
                'nothing
            Else
                If entry.Alpha(0) <> 255 Then
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
                    If PngColor.Compare(palette.Entries(entryIndex), palette.Entries(nextEntryIndex)) Then
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
            If PngColor.Compare(color, testColor) Then
                indicator = True
                Exit For
            End If
        Next

        Return indicator
    End Function
#End Region


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

    Friend Shared Function byteArrayToLong(ByVal bytes As Byte()) As UInteger
        Dim rtnVal As UInteger = 0
        Dim binaryPlace As Long = CLng(Math.Pow(2, 8 * (bytes.Length - 1)))

        For idx As Integer = 0 To bytes.Length - 1
            rtnVal += CUInt(binaryPlace * bytes(idx))
            binaryPlace >>= 8
        Next

        Return rtnVal
    End Function

#Region "Filtering"
    Public Shared Function ApplyFilter( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()) _
    ) As List(Of Byte())
        Dim filteredBytes As New List(Of Byte())

        'Filter Type
        Select Case scanlineBytes(0)(0)
            Case 0 'no filter
                filteredBytes.AddRange(scanlineBytes.GetRange(1, scanlineBytes.Count - 1))
            Case 1 'Sub
                filteredBytes = applySubFilter(scanlineBytes)
            Case 2 'Up
                filteredBytes = applyUpFilter(scanlineBytes, previousScanlineBytes)
            Case 3 'Average
                filteredBytes = applyAverageFilter(scanlineBytes, previousScanlineBytes)
            Case 4 'Paeth
                filteredBytes = applyPaethFilter(scanlineBytes, previousScanlineBytes)
            Case Else
                Throw New ArgumentOutOfRangeException("Filter")
        End Select

        Return filteredBytes
    End Function

    Private Shared Function applySubFilter(ByVal scanlineBytes As List(Of Byte())) As List(Of Byte())
        Dim filteredBytes As New List(Of Byte())
        filteredBytes.Add(scanlineBytes(1))

        'First pixel is unmodified so start at second pixel
        For pixelIndex As Integer = 2 To scanlineBytes.Count - 1

            Dim previousPixelBytes As Byte() = scanlineBytes(pixelIndex - 1)
            Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)
            Dim bytes As New List(Of Byte)

            For byteIndex = 0 To previousPixelBytes.Length - 1

                bytes.Add(subtractMod256(currentPixelBytes(byteIndex), _
                                         previousPixelBytes(byteIndex)))
            Next

            filteredBytes.Add(bytes.ToArray)
        Next

        Return filteredBytes
    End Function

    Private Shared Function applyUpFilter( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()) _
    ) As List(Of Byte())
        Dim filteredBytes As New List(Of Byte())

        If previousScanlineBytes Is Nothing Then
            filteredBytes.AddRange(scanlineBytes.GetRange(1, scanlineBytes.Count - 1))
        Else
            For pixelIndex As Integer = 1 To scanlineBytes.Count - 1

                Dim upPixelBytes As Byte() = previousScanlineBytes(pixelIndex)
                Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)
                Dim bytes As New List(Of Byte)

                For byteIndex = 0 To upPixelBytes.Length - 1
                    bytes.Add(subtractMod256(currentPixelBytes(byteIndex), _
                        upPixelBytes(byteIndex)))
                Next

                filteredBytes.Add(bytes.ToArray)
            Next
        End If

        Return filteredBytes
    End Function

    Private Shared Function applyAverageFilter( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()) _
    ) As List(Of Byte())
        Dim filteredBytes As New List(Of Byte())

        If previousScanlineBytes Is Nothing Then
            filteredBytes.Add(scanlineBytes(1))

            'First pixel is unmodified so start at second pixel
            For pixelIndex As Integer = 2 To scanlineBytes.Count - 1

                Dim previousPixelBytes As Byte() = scanlineBytes(pixelIndex - 1)
                Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)
                Dim bytes As New List(Of Byte)

                For byteIndex = 0 To previousPixelBytes.Length - 1
                    bytes.Add(subtractMod256(currentPixelBytes(byteIndex), _
                        floorOfAverage(previousPixelBytes(byteIndex), 0)))
                Next

                filteredBytes.Add(bytes.ToArray)
            Next
        Else
            For pixelIndex As Integer = 1 To scanlineBytes.Count - 1
                Dim previousPixelBytes As Byte() = Nothing

                If pixelIndex > 1 Then
                    previousPixelBytes = scanlineBytes(pixelIndex - 1)
                End If

                Dim upPixelBytes As Byte() = previousScanlineBytes(pixelIndex)
                Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)
                Dim bytes As New List(Of Byte)

                For byteIndex = 0 To upPixelBytes.Length - 1
                    Dim filterFactor As Byte = 0

                    If previousPixelBytes Is Nothing Then
                        filterFactor = floorOfAverage(upPixelBytes(byteIndex), 0)
                    Else
                        filterFactor = floorOfAverage(upPixelBytes(byteIndex), _
                                                      previousPixelBytes(byteIndex))
                    End If

                    bytes.Add(subtractMod256(currentPixelBytes(byteIndex), _
                        filterFactor))
                Next

                filteredBytes.Add(bytes.ToArray)
            Next
        End If

        Return filteredBytes
    End Function

    Private Shared Function applyPaethFilter( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()) _
    ) As List(Of Byte())
        Dim filteredBytes As New List(Of Byte())

        For pixelIndex As Integer = 1 To scanlineBytes.Count - 1
            Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)
            Dim bytes As New List(Of Byte)
            For byteIndex As Integer = 0 To currentPixelBytes.Length - 1
                bytes.Add(subtractMod256(currentPixelBytes(byteIndex), _
                     getPredictedValue(scanlineBytes, _
                                       previousScanlineBytes, _
                                       byteIndex, _
                                       pixelIndex)))
            Next

            filteredBytes.Add(bytes.ToArray)
        Next

        Return filteredBytes
    End Function

    Private Shared Function subtractMod256(ByVal frstVal As Byte, ByVal secVal As Byte) As Byte
        Dim rtnVal As Byte = 0

        Try
            If CInt(frstVal) - CInt(secVal) < 0 Then
                rtnVal = CByte(256 - Math.Abs(CInt(frstVal) - CInt(secVal)))
            Else
                rtnVal = frstVal - secVal
            End If

        Catch ex As Exception
            Dim x As Integer = 1
        End Try

        Return rtnVal
    End Function

    Public Shared Sub removeFilter( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()) _
    )
        Select Case scanlineBytes(0)(0)
            Case 0 'no filter
                'do nothing
            Case 1 'Sub
                remSubFilter(scanlineBytes)
            Case 2 'Up
                removeUpFilter(scanlineBytes, previousScanlineBytes)
            Case 3 'Average
                removeAverageFilter(scanlineBytes, previousScanlineBytes)
            Case 4 'Paeth
                removePaethFilter(scanlineBytes, previousScanlineBytes)
            Case Else
                Throw New ArgumentOutOfRangeException("Filter")
        End Select
    End Sub

    Private Shared Sub remSubFilter(ByVal scanlineBytes As List(Of Byte()))
        'First pixel is unmodified so start at second pixel.  ScanlineBytes includes the
        'filter byte so start the loop at 2.
        For pixelIndex As Integer = 2 To scanlineBytes.Count - 1

            Dim previousPixelBytes As Byte() = scanlineBytes(pixelIndex - 1)
            Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)

            For byteIndex = 0 To previousPixelBytes.Length - 1
                currentPixelBytes(byteIndex) = _
                    CByte((CInt(currentPixelBytes(byteIndex)) + _
                    CInt(previousPixelBytes(byteIndex))) Mod 256)
            Next
        Next
    End Sub

    Private Shared Sub removeUpFilter( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()) _
    )
        If previousScanlineBytes Is Nothing Then
            'nothing since this means we're at the first row and the first row is unmodified
        Else
            For pixelIndex As Integer = 1 To scanlineBytes.Count - 1

                Dim upPixelBytes As Byte() = previousScanlineBytes(pixelIndex)
                Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)

                For byteIndex = 0 To upPixelBytes.Length - 1
                    currentPixelBytes(byteIndex) = _
                        CByte(CInt((currentPixelBytes(byteIndex)) + _
                        CInt(upPixelBytes(byteIndex))) Mod 256)
                Next
            Next
        End If
    End Sub

    Private Shared Function floorOfAverage(ByVal byte1 As Byte, ByVal byte2 As Byte) As Byte
        Return CByte(Math.Floor((CInt(byte1) + CInt(byte2)) / 2))
    End Function

    Private Shared Sub removeAverageFilter( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()) _
    )
        If previousScanlineBytes Is Nothing Then
            'First pixel is unmodified so start at second pixel
            For pixelIndex As Integer = 2 To scanlineBytes.Count - 1

                Dim previousPixelBytes As Byte() = scanlineBytes(pixelIndex - 1)
                Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)

                For byteIndex = 0 To previousPixelBytes.Length - 1
                    currentPixelBytes(byteIndex) = _
                        CByte(CInt((currentPixelBytes(byteIndex)) + _
                        CInt(floorOfAverage(previousPixelBytes(byteIndex), 0))) Mod 256)
                Next
            Next
        Else
            For pixelIndex As Integer = 1 To scanlineBytes.Count - 1
                Dim previousPixelBytes As Byte() = Nothing

                If pixelIndex > 1 Then
                    previousPixelBytes = scanlineBytes(pixelIndex - 1)
                End If

                Dim upPixelBytes As Byte() = previousScanlineBytes(pixelIndex)
                Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)

                For byteIndex = 0 To upPixelBytes.Length - 1
                    Dim filterFactor As Byte = 0

                    If previousPixelBytes Is Nothing Then
                        filterFactor = floorOfAverage(upPixelBytes(byteIndex), 0)
                    Else
                        filterFactor = floorOfAverage(upPixelBytes(byteIndex), _
                                                      previousPixelBytes(byteIndex))
                    End If

                    currentPixelBytes(byteIndex) = _
                        CByte(CInt((currentPixelBytes(byteIndex)) + _
                        CInt(filterFactor)) Mod 256)
                Next
            Next
        End If
    End Sub

    Private Shared Sub removePaethFilter( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()) _
    )

        For pixelIndex As Integer = 1 To scanlineBytes.Count - 1
            Dim currentPixelBytes As Byte() = scanlineBytes(pixelIndex)

            For byteIndex As Integer = 0 To currentPixelBytes.Length - 1
                currentPixelBytes(byteIndex) = _
                    CByte(CInt((currentPixelBytes(byteIndex)) + _
                    CInt(getPredictedValue(scanlineBytes, _
                                           previousScanlineBytes, _
                                           byteIndex, _
                                           pixelIndex))) Mod 256)
            Next
        Next
    End Sub

    Private Shared Function getPredictedValue( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal previousScanlineBytes As List(Of Byte()), _
        ByVal byteIndex As Integer, _
        ByVal pixelIndex As Integer _
    ) As Byte
        Dim upPixelByte As Byte = 0

        If previousScanlineBytes Is Nothing Then
            'upByte should be 0
        Else
            upPixelByte = previousScanlineBytes(pixelIndex)(byteIndex)
        End If

        Dim diagonalPixelByte As Byte = 0

        If previousScanlineBytes Is Nothing Or pixelIndex = 1 Then
            'diagonalByte should be 0
        Else
            diagonalPixelByte = previousScanlineBytes(pixelIndex - 1)(byteIndex)
        End If

        Dim previousPixelByte As Byte = 0

        If pixelIndex = 1 Then
            'previousPixelByte should be 0
        Else
            previousPixelByte = scanlineBytes(pixelIndex - 1)(byteIndex)
        End If

        Return PaethPredictor(upPixelByte, diagonalPixelByte, previousPixelByte)
    End Function

    Private Shared Function PaethPredictor( _
        ByVal upPixelByte As Byte, _
        ByVal diagonalPixelByte As Byte, _
        ByVal previousPixelByte As Byte _
    ) As Byte
        Dim predictedValue As Byte

        Dim p As Integer = CInt(previousPixelByte) + CInt(upPixelByte) - CInt(diagonalPixelByte)
        Dim pa As Integer = Math.Abs(p - previousPixelByte)
        Dim pb As Integer = Math.Abs(p - upPixelByte)
        Dim pc As Integer = Math.Abs(p - diagonalPixelByte)

        If pa <= pb And pa <= pc Then
            predictedValue = previousPixelByte
        ElseIf pb <= pc Then
            predictedValue = upPixelByte
        Else
            predictedValue = diagonalPixelByte
        End If

        Return predictedValue
    End Function
#End Region

#Region "Adam7 Interlacing"
    Public Shared Function calcPassWidth( _
        ByVal startPixelIndex As Integer, _
        ByVal pixelIndexIncrement As Integer, _
        ByVal imageWidth As Long _
    ) As Integer
        Dim width As Integer = 0

        Dim pixelIndex As Integer = startPixelIndex

        While pixelIndex < imageWidth
            width += 1
            pixelIndex += pixelIndexIncrement
        End While

        Return width
    End Function

    Public Shared Function calcPassHeight( _
        ByVal startScanlineIndex As Integer, _
        ByVal scanlineIndexIncrement As Integer, _
        ByVal imageHeight As Long _
    ) As Integer
        Dim height As Integer = 0

        Dim scanlineIndex As Integer = startScanlineIndex

        While scanlineIndex < imageHeight
            height += 1
            scanlineIndex += scanlineIndexIncrement
        End While

        Return height
    End Function
#End Region

    
End Class
