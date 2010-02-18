Option Strict On
Option Explicit On

Public Class PngScanline
#Region "Declarations"
    Private _Filter() As Byte = Nothing
    Private _Pixels As New List(Of PngPixel)
#End Region

#Region "Constructors"
    Public Sub New(ByVal filter() As Byte, ByVal pixels As List(Of PngPixel))
        _Filter = filter
        _Pixels = pixels
    End Sub
#End Region

#Region "Public Properties"
    Public ReadOnly Property Filter() As Byte()
        Get
            Return _Filter
        End Get
    End Property

    Public ReadOnly Property Pixels() As List(Of PngPixel)
        Get
            Return _Pixels
        End Get
    End Property
#End Region

#Region "Filtering"
    Public Sub ApplyFilter(ByVal previousScanline As PngScanline)
        Select Case Filter(0)
            Case 0 'no filter
                'do nothing
            Case 1 'Sub
                applySubFilter()
            Case 2 'Up
                applyUpFilter(previousScanline)
            Case 3 'Average
                applyAverageFilter(previousScanline)
            Case 4 'Paeth
                applyPaethFilter(previousScanline)
            Case Else
                Throw New ArgumentOutOfRangeException("Filter")
        End Select
    End Sub

    ''' <summary>
    ''' Applies filter type 1 (Sub) from a scanline
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub applySubFilter()
        'First pixel is unmodified so start at second pixel
        For pixelIndex As Integer = 1 To Me.Pixels.Count - 1

            Dim previousPixel As PngPixel = Me.Pixels(pixelIndex - 1)
            Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

            For byteIndex = 0 To previousPixel.Color.ColorSamples.Length - 1

                currentPixel.Color.ColorSamples(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.ColorSamples(byteIndex)) - _
                                                        CInt(previousPixel.Color.ColorSamples(byteIndex))))
            Next

            If previousPixel.Color.AlphaSample Is Nothing Then
                'nothing
            Else
                For byteIndex = 0 To previousPixel.Color.AlphaSample.Length - 1
                    currentPixel.Color.AlphaSample(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.AlphaSample(byteIndex)) - _
                                                          CInt(previousPixel.Color.AlphaSample(byteIndex))))
                Next
            End If
        Next
    End Sub

    ''' <summary>
    ''' applies the filter type 2 (up) from a scanline
    ''' </summary>
    ''' <param name="previousScanline">
    ''' the scanline directly above this one
    ''' </param>
    ''' <remarks>
    ''' previous scanline must be have its filter removed before removing the current 
    ''' scanline's filter
    ''' </remarks>
    Private Sub applyUpFilter(ByVal previousScanline As PngScanline)
        If previousScanline Is Nothing Then
            'nothing since this means we're at the first row and the first row is unmodified
        Else
            For pixelIndex As Integer = 0 To Me.Pixels.Count - 1

                Dim upPixel As PngPixel = previousScanline.Pixels(pixelIndex)
                Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

                For byteIndex = 0 To upPixel.Color.ColorSamples.Length - 1
                    currentPixel.Color.ColorSamples(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.ColorSamples(byteIndex)) - _
                                                            CInt(upPixel.Color.ColorSamples(byteIndex))))
                Next

                If upPixel.Color.AlphaSample Is Nothing Then
                    'nothing
                Else
                    For byteIndex = 0 To upPixel.Color.AlphaSample.Length - 1
                        currentPixel.Color.AlphaSample(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.AlphaSample(byteIndex)) - _
                                                              CInt(upPixel.Color.AlphaSample(byteIndex))))
                    Next
                End If
            Next
        End If
    End Sub

    ''' <summary>
    ''' applies the filter type 2 (up) from a scanline
    ''' </summary>
    ''' <param name="previousScanline">
    ''' the scanline directly above this one
    ''' </param>
    ''' <remarks>
    ''' previous scanline must be have its filter removed before removing the current 
    ''' scanline's filter
    ''' </remarks>
    Private Sub applyAverageFilter(ByVal previousScanline As PngScanline)
        If previousScanline Is Nothing Then
            'First pixel is unmodified so start at second pixel
            For pixelIndex As Integer = 1 To Me.Pixels.Count - 1

                Dim previousPixel As PngPixel = Me.Pixels(pixelIndex - 1)
                Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

                For byteIndex = 0 To previousPixel.Color.ColorSamples.Length - 1
                    currentPixel.Color.ColorSamples(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.ColorSamples(byteIndex)) - _
                                                            CInt(floorOfAverage(previousPixel.Color.ColorSamples(byteIndex), 0))))
                Next

                If previousPixel.Color.AlphaSample Is Nothing Then
                    'nothing
                Else
                    For byteIndex = 0 To previousPixel.Color.AlphaSample.Length - 1
                        currentPixel.Color.AlphaSample(byteIndex) = CByte(Math.Abs(CInt(floorOfAverage(previousPixel.Color.AlphaSample(byteIndex), 0)) - _
                                                              CInt(currentPixel.Color.AlphaSample(byteIndex))))
                    Next
                End If
            Next
        Else
            For pixelIndex As Integer = 0 To Me.Pixels.Count - 1
                Dim previousPixel As PngPixel = Nothing

                If pixelIndex > 0 Then
                    previousPixel = Me.Pixels(pixelIndex - 1)
                End If

                Dim upPixel As PngPixel = previousScanline.Pixels(pixelIndex)
                Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

                For byteIndex = 0 To upPixel.Color.ColorSamples.Length - 1
                    Dim filterFactor As Byte = 0

                    If previousPixel Is Nothing Then
                        filterFactor = floorOfAverage(upPixel.Color.ColorSamples(byteIndex), 0)
                    Else
                        filterFactor = floorOfAverage(upPixel.Color.ColorSamples(byteIndex), _
                                                      previousPixel.Color.ColorSamples(byteIndex))
                    End If

                    currentPixel.Color.ColorSamples(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.ColorSamples(byteIndex)) - _
                                                            CInt(filterFactor)))
                Next

                If upPixel.Color.AlphaSample Is Nothing Then
                    'nothing
                Else

                    For byteIndex = 0 To upPixel.Color.AlphaSample.Length - 1
                        Dim filterFactor As Byte = 0

                        If previousPixel Is Nothing Then
                            filterFactor = floorOfAverage(upPixel.Color.AlphaSample(byteIndex), 0)
                        Else
                            filterFactor = floorOfAverage(upPixel.Color.AlphaSample(byteIndex), _
                                                          previousPixel.Color.ColorSamples(byteIndex))
                        End If

                        currentPixel.Color.AlphaSample(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.AlphaSample(byteIndex)) - _
                                                              CInt(filterFactor)))
                    Next
                End If
            Next
        End If
    End Sub

    Private Sub applyPaethFilter(ByVal previousScanline As PngScanline)

        For pixelIndex As Integer = 0 To Me.Pixels.Count - 1
            Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

            For byteIndex As Integer = 0 To currentPixel.Color.ColorSamples.Length - 1
                currentPixel.Color.ColorSamples(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.ColorSamples(byteIndex)) - _
                                                        CInt(getPredictedValue(previousScanline, byteIndex, pixelIndex, False))))
            Next

            If currentPixel.Color.AlphaSample Is Nothing Then
                'nothing
            Else
                For byteIndex As Integer = 0 To currentPixel.Color.AlphaSample.Length - 1
                    currentPixel.Color.AlphaSample(byteIndex) = CByte(Math.Abs(CInt(currentPixel.Color.AlphaSample(byteIndex)) - _
                                                          CInt(getPredictedValue(previousScanline, byteIndex, pixelIndex, True))))

                Next
            End If
        Next
    End Sub

    Public Sub removeFilter(ByVal previousScanline As PngScanline)
        Select Case Filter(0)
            Case 0 'no filter
                'do nothing
            Case 1 'Sub
                remSubFilter()
            Case 2 'Up
                removeUpFilter(previousScanline)
            Case 3 'Average
                removeAverageFilter(previousScanline)
            Case 4 'Paeth
                removePaethFilter(previousScanline)
            Case Else
                Throw New ArgumentOutOfRangeException("Filter")
        End Select
    End Sub

    ''' <summary>
    ''' Removes filter type 1 (Sub) from a scanline
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub remSubFilter()
        'First pixel is unmodified so start at second pixel
        For pixelIndex As Integer = 1 To Me.Pixels.Count - 1

            Dim previousPixel As PngPixel = Me.Pixels(pixelIndex - 1)
            Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

            For byteIndex = 0 To previousPixel.Color.ColorSamples.Length - 1
                If currentPixel.Color.ColorSamples(byteIndex) > 0 Then
                    Dim x As Integer = 1
                End If
                currentPixel.Color.ColorSamples(byteIndex) = CByte((CInt(currentPixel.Color.ColorSamples(byteIndex)) + _
                                                        CInt(previousPixel.Color.ColorSamples(byteIndex))) _
                                                        Mod 256)
            Next

            If previousPixel.Color.AlphaSample Is Nothing Then
                'nothing
            Else
                For byteIndex = 0 To previousPixel.Color.AlphaSample.Length - 1
                    currentPixel.Color.AlphaSample(byteIndex) = CByte(CInt((currentPixel.Color.AlphaSample(byteIndex)) + _
                                                          CInt(previousPixel.Color.AlphaSample(byteIndex))) _
                                                          Mod 256)
                Next
            End If
        Next
    End Sub

    ''' <summary>
    ''' removes the filter type 2 (up) from a scanline
    ''' </summary>
    ''' <param name="previousScanline">
    ''' the scanline directly above this one
    ''' </param>
    ''' <remarks>
    ''' previous scanline must be have its filter removed before removing the current 
    ''' scanline's filter
    ''' </remarks>
    Private Sub removeUpFilter(ByVal previousScanline As PngScanline)
        If previousScanline Is Nothing Then
            'nothing since this means we're at the first row and the first row is unmodified
        Else
            For pixelIndex As Integer = 0 To Me.Pixels.Count - 1

                Dim upPixel As PngPixel = previousScanline.Pixels(pixelIndex)
                Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

                For byteIndex = 0 To upPixel.Color.ColorSamples.Length - 1
                    currentPixel.Color.ColorSamples(byteIndex) = CByte(CInt((currentPixel.Color.ColorSamples(byteIndex)) + _
                                                            CInt(upPixel.Color.ColorSamples(byteIndex))) _
                                                            Mod 256)
                Next

                If upPixel.Color.AlphaSample Is Nothing Then
                    'nothing
                Else
                    For byteIndex = 0 To upPixel.Color.AlphaSample.Length - 1
                        currentPixel.Color.AlphaSample(byteIndex) = CByte(CInt((currentPixel.Color.AlphaSample(byteIndex)) + _
                                                              CInt(upPixel.Color.AlphaSample(byteIndex))) _
                                                              Mod 256)
                    Next
                End If
            Next
        End If
    End Sub

    Private Function floorOfAverage(ByVal byte1 As Byte, ByVal byte2 As Byte) As Byte
        Return CByte(Math.Floor((CInt(byte1) + CInt(byte2)) / 2))
    End Function

    ''' <summary>
    ''' removes the filter type 2 (up) from a scanline
    ''' </summary>
    ''' <param name="previousScanline">
    ''' the scanline directly above this one
    ''' </param>
    ''' <remarks>
    ''' previous scanline must be have its filter removed before removing the current 
    ''' scanline's filter
    ''' </remarks>
    Private Sub removeAverageFilter(ByVal previousScanline As PngScanline)
        If previousScanline Is Nothing Then
            'First pixel is unmodified so start at second pixel
            For pixelIndex As Integer = 1 To Me.Pixels.Count - 1

                Dim previousPixel As PngPixel = Me.Pixels(pixelIndex - 1)
                Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

                For byteIndex = 0 To previousPixel.Color.ColorSamples.Length - 1
                    currentPixel.Color.ColorSamples(byteIndex) = CByte(CInt((currentPixel.Color.ColorSamples(byteIndex)) + _
                                                            CInt(floorOfAverage(previousPixel.Color.ColorSamples(byteIndex), 0))) _
                                                            Mod 256)
                Next

                If previousPixel.Color.AlphaSample Is Nothing Then
                    'nothing
                Else
                    For byteIndex = 0 To previousPixel.Color.AlphaSample.Length - 1
                        currentPixel.Color.AlphaSample(byteIndex) = CByte(CInt((floorOfAverage(previousPixel.Color.AlphaSample(byteIndex), 0)) + _
                                                              CInt(currentPixel.Color.AlphaSample(byteIndex))) _
                                                              Mod 256)
                    Next
                End If
            Next
        Else
            For pixelIndex As Integer = 0 To Me.Pixels.Count - 1
                Dim previousPixel As PngPixel = Nothing

                If pixelIndex > 0 Then
                    previousPixel = Me.Pixels(pixelIndex - 1)
                End If

                Dim upPixel As PngPixel = previousScanline.Pixels(pixelIndex)
                Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

                For byteIndex = 0 To upPixel.Color.ColorSamples.Length - 1
                    Dim filterFactor As Byte = 0

                    If previousPixel Is Nothing Then
                        filterFactor = floorOfAverage(upPixel.Color.ColorSamples(byteIndex), 0)
                    Else
                        filterFactor = floorOfAverage(upPixel.Color.ColorSamples(byteIndex), _
                                                      previousPixel.Color.ColorSamples(byteIndex))
                    End If

                    currentPixel.Color.ColorSamples(byteIndex) = CByte(CInt((currentPixel.Color.ColorSamples(byteIndex)) + _
                                                            CInt(filterFactor)) _
                                                            Mod 256)
                Next

                If upPixel.Color.AlphaSample Is Nothing Then
                    'nothing
                Else

                    For byteIndex = 0 To upPixel.Color.AlphaSample.Length - 1
                        Dim filterFactor As Byte = 0

                        If previousPixel Is Nothing Then
                            filterFactor = floorOfAverage(upPixel.Color.AlphaSample(byteIndex), 0)
                        Else
                            filterFactor = floorOfAverage(upPixel.Color.AlphaSample(byteIndex), _
                                                          previousPixel.Color.ColorSamples(byteIndex))
                        End If

                        currentPixel.Color.AlphaSample(byteIndex) = CByte(CInt((currentPixel.Color.AlphaSample(byteIndex)) + _
                                                              CInt(filterFactor)) _
                                                              Mod 256)
                    Next
                End If
            Next
        End If
    End Sub

    Private Sub removePaethFilter(ByVal previousScanline As PngScanline)

        For pixelIndex As Integer = 0 To Me.Pixels.Count - 1
            Dim currentPixel As PngPixel = Me.Pixels(pixelIndex)

            For byteIndex As Integer = 0 To currentPixel.Color.ColorSamples.Length - 1
                currentPixel.Color.ColorSamples(byteIndex) = CByte(CInt((currentPixel.Color.ColorSamples(byteIndex)) + _
                                                        CInt(getPredictedValue(previousScanline, byteIndex, pixelIndex, False))) _
                                                        Mod 256)
            Next

            If currentPixel.Color.AlphaSample Is Nothing Then
                'nothing
            Else
                For byteIndex As Integer = 0 To currentPixel.Color.AlphaSample.Length - 1
                    currentPixel.Color.AlphaSample(byteIndex) = CByte(CInt((currentPixel.Color.AlphaSample(byteIndex)) + _
                                                          CInt(getPredictedValue(previousScanline, byteIndex, pixelIndex, True))) _
                                                          Mod 256)

                Next
            End If
        Next
    End Sub

    Private Function getPredictedValue( _
        ByVal previousScanline As PngScanline, _
        ByVal byteIndex As Integer, _
        ByVal pixelIndex As Integer, _
        ByVal alpha As Boolean _
    ) As Byte
        Dim upPixelByte As Byte = 0

        If previousScanline Is Nothing Then
            'upByte should be 0
        Else
            If alpha Then
                upPixelByte = previousScanline.Pixels(pixelIndex).Color.AlphaSample(byteIndex)
            Else
                upPixelByte = previousScanline.Pixels(pixelIndex).Color.ColorSamples(byteIndex)
            End If
        End If

        Dim diagonalPixelByte As Byte = 0

        If previousScanline Is Nothing Or pixelIndex = 0 Then
            'diagonalByte should be 0
        Else
            If alpha Then
                diagonalPixelByte = previousScanline.Pixels(pixelIndex - 1).Color.AlphaSample(byteIndex)
            Else
                diagonalPixelByte = previousScanline.Pixels(pixelIndex - 1).Color.ColorSamples(byteIndex)
            End If

        End If

        Dim previousPixelByte As Byte = 0

        If pixelIndex = 0 Then
            'previousPixelByte should be 0
        Else
            If alpha Then
                previousPixelByte = Me.Pixels(pixelIndex - 1).Color.AlphaSample(byteIndex)
            Else
                previousPixelByte = Me.Pixels(pixelIndex - 1).Color.ColorSamples(byteIndex)
            End If

        End If

        Return PaethPredictor(upPixelByte, diagonalPixelByte, previousPixelByte)
    End Function

    Private Function PaethPredictor( _
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
End Class
