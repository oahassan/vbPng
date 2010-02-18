Public Class PngPalette
#Region "Declarations"
    Private _Entries As New List(Of PngColor)
    Private _TransparentEntryCount As Integer = 0

    Public Delegate Function colorComparer(ByVal color1 As PngColor, ByVal color2 As PngColor) As Boolean
#End Region

#Region "Constructors"
    Public Sub New()

    End Sub

    Public Sub New(ByRef scanLines As List(Of PngScanline))
        For Each line As PngScanline In scanLines
            For Each pixel As PngPixel In line.Pixels
                Dim colorEntry As Integer = Me.hasEntry(pixel.Color)

                If colorEntry = -1 Then
                    addEntry(pixel.Color)

                    If pixel.Color.IsOpaque = False Then
                        _TransparentEntryCount += 1
                    End If

                    pixel.ReplaceColorWithPaletteEntry(Me._Entries.Count - 1)
                Else
                    pixel.ReplaceColorWithPaletteEntry(colorEntry)
                End If
            Next
        Next
    End Sub

    Public Sub New(ByVal paletteChunk As PngChunk)
        Dim paletteEntry As PngColor = readPaletteEntry(paletteChunk.Data)

        Do Until paletteEntry Is Nothing
            _Entries.Add(paletteEntry)
            paletteEntry = readPaletteEntry(paletteChunk.Data)
        Loop

        paletteChunk.Data.Close()
    End Sub
#End Region

#Region "Public Properties"
    Public ReadOnly Property HasNonOpaqueEntries() As Boolean
        Get
            Return _TransparentEntryCount > 0
        End Get
    End Property

    Public ReadOnly Property Entries() As PngColor()
        Get
            Return _Entries.ToArray
        End Get
    End Property
#End Region

#Region "Public Functions"
    Public Sub setEntryAlpha(ByVal index As Integer, ByVal alpha() As Byte)
        Dim opaqueIndicator As Boolean = True

        For Each sampleByte As Byte In alpha
            If sampleByte <> 255 Then
                opaqueIndicator = False
                Exit For
            End If
        Next

        If opaqueIndicator = False _
        AndAlso Entries(index).IsOpaque Then
            _TransparentEntryCount += 1
        End If

        _Entries(index).setAlpha(alpha)
    End Sub

    Public Sub addEntry(ByVal color As PngColor)
        If color Is Nothing Then
            Throw New ArgumentNullException("color")
        End If

        _Entries.Add(color)

        If color.IsOpaque = False Then
            _TransparentEntryCount += 1
        End If

    End Sub

    Public Function hasEntry( _
        ByVal color As PngColor, _
        Optional ByVal colorComparer As colorComparer = Nothing _
    ) As Integer
        If color Is Nothing Then
            Throw New ArgumentNullException("color")
        End If

        If colorComparer Is Nothing Then
            colorComparer = AddressOf PngColor.compare
        End If

        Dim entryPosition As Integer = -1

        For entryIndex As Integer = 0 To _Entries.Count - 1
            Dim entry As PngColor = _Entries(entryIndex)

            If colorComparer.Invoke(color, entry) Then
                entryPosition = entryIndex
                Exit For
            End If
        Next

        Return entryPosition
    End Function

    ''' <summary>
    ''' removes entries beyond the last non-opaque entry
    ''' </summary>
    ''' <remarks>
    ''' preprocessing for creating the transparency chunk
    ''' </remarks>
    Public Sub truncateEntries()
        Dim truncatedEntries As New List(Of PngColor)

        Dim lastTransparentEntryIndex As Integer = -1

        For entryIndex As Integer = 1 To Me._Entries.Count - 1
            Dim entry As PngColor = Me._Entries(entryIndex)

            If entry.IsOpaque = False Then
                lastTransparentEntryIndex = entryIndex
            End If
        Next

        For entryIndex As Integer = 1 To Me._Entries.Count - 1
            If entryIndex > lastTransparentEntryIndex Then
                truncatedEntries.Add(Me._Entries(entryIndex))
            End If
        Next

        For Each entry As PngColor In truncatedEntries
            _Entries.Remove(entry)
        Next
    End Sub
#End Region

#Region "Private Functions"
    Private Function readPaletteEntry(ByVal paletteData As IO.MemoryStream) As PngColor
        Dim sampleBuffer(2) As Byte
        Dim paletteEntry As PngColor = Nothing
        Dim bytesRead As Integer = paletteData.Read(sampleBuffer, 0, sampleBuffer.Length)

        If bytesRead = sampleBuffer.Length Then
            paletteEntry = New PngColor(sampleBuffer, Nothing)
        End If

        Return paletteEntry
    End Function

#End Region
    End Class
