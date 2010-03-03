Public Class PngPalette
#Region "Declarations"
    Private _Entries As New List(Of PngColor)
    Private _TransparentEntryCount As Integer = 0

    ''' <summary>
    ''' Delegate for comparison functions
    ''' </summary>
    ''' <param name="color1">
    ''' first color to compare
    ''' </param>
    ''' <param name="color2">
    ''' second color to compare
    ''' </param>
    ''' <returns>
    ''' Boolean value indicating equivalency of the two colors
    ''' </returns>
    Public Delegate Function colorComparer(ByVal color1 As PngColor, ByVal color2 As PngColor) As Boolean
#End Region

#Region "Constructors"
    ''' <summary>
    ''' Instantiates a new PngPalette
    ''' </summary>
    Public Sub New()

    End Sub

    ''' <summary>
    ''' Instantiates a new PngPalette and populates it with the colors in the scanlines
    ''' of a png.
    ''' </summary>
    ''' <param name="scanLines">
    ''' Scanlines to create palette entries
    ''' </param>
    ''' <param name="setPixelColors">
    ''' Indicates whether to replace a PngPixel's color with its palette entry index
    ''' </param>
    ''' <remarks></remarks>
    Public Sub New( _
        ByVal scanLines As List(Of PngScanline), _
        ByVal setPixelColors As Boolean _
    )
        For lineIndex As Integer = 0 To scanLines.Count - 1
            Dim line As PngScanline = scanLines(lineIndex)

            For pixelIndex As Integer = 0 To line.Pixels.Count - 1
                Dim pixel As PngPixel = line.Pixels(pixelIndex)

                Dim colorEntry As Integer = Me.hasEntry(pixel.Color)

                If colorEntry = -1 Then
                    addEntry(pixel.Color)

                    If pixel.Color.IsOpaque = False Then
                        _TransparentEntryCount += 1
                    End If

                    If setPixelColors Then
                        pixel.ReplaceColorWithPaletteEntry(Me._Entries.Count - 1)
                    End If
                Else
                    If setPixelColors Then
                        pixel.ReplaceColorWithPaletteEntry(colorEntry)
                    End If
                End If
            Next
        Next
    End Sub

    Friend Sub New(ByVal paletteChunk As PngChunk)
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
    ''' <summary>
    ''' Adds a PngColor to the entries in this PngPalette
    ''' </summary>
    ''' <param name="color">
    ''' PngColor to add
    ''' </param>
    Public Sub addEntry(ByVal color As PngColor)
        If color Is Nothing Then
            Throw New ArgumentNullException("color")
        End If

        _Entries.Add(color)

        If color.IsOpaque = False Then
            _TransparentEntryCount += 1
        End If

    End Sub

    ''' <summary>
    ''' Returns whether a PngColor exists in this PngPalette
    ''' </summary>
    ''' <param name="color">
    ''' PngColor to search for
    ''' </param>
    ''' <param name="colorComparer">
    ''' Optionally takes in a comparer to customize what colors match
    ''' </param>
    ''' <returns>
    ''' A boolean indicating if this PngPalette contains the given PngColor
    ''' </returns>
    Public Function hasEntry( _
        ByVal color As PngColor, _
        Optional ByVal colorComparer As colorComparer = Nothing _
    ) As Integer
        If color Is Nothing Then
            Throw New ArgumentNullException("color")
        End If

        If colorComparer Is Nothing Then
            colorComparer = AddressOf PngColor.Compare
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
#End Region

#Region "Friend Functions"
    ''' <summary>
    ''' removes entries beyond the last non-opaque entry
    ''' </summary>
    ''' <remarks>
    ''' preprocessing for creating the transparency chunk
    ''' </remarks>
    Friend Sub truncateEntries()
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
        Dim paletteEntry As PngColor = Nothing
        Dim sampleBuffer(2) As Byte
        Dim bytesRead As Integer = paletteData.Read(sampleBuffer, 0, sampleBuffer.Length)

        If bytesRead = sampleBuffer.Length Then
            Dim r As Byte() = New Byte() {sampleBuffer(0)}
            Dim g As Byte() = New Byte() {sampleBuffer(1)}
            Dim b As Byte() = New Byte() {sampleBuffer(2)}

            paletteEntry = New PngColor(r, g, b, Nothing)
        End If

        Return paletteEntry
    End Function

#End Region
    End Class
