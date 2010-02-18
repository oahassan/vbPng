Option Strict On
Option Explicit On

Imports System.IO

Public Class PngImage
    Private _Palette As PngPalette = Nothing
    Private _Header As PngHeader = Nothing
    Private _ScanLines As List(Of PngScanline) = Nothing
    Private _UnhandledChunks As New List(Of PngChunk)
    Private _ApplyTrasnparencyIndicator As Boolean = False
    Private _TransparentColor As PngColor = Nothing

    Public ReadOnly Property Width() As Long
        Get
            Return Scanlines(0).Pixels.Count
        End Get
    End Property

    Public ReadOnly Property Height() As Long
        Get
            Return _ScanLines.Count
        End Get
    End Property

    Public ReadOnly Property BitDepth() As Integer
        Get
            Return _Header.BitDepth
        End Get
    End Property

    Public ReadOnly Property ColorType() As Integer
        Get
            Return _Header.ColorType
        End Get
    End Property

    Public ReadOnly Property InterlaceMethod() As Integer
        Get
            Return _Header.InterlaceMethod
        End Get
    End Property

    Public ReadOnly Property FilterMethod() As Integer
        Get
            Return _Header.FilterMethod
        End Get
    End Property

    Public ReadOnly Property CompressionMethod() As Integer
        Get
            Return _Header.CompressionMethod
        End Get
    End Property

    Public ReadOnly Property Palette() As PngPalette
        Get
            Return _Palette
        End Get
    End Property

    Public ReadOnly Property Scanlines() As PngScanline()
        Get
            Return _ScanLines.ToArray
        End Get
    End Property

    Public Class colorTypes
        Public Const GREYSCALE As Integer = 0
        Public Const TRUECOLOR As Integer = 2
        Public Const INDEXED As Integer = 3
        Public Const GREYSCALE_ALPHA As Integer = 4
        Public Const TRUECOLOR_ALPHA As Integer = 6
    End Class

    Public Sub New()

    End Sub

    Public Sub New( _
        ByVal header As PngHeader, _
        ByVal palette As PngPalette, _
        ByVal scanlines As List(Of PngScanline) _
    )
        _Header = header
        _Palette = palette
        _ScanLines = scanlines
    End Sub

    Public Sub New( _
        ByVal chunks As List(Of PngChunk) _
    )
        For Each chunk As PngChunk In chunks
            Select Case chunk.Name
                Case PngChunk.ChunkNames.PLTE
                    _Palette = New PngPalette(chunk)

                Case PngChunk.ChunkNames.IHDR
                    _Header = New PngHeader(chunk)

                Case PngChunk.ChunkNames.IDAT
                    Dim uncompressedData = zStreamReader.ReadStream(chunk.Data)

                    uncompressedData.Seek(0, SeekOrigin.Begin)
                    'PngLibUtil.writeBytesToFile(uncompressedData, "C:\Users\Wale\Documents\Spriting\Sprite.txt")

                    _ScanLines = readImageData(uncompressedData, _
                                               _Header.Width, _
                                               _Header.Height, _
                                               _Header.BitDepth, _
                                               _Header.ColorType)
                    chunk.Data.Close()
                Case PngChunk.ChunkNames.TRNS
                    processTransparency(chunk)

                Case Else
                    _UnhandledChunks.Add(chunk)
            End Select
        Next
    End Sub

    Private Function readImageData( _
        ByVal dataStream As MemoryStream, _
        ByVal width As Long, _
        ByVal height As Long, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    ) As List(Of PngScanline)
        Dim scanLines As New List(Of PngScanline)

        For lineCount As Long = 1 To height
            Dim filterBytes(0) As Byte
            dataStream.Read(filterBytes, 0, 1)
            Dim pixels As New List(Of PngPixel)

            For pixelIndex = 1 To width
                pixels.Add(readPixel(dataStream, bitDepth, colorType))
            Next

            scanLines.Add(New PngScanline(filterBytes, pixels))

            If scanLines.Count = 1 Then
                scanLines(0).removeFilter(Nothing)
            Else
                scanLines(scanLines.Count - 1).removeFilter(scanLines(scanLines.Count - 2))
            End If
        Next

        dataStream.Close()

        Return scanLines
    End Function

    Private Function readPixel( _
        ByVal dataStream As MemoryStream, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    ) As PngPixel
        Dim pixel As PngPixel = Nothing

        Select Case colorType
            Case colorTypes.GREYSCALE
                Dim colorBytes(0) As Byte

                dataStream.Read(colorBytes, 0, 1)

                Dim color As New PngColor(colorBytes, Nothing)
                pixel = New PngPixel(color)
            Case colorTypes.TRUECOLOR
                Dim colorByteCount As Integer = CInt((3 * bitDepth) / 8)
                Dim colorBytes(colorByteCount - 1) As Byte

                dataStream.Read(colorBytes, 0, colorByteCount)

                Dim color As New PngColor(colorBytes, Nothing)
                pixel = New PngPixel(color)
            Case colorTypes.INDEXED
                Dim paletteIndex As Integer = dataStream.ReadByte
                pixel = New PngPixel(paletteIndex)
            Case colorTypes.GREYSCALE_ALPHA
                Dim sampleByteCount As Integer = CInt(bitDepth / 8)
                Dim colorByteCount As Integer = sampleByteCount
                Dim colorBytes(colorByteCount - 1) As Byte

                dataStream.Read(colorBytes, 0, colorByteCount)

                Dim alphaBytes(sampleByteCount - 1) As Byte
                dataStream.Read(alphaBytes, 0, sampleByteCount)

                Dim color As New PngColor(colorBytes, alphaBytes)
                pixel = New PngPixel(color)
            Case colorTypes.TRUECOLOR_ALPHA
                Dim sampleByteCount As Integer = CInt(bitDepth / 8)
                Dim colorByteCount As Integer = 3 * sampleByteCount
                Dim colorBytes(colorByteCount - 1) As Byte

                dataStream.Read(colorBytes, 0, colorByteCount)

                Dim alphaBytes(sampleByteCount - 1) As Byte
                dataStream.Read(alphaBytes, 0, sampleByteCount)

                Dim color As New PngColor(colorBytes, alphaBytes)
                pixel = New PngPixel(color)
            Case Else
                Throw New ArgumentOutOfRangeException("bitDepth")
        End Select

        Return pixel
    End Function

    Private Sub processTransparency(ByVal chunk As PngChunk)
        Select Case _Header.ColorType
            Case 0
                Me._ApplyTrasnparencyIndicator = True

                If Me.BitDepth = 8 Then
                    Dim transparenecyBuffer(2) As Byte
                    Dim nbrRead As Integer = chunk.Data.Read(transparenecyBuffer, 0, 2)
                    Dim paletteIndex As Integer = 0

                    Do Until nbrRead = 0
                        _Palette.setEntryAlpha(paletteIndex, transparenecyBuffer)
                        nbrRead = chunk.Data.Read(transparenecyBuffer, 0, 2)
                        paletteIndex += 1
                    Loop
                ElseIf Me.BitDepth = 16 Then

                End If
            Case 2


            Case 3
                Dim transparenecyBuffer(0) As Byte
                Dim nbrRead As Integer = chunk.Data.Read(transparenecyBuffer, 0, 1)
                Dim paletteIndex As Integer = 0
                Do Until nbrRead = 0
                    _Palette.setEntryAlpha(paletteIndex, transparenecyBuffer)
                    transparenecyBuffer = New Byte(0) {}
                    nbrRead = chunk.Data.Read(transparenecyBuffer, 0, 1)
                    paletteIndex += 1
                Loop
            Case Else
                Throw New ArgumentOutOfRangeException("colorType")
        End Select

        chunk.Data.Close()
    End Sub

    Public Sub SetHeader(ByVal header As PngHeader)
        _Header = header
    End Sub
End Class
