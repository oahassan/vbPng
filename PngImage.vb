Option Strict On
Option Explicit On

Imports System.IO

''' <summary>
''' Encapsulates a png image
''' </summary>
Public Class PngImage

#Region "Declarations"
    ''' <summary>
    ''' Enumeration of png color types
    ''' </summary>
    Public Enum colorTypes
        GREYSCALE = 0
        TRUECOLOR = 2
        INDEXED = 3
        GREYSCALE_ALPHA = 4
        TRUECOLOR_ALPHA = 6
    End Enum

    ''' <summary>
    ''' Enumeration of png interlacing methods
    ''' </summary>
    Public Enum InterlacingMethods
        None = 0
        Adam7 = 1
    End Enum

    ''' <summary>
    ''' Enumeration of png filtering methods
    ''' </summary>
    Public Enum FilterMethods
        Dflt = 0
    End Enum

    ''' <summary>
    ''' Enumeration of png compression methods
    ''' </summary>
    Public Enum CompressionMethods
        Deflate = 0
    End Enum

    ''' <summary>
    ''' Enumeration of png bitdepths
    ''' </summary>
    Public Enum BitDepths
        OneBit = 1
        TwoBits = 2
        FourBits = 4
        EightBits = 8
        SixteenBits = 16
    End Enum

    Friend Class InterlacingParameters
        Public Shared START_SCANLINE_INDEX As Integer() = {0, 0, 4, 0, 2, 0, 1}
        Public Shared START_PIXEL_INDEX As Integer() = {0, 4, 0, 2, 0, 1, 0}
        Public Shared SCANLINE_INCREMENT As Integer() = {8, 8, 8, 4, 4, 2, 2}
        Public Shared PIXEL_INCREMENT As Integer() = {8, 8, 4, 4, 2, 2, 1}
    End Class

    Private _Palette As PngPalette = Nothing
    Private _Header As PngHeader = Nothing
    Private _ScanLines As List(Of PngScanline) = Nothing
    Private _UnhandledChunks As New List(Of PngChunk)
    Private _TransparentColor As PngColor = Nothing
#End Region

#Region "Public Properties"
    ''' <summary>
    ''' Width of the image in pixels
    ''' </summary>
    Public ReadOnly Property Width() As Long
        Get
            Return Scanlines(0).Pixels.Count
        End Get
    End Property

    ''' <summary>
    ''' Height of the image in pixels
    ''' </summary>
    Public ReadOnly Property Height() As Long
        Get
            Return _ScanLines.Count
        End Get
    End Property

    ''' <summary>
    ''' Number of bits in a color or alpha sample
    ''' </summary>
    Public ReadOnly Property BitDepth() As Integer
        Get
            Return _Header.BitDepth
        End Get
    End Property

    ''' <summary>
    ''' The color type of a pixel
    ''' </summary>
    Public ReadOnly Property ColorType() As Integer
        Get
            Return _Header.ColorType
        End Get
    End Property

    ''' <summary>
    ''' The interlacing method to apply to image data
    ''' </summary>
    Public ReadOnly Property InterlaceMethod() As Integer
        Get
            Return _Header.InterlaceMethod
        End Get
    End Property

    ''' <summary>
    ''' The filter method to apply to image data
    ''' </summary>
    Public ReadOnly Property FilterMethod() As Integer
        Get
            Return _Header.FilterMethod
        End Get
    End Property

    ''' <summary>
    ''' The compression alogrithm to apply to image data
    ''' </summary>
    Public ReadOnly Property CompressionMethod() As Integer
        Get
            Return _Header.CompressionMethod
        End Get
    End Property

    ''' <summary>
    ''' An indexed list of colors that appear on pixels in the scanlines.
    ''' </summary>
    Public ReadOnly Property Palette() As PngPalette
        Get
            Return _Palette
        End Get
    End Property

    ''' <summary>
    ''' Ordered list of the scanlines that appear in the image.
    ''' </summary>
    Public ReadOnly Property Scanlines() As PngScanline()
        Get
            Return _ScanLines.ToArray
        End Get
    End Property

    ''' <summary>
    ''' Ancillary chunks containing their binary data
    ''' </summary>
    ''' <remarks>
    ''' All ancillary chunks that are not encapsulated in a png objects are kept as raw
    ''' data in a PngChunk.
    ''' </remarks>
    Public ReadOnly Property UnhandledChunks() As PngChunk()
        Get
            Return _UnhandledChunks.ToArray
        End Get
    End Property
#End Region

#Region "Constructors"
    ''' <summary>
    ''' Instantiates a new PngImage
    ''' </summary>
    Public Sub New()

    End Sub

    ''' <summary>
    ''' Instantiates a new PngImage
    ''' </summary>
    ''' <param name="header">
    ''' Header for the new image
    ''' </param>
    ''' <param name="scanlines">
    ''' image data for the new image
    ''' </param>
    ''' <param name="Palette">
    ''' optional palette for the new image
    ''' </param>
    ''' <remarks></remarks>
    Public Sub New( _
        ByVal header As PngHeader, _
        ByVal scanlines As List(Of PngScanline), _
        Optional ByVal Palette As PngPalette = Nothing _
    )
        _Header = header
        _ScanLines = scanlines
        _Palette = Palette
    End Sub

    Friend Sub New( _
        ByVal chunks As List(Of PngChunk) _
    )
        Dim idatProcessed As Boolean = False
        Dim transparentColor As PngColor = Nothing

        For Each chunk As PngChunk In chunks
            Select Case chunk.Name
                Case PngChunk.ChunkNames.PLTE
                    _Palette = New PngPalette(chunk)

                Case PngChunk.ChunkNames.IHDR
                    _Header = New PngHeader(chunk)

                Case PngChunk.ChunkNames.IDAT
                    If idatProcessed = False Then
                        Dim uncompressedData = zStreamReader.ReadStream(readIDATs(chunks))

                        uncompressedData.Seek(0, SeekOrigin.Begin)

                        If _Header.InterlaceMethod = 0 Then
                            _ScanLines = readScanlines(uncompressedData, _
                                                       _Header.Width, _
                                                       _Header.Height, _
                                                       _Header.BitDepth, _
                                                       _Header.ColorType)
                            uncompressedData.Close()
                            idatProcessed = True
                        ElseIf _Header.InterlaceMethod = 1 Then
                            readInterlacedImageData(uncompressedData, _
                                                    _Header.Width, _
                                                    _Header.Height, _
                                                    _Header.BitDepth, _
                                                    _Header.ColorType)
                            uncompressedData.Close()
                        Else
                            Throw New ArgumentOutOfRangeException("Invalid Interlace Method")
                        End If

                    End If
                Case PngChunk.ChunkNames.tRNS
                    processtRNSChunk(chunk, transparentColor)

                Case Else
                    _UnhandledChunks.Add(chunk)
            End Select
        Next

        If transparentColor Is Nothing = False Then
            For Each scanline As PngScanline In _ScanLines
                For Each pixel As PngPixel In scanline.Pixels
                    If PngColor.Compare(pixel.Color, transparentColor) Then
                        Select Case _Header.BitDepth
                            Case 1, 2, 4, 8
                                pixel.Color.setAlpha(New Byte() {0})
                            Case 16
                                pixel.Color.setAlpha(New Byte() {0, 0})
                            Case Else
                                Throw New ArgumentOutOfRangeException("Bit Depth")
                        End Select
                    End If
                Next
            Next
        End If

        If _Palette Is Nothing Then
            _Palette = New PngPalette(_ScanLines, False)
        End If
    End Sub
#End Region

#Region "Private Methods"
    Private Function readIDATs(ByVal chunks As List(Of PngChunk)) As MemoryStream
        Dim dataStream As New MemoryStream

        For Each datChunk As PngChunk In chunks
            If datChunk.Name = PngChunk.ChunkNames.IDAT Then
                Dim buffer(4096) As Byte
                Dim numberRead As Integer = datChunk.Data.Read(buffer, _
                                                               0, _
                                                               4096)

                Do While numberRead > 0
                    dataStream.Write(buffer, 0, numberRead)
                    numberRead = datChunk.Data.Read(buffer, 0, 4096)
                Loop

                datChunk.Data.Close()
            End If
        Next

        Return dataStream
    End Function

    Private Function readScanlines( _
        ByVal dataStream As MemoryStream, _
        ByVal width As Long, _
        ByVal height As Long, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    ) As List(Of PngScanline)
        Dim scanlines As New List(Of PngScanline)
        Dim scanLineByteLength As Integer = calculateScanlineByteLength(width, _
                                                                        bitDepth, _
                                                                        colorType)
        Dim previousScanlineBytes As List(Of Byte()) = Nothing

        For scanlineIndex As Integer = 0 To CInt(height) - 1
            Dim scanLineBytes As List(Of Byte()) = readScanlineBytes(scanLineByteLength, _
                                                                     dataStream, _
                                                                     bitDepth, _
                                                                     colorType)

            PngLibUtil.removeFilter(scanLineBytes, previousScanlineBytes)

            scanlines.Add(createScanlineFromBytes(scanLineBytes, _
                                                  bitDepth, _
                                                  colorType, _
                                                  width))

            previousScanlineBytes = scanLineBytes
        Next

        Return scanlines
    End Function

    Private Function createScanlineFromBytes( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer, _
        ByVal imageWidth As Long _
    ) As PngScanline
        Dim filter As Byte = scanlineBytes(0)(0)
        Dim pixels As New List(Of PngPixel)
        Dim numberOfPixelsWritten As Integer = 0

        For bytesIndex As Integer = 1 To scanlineBytes.Count - 1
            Select Case bitDepth
                Case 1, 2, 4
                    pixels.AddRange(createPngPixelsFromBits(scanlineBytes(bytesIndex)(0), _
                                                            imageWidth, _
                                                            bitDepth, _
                                                            colorType, _
                                                            numberOfPixelsWritten))
                Case 8, 16
                    pixels.Add(createPngPixelFromBytes(scanlineBytes(bytesIndex), _
                                                       bitDepth, _
                                                       colorType))
                Case Else
                    Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
            End Select
        Next

        Return New PngScanline(CType(filter, PngScanline.FilterTypes), pixels)
    End Function

    Private Function createPngPixelsFromBits( _
        ByVal buffer As Byte, _
        ByVal imageWdth As Long, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer, _
        ByRef numberOfPixelsWritten As Integer _
    ) As List(Of PngPixel)
        Dim pixels As New List(Of PngPixel)
        Dim bitMask As Byte = CByte(2 ^ bitDepth - 1)

        For bitIndex As Integer = CInt(8 / bitDepth) - 1 To 0 Step -1
            If numberOfPixelsWritten = imageWdth Then
                Exit For
            Else
                Select Case colorType
                    Case colorTypes.GREYSCALE
                        Dim R As Byte() = New Byte() {CByte(bitMask _
                                          And (buffer >> (bitDepth * bitIndex)))}
                        Dim G As Byte() = New Byte() {CByte(bitMask _
                                          And (buffer >> (bitDepth * bitIndex)))}
                        Dim B As Byte() = New Byte() {CByte(bitMask _
                                          And (buffer >> (bitDepth * bitIndex)))}
                        Dim color As New PngColor(R, G, B, Nothing)

                        pixels.Add(New PngPixel(color))
                    Case colorTypes.INDEXED
                        Dim paletteIndex As Integer = bitMask And (buffer >> (bitDepth * bitIndex))
                        pixels.Add(New PngPixel(paletteIndex))
                    Case Else
                        Throw New ArgumentOutOfRangeException("Invalid Color Type")
                End Select

                numberOfPixelsWritten += 1
            End If
        Next

        Return pixels
    End Function

    Private Function createPngPixelFromBytes( _
        ByVal bytes As Byte(), _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    ) As PngPixel
        Dim pixel As PngPixel = Nothing

        Select Case colorType
            Case colorTypes.GREYSCALE
                Select Case bitDepth
                    Case 8, 16
                        Dim r As Byte() = CType(bytes.Clone, Byte())
                        Dim g As Byte() = CType(bytes.Clone, Byte())
                        Dim b As Byte() = CType(bytes.Clone, Byte())
                        pixel = New PngPixel(New PngColor(r, g, b, Nothing))

                    Case Else
                        Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
                End Select
            Case colorTypes.GREYSCALE_ALPHA
                Select Case bitDepth
                    Case 8
                        Dim colorBytes As Byte() = New Byte() {bytes(0)}

                        Dim r As Byte() = CType(colorBytes.Clone, Byte())
                        Dim g As Byte() = CType(colorBytes.Clone, Byte())
                        Dim b As Byte() = CType(colorBytes.Clone, Byte())
                        Dim alpha As Byte() = New Byte() {bytes(1)}

                        pixel = New PngPixel(New PngColor(r, g, b, alpha))
                    Case 16
                        Dim colorBytes As Byte() = New Byte() {bytes(0), bytes(1)}

                        Dim r As Byte() = CType(colorBytes.Clone, Byte())
                        Dim g As Byte() = CType(colorBytes.Clone, Byte())
                        Dim b As Byte() = CType(colorBytes.Clone, Byte())
                        Dim alpha As Byte() = New Byte() {bytes(2), bytes(3)}

                        pixel = New PngPixel(New PngColor(r, g, b, alpha))

                    Case Else
                        Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
                End Select
            Case colorTypes.TRUECOLOR
                Select Case bitDepth
                    Case 8
                        Dim r As Byte() = New Byte() {bytes(0)}
                        Dim g As Byte() = New Byte() {bytes(1)}
                        Dim b As Byte() = New Byte() {bytes(2)}

                        pixel = New PngPixel(New PngColor(r, g, b, Nothing))
                    Case 16
                        Dim r As Byte() = New Byte() {bytes(0), bytes(1)}
                        Dim g As Byte() = New Byte() {bytes(2), bytes(3)}
                        Dim b As Byte() = New Byte() {bytes(4), bytes(5)}

                        pixel = New PngPixel(New PngColor(r, g, b, Nothing))
                    Case Else
                        Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
                End Select
            Case colorTypes.TRUECOLOR_ALPHA
                Select Case bitDepth
                    Case 8
                        Dim r As Byte() = New Byte() {bytes(0)}
                        Dim g As Byte() = New Byte() {bytes(1)}
                        Dim b As Byte() = New Byte() {bytes(2)}
                        Dim alpha As Byte() = New Byte() {bytes(3)}

                        pixel = New PngPixel(New PngColor(r, g, b, alpha))
                    Case 16
                        Dim r As Byte() = New Byte() {bytes(0), bytes(1)}
                        Dim g As Byte() = New Byte() {bytes(2), bytes(3)}
                        Dim b As Byte() = New Byte() {bytes(4), bytes(5)}
                        Dim alpha As Byte() = New Byte() {bytes(6), bytes(7)}

                        pixel = New PngPixel(New PngColor(r, g, b, alpha))
                    Case Else
                        Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
                End Select
            Case colorTypes.INDEXED
                pixel = New PngPixel(bytes(0))
            Case Else
                Throw New ArgumentOutOfRangeException("Invalid Color Type")
        End Select

        Return pixel
    End Function

    Private Function readScanlineBytes( _
        ByVal scanlineByteLength As Integer, _
        ByVal dataStream As MemoryStream, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    ) As List(Of Byte())
        Dim scanlineBytes As New List(Of Byte())

        Dim filterBytes(0) As Byte
        dataStream.Read(filterBytes, 0, 1)

        scanlineBytes.Add(filterBytes)

        Dim numberOfBitsRead As Integer = 0
        Dim buffer As Byte()
        Dim byteIndex As Integer = 1

        While byteIndex < scanlineByteLength
            Select Case bitDepth
                Case 1, 2, 4
                    buffer = New Byte(0) {}
                    dataStream.Read(buffer, 0, 1)

                    scanlineBytes.Add(buffer)
                    byteIndex += 1
                Case 8, 16
                    Dim samples As Byte() = readBytes(dataStream, bitDepth, colorType)
                    scanlineBytes.Add(samples)

                    byteIndex += samples.Length
                Case Else
                    Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
            End Select
        End While

        Return scanlineBytes
    End Function

    Private Function calculateScanlineByteLength( _
        ByVal width As Long, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    ) As Integer
        Dim length As Integer = 0

        Select Case bitDepth
            Case 1, 2, 4
                'number of bits in a scanline
                Dim bitCount As Long = width * bitDepth

                'number of bits in last byte if not all bits are used in the last byte
                Dim remainderBits As Long = bitCount Mod 8

                length = CInt((bitCount - remainderBits) / 8)

                If remainderBits > 0 Then
                    length += 1
                End If
            Case 8, 16
                Select Case colorType
                    Case colorTypes.GREYSCALE
                        length = CInt(width * (bitDepth / 8))
                    Case colorTypes.TRUECOLOR
                        length = CInt(width * (3 * (bitDepth / 8)))
                    Case colorTypes.INDEXED
                        length = CInt(width)
                    Case colorTypes.GREYSCALE_ALPHA
                        length = CInt(width * (2 * (bitDepth / 8)))
                    Case colorTypes.TRUECOLOR_ALPHA
                        length = CInt(width * (4 * (bitDepth / 8)))
                    Case Else
                        Throw New ArgumentOutOfRangeException("Invalid Color Type")
                End Select
            Case Else
                Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
        End Select

        'add one for filter byte
        Return length + 1
    End Function

    Private Sub processtRNSChunk( _
        ByVal chunk As PngChunk, _
        ByRef transparentColor As PngColor _
    )
        Select Case _Header.ColorType
            Case colorTypes.GREYSCALE
                Dim sampleBytes() As Byte = readBytes(chunk.Data, _
                                                      16, _
                                                      _Header.ColorType)
                Dim r As Byte() = New Byte() {sampleBytes(0), sampleBytes(1)}
                Dim g As Byte() = New Byte() {sampleBytes(0), sampleBytes(1)}
                Dim b As Byte() = New Byte() {sampleBytes(0), sampleBytes(1)}

                transparentColor = New PngColor(r, g, b, Nothing)
            Case colorTypes.TRUECOLOR
                Dim sampleBytes() As Byte = readBytes(chunk.Data, _
                                                      16, _
                                                      _Header.ColorType)
                Dim r As Byte() = New Byte() {sampleBytes(0), sampleBytes(1)}
                Dim g As Byte() = New Byte() {sampleBytes(2), sampleBytes(3)}
                Dim b As Byte() = New Byte() {sampleBytes(4), sampleBytes(5)}

                transparentColor = New PngColor(r, g, b, Nothing)
            Case colorTypes.INDEXED
                Dim transparenecyBuffer(0) As Byte
                Dim numberRead As Integer = chunk.Data.Read(transparenecyBuffer, 0, 1)
                Dim paletteIndex As Integer = 0

                Do Until numberRead = 0
                    _Palette.Entries(paletteIndex).setAlpha(transparenecyBuffer)
                    transparenecyBuffer = New Byte(0) {}
                    numberRead = chunk.Data.Read(transparenecyBuffer, 0, 1)
                    paletteIndex += 1
                Loop
            Case Else
                Throw New ArgumentOutOfRangeException("Invalid Color Type")
        End Select

        chunk.Data.Close()
    End Sub

    Private Function readBytes( _
        ByVal dataStream As MemoryStream, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    ) As Byte()
        Dim sampleCnt As Integer = getSampleByteCnt(colorType, bitDepth)
        Dim rtnBytes(sampleCnt - 1) As Byte

        dataStream.Read(rtnBytes, 0, sampleCnt)

        Return rtnBytes
    End Function

    Private Function getSampleByteCnt( _
        ByVal colorType As Integer, _
        ByVal bitDepth As Integer _
    ) As Integer
        Dim rtnCnt As Integer = 0

        Select Case colorType
            Case colorTypes.GREYSCALE
                rtnCnt = CInt(bitDepth / 8)
            Case colorTypes.TRUECOLOR
                rtnCnt = CInt(3 * (bitDepth / 8))
            Case colorTypes.INDEXED
                rtnCnt = 1
            Case colorTypes.GREYSCALE_ALPHA
                rtnCnt = CInt(2 * (bitDepth / 8))
            Case colorTypes.TRUECOLOR_ALPHA
                rtnCnt = CInt(4 * (bitDepth / 8))
            Case Else
                Throw New ArgumentOutOfRangeException("Invalid Color Type")
        End Select

        Return rtnCnt
    End Function

    Private Sub readInterlacedImageData( _
        ByVal dataStream As MemoryStream, _
        ByVal imageWidth As Long, _
        ByVal imageHeight As Long, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    )
        createEmptyScanlines(imageHeight, imageWidth)

        For passNumber As Integer = 1 To 7
            addPassPixels(passNumber, _
                          imageHeight, _
                          imageWidth, _
                          dataStream, _
                          bitDepth, _
                          colorType)
        Next
    End Sub

    Private Sub addPassPixels( _
        ByVal passNumber As Integer, _
        ByVal imageHeight As Long, _
        ByVal imageWidth As Long, _
        ByVal dataStream As MemoryStream, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    )
        Dim startScanlineIndex As Integer = _
                InterlacingParameters.START_SCANLINE_INDEX(passNumber - 1)
        Dim scanlineIndexIncrement As Integer = _
            InterlacingParameters.SCANLINE_INCREMENT(passNumber - 1)
        Dim startPixelIndex As Integer = _
            InterlacingParameters.START_PIXEL_INDEX(passNumber - 1)
        Dim pixelIndexIncrement As Integer = _
            InterlacingParameters.PIXEL_INCREMENT(passNumber - 1)

        Dim passWidth As Integer = PngLibUtil.calcPassWidth(startPixelIndex, _
                                                            pixelIndexIncrement, _
                                                            imageWidth)
        Dim passHeight As Integer = PngLibUtil.calcPassHeight(startScanlineIndex, _
                                                              scanlineIndexIncrement, _
                                                              imageHeight)

        If passWidth > 0 And passHeight > 0 Then
            Dim passScanlines As List(Of PngScanline) = _
                readScanlines(dataStream, _
                              passWidth, _
                              passHeight, _
                              bitDepth, _
                              colorType)

            Dim scanlineIndex As Integer = startScanlineIndex
            Dim passImageScanlineIndex As Integer = 0

            While scanlineIndex < imageHeight
                Dim pixelIndex As Integer = startPixelIndex
                Dim passImagePixelIndex As Integer = 0

                While pixelIndex < imageWidth
                    setPixel(scanlineIndex, _
                             pixelIndex, _
                             passScanlines(passImageScanlineIndex).Pixels(passImagePixelIndex))
                    pixelIndex += pixelIndexIncrement
                    passImagePixelIndex += 1
                End While

                passImageScanlineIndex += 1
                scanlineIndex += scanlineIndexIncrement
            End While
        End If
    End Sub

    Private Sub createEmptyScanlines(ByVal imageHeight As Long, ByVal imageWidth As Long)
        _ScanLines = New List(Of PngScanline)

        For scanlineIndex As Long = 0 To imageHeight - 1
            Dim filter As PngScanline.FilterTypes = PngScanline.FilterTypes.NoFilter
            Dim pixels As New List(Of PngPixel)

            For pixelIndex As Long = 0 To imageWidth - 1
                pixels.Add(New PngPixel(0))
            Next
            _ScanLines.Add(New PngScanline(filter, pixels))
        Next
    End Sub

    Private Sub setPixel( _
        ByVal scanlineIndex As Integer, _
        ByVal pixelIndex As Integer, _
        ByVal pixel As PngPixel _
    )
        _ScanLines(scanlineIndex).Pixels(pixelIndex) = pixel
    End Sub

    Friend Function getPixel( _
        ByVal scanlineIndex As Integer, _
        ByVal pixelIndex As Integer _
    ) As PngPixel
        Return _ScanLines(scanlineIndex).Pixels(pixelIndex)
    End Function
#End Region
End Class
