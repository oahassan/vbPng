Option Explicit On
Option Strict On

Imports System.IO

''' <summary>
''' Writes PngImages to Png files
''' </summary>
Public Class PngWriter
    ''' <summary>
    ''' Writes a PNG file for a given PngImage
    ''' </summary>
    ''' <param name="filePath">
    ''' Path to write png file to
    ''' </param>
    ''' <param name="png">
    ''' PngImage to write to path
    ''' </param>
    Public Sub WritePng( _
        ByVal filePath As String, _
        ByVal png As PngImage _
    )
        Dim pngStream As New FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)

        writePngSignature(pngStream)

        writeChunk(png, pngStream, "IHDR")

        For Each unhandledChunk As PngChunk In png.UnhandledChunks
            If PngChunk._PrePLTEChunks.Contains(unhandledChunk.Name) Then
                writeChunk(png, pngStream, unhandledChunk.Name, unhandledChunk.Data)
            End If
        Next

        If png.ColorType = PngImage.colorTypes.INDEXED Then
            writeChunk(png, pngStream, "PLTE")
        End If

        For Each unhandledChunk As PngChunk In png.UnhandledChunks
            If PngChunk._PostPLTEChunks.Contains(unhandledChunk.Name) _
            AndAlso unhandledChunk.Name <> PngChunk.ChunkNames.tRNS Then
                writeChunk(png, pngStream, unhandledChunk.Name, unhandledChunk.Data)
            End If
        Next

        If png.Palette.HasNonOpaqueEntries Then
            Select Case png.ColorType
                Case PngImage.colorTypes.GREYSCALE, PngImage.colorTypes.INDEXED, PngImage.colorTypes.TRUECOLOR
                    writeChunk(png, pngStream, PngChunk.ChunkNames.tRNS)
                Case Else
                    'no trasparency is valid for other color types
            End Select
        End If

        writeChunk(png, pngStream, PngChunk.ChunkNames.IDAT)
        writeChunk(png, pngStream, PngChunk.ChunkNames.IEND)

        pngStream.Close()
    End Sub

    Private Sub writePngSignature(ByVal writer As Stream)
        Dim pngSignature() As Byte = {137, 80, 78, 71, 13, 10, 26, 10}

        For Each sigByte As Byte In pngSignature
            writer.WriteByte(sigByte)
        Next
    End Sub

    Private Sub writeChunk( _
        ByVal png As PngImage, _
        ByVal pngStream As Stream, _
        ByVal chunkNm As String, _
        Optional ByVal chunkData As MemoryStream = Nothing _
    )
        Dim chunkDataStream As MemoryStream = Nothing
        Dim numberRead As Integer = 0
        Dim bufferBytes(4096) As Byte

        Select Case chunkNm
            Case PngChunk.ChunkNames.IHDR
                chunkDataStream = getHeaderChunkData(png)
            Case PngChunk.ChunkNames.PLTE
                chunkDataStream = getPaletteChunkData(png)
            Case PngChunk.ChunkNames.tRNS
                chunkDataStream = getTransparencyChunkData(png)
            Case PngChunk.ChunkNames.IDAT
                chunkDataStream = writeImageDataChunk(png)
            Case PngChunk.ChunkNames.IEND
                chunkDataStream = writeEndChunk()
            Case Else
                If chunkData Is Nothing Then
                    Throw New ArgumentNullException("chunkData")
                Else
                    Dim dataStream As New MemoryStream

                    'Write signature
                    writeChunkSignature(dataStream, chunkNm)

                    numberRead = chunkData.Read(bufferBytes, 0, 4096)

                    Do Until numberRead = 0
                        dataStream.Write(bufferBytes, 0, numberRead)
                        numberRead = chunkData.Read(bufferBytes, 0, 4096)
                    Loop

                    chunkDataStream = dataStream
                End If
        End Select

        chunkDataStream.Seek(0, SeekOrigin.Begin)
        Dim crc As UInteger = CRCGenerator.crc(chunkDataStream)

        'Write chunk length
        writeUInt(pngStream, CUInt(chunkDataStream.Length - 4))

        chunkDataStream.Seek(0, SeekOrigin.Begin)

        'Write data and name
        numberRead = chunkDataStream.Read(bufferBytes, 0, bufferBytes.Length)

        While numberRead <> 0
            pngStream.Write(bufferBytes, 0, numberRead)
            numberRead = chunkDataStream.Read(bufferBytes, 0, bufferBytes.Length)
        End While

        chunkDataStream.Close()

        'Write crc
        writeUInt(pngStream, crc)
    End Sub

    Private Function getHeaderChunkData( _
        ByVal png As PngImage _
    ) As MemoryStream
        'Write chunk data for crc
        Dim dataStream As New MemoryStream

        'Write signature
        writeChunkSignature(dataStream, PngChunk.ChunkNames.IHDR)

        writeUInt(dataStream, CUInt(png.Width))
        writeUInt(dataStream, CUInt(png.Height))
        dataStream.WriteByte(CByte(png.BitDepth))
        dataStream.WriteByte(CByte(png.ColorType))
        dataStream.WriteByte(CByte(png.CompressionMethod))
        dataStream.WriteByte(CByte(png.FilterMethod))
        dataStream.WriteByte(CByte(png.InterlaceMethod))

        Return dataStream
    End Function

    Private Function getPaletteChunkData(ByVal png As PngImage) As MemoryStream
        Dim dataStream As New MemoryStream

        'Write signature
        writeChunkSignature(dataStream, PngChunk.ChunkNames.PLTE)

        For Each color As PngColor In png.Palette.Entries
            For byteIndex = 0 To color.R.Length - 1
                dataStream.WriteByte(color.R(byteIndex))
            Next

            For byteIndex = 0 To color.G.Length - 1
                dataStream.WriteByte(color.G(byteIndex))
            Next

            For byteIndex = 0 To color.B.Length - 1
                dataStream.WriteByte(color.B(byteIndex))
            Next
        Next

        Return dataStream
    End Function

    Private Function getTransparencyChunkData(ByVal png As PngImage) As MemoryStream
        Dim dataStream As New MemoryStream

        'Write signature
        writeChunkSignature(dataStream, PngChunk.ChunkNames.tRNS)

        Select Case png.ColorType
            Case PngImage.colorTypes.GREYSCALE
                Dim transparencyPalette As PngPalette

                If png.Palette Is Nothing Then
                    transparencyPalette = New PngPalette(New List(Of PngScanline)(png.Scanlines), _
                                                         False)
                Else
                    transparencyPalette = png.Palette
                End If

                transparencyPalette.truncateEntries()

                For Each entry As PngColor In transparencyPalette.Entries
                    If entry.Alpha Is Nothing Then
                        'nothing
                    Else
                        Select Case png.BitDepth
                            Case 1, 2, 4, 8
                                If entry.Alpha(0) = 0 Then
                                    dataStream.WriteByte(0)
                                    dataStream.WriteByte(entry.Samples(0))
                                End If

                                Exit For
                            Case 16
                                If entry.Alpha(0) = 0 And entry.Alpha(1) = 0 Then
                                    For Each sampleByte As Byte In entry.Samples
                                        dataStream.WriteByte(sampleByte)
                                    Next
                                End If

                                Exit For
                            Case Else
                                Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
                        End Select
                    End If
                Next
            Case PngImage.colorTypes.TRUECOLOR
                Dim transparencyPalette As PngPalette

                If png.Palette Is Nothing Then
                    transparencyPalette = New PngPalette(New List(Of PngScanline)(png.Scanlines), _
                                                         False)
                Else
                    transparencyPalette = png.Palette
                End If

                transparencyPalette.truncateEntries()

                For Each entry As PngColor In transparencyPalette.Entries
                    If entry.Alpha Is Nothing Then
                        'nothing
                    Else
                        Select Case png.BitDepth
                            Case 1, 2, 4, 8
                                dataStream.WriteByte(0)
                                dataStream.WriteByte(entry.R(0))

                                dataStream.WriteByte(0)
                                dataStream.WriteByte(entry.G(0))

                                dataStream.WriteByte(0)
                                dataStream.WriteByte(entry.B(0))

                                Exit For
                            Case 16
                                For Each sampleByte As Byte In entry.R
                                    dataStream.WriteByte(sampleByte)
                                Next

                                For Each sampleByte As Byte In entry.G
                                    dataStream.WriteByte(sampleByte)
                                Next

                                For Each sampleByte As Byte In entry.B
                                    dataStream.WriteByte(sampleByte)
                                Next

                                Exit For
                            Case Else
                                Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
                        End Select
                    End If
                Next
            Case PngImage.colorTypes.INDEXED
                Dim transparencyPalette As PngPalette

                If png.Palette Is Nothing Then
                    transparencyPalette = New PngPalette(New List(Of PngScanline)(png.Scanlines), _
                                                         False)
                Else
                    transparencyPalette = png.Palette
                End If

                transparencyPalette.truncateEntries()

                For Each color As PngColor In transparencyPalette.Entries
                    If color.Alpha Is Nothing Then
                        dataStream.WriteByte(255)
                    Else
                        If color.Alpha.Length = 1 Then
                            dataStream.WriteByte(color.Alpha(0))
                        ElseIf color.Alpha.Length = 2 Then
                            dataStream.WriteByte(color.Alpha(0))
                            dataStream.WriteByte(color.Alpha(1))
                        End If

                    End If
                Next
        End Select

        Return dataStream
    End Function

    Private Function writeImageDataChunk(ByVal png As PngImage) As MemoryStream
        Dim dataStream As New MemoryStream

        If png.InterlaceMethod = 0 Then
            writeScanlinesToDataStream(dataStream, _
                                       png.Scanlines, _
                                       png.BitDepth, _
                                       png.ColorType)
        ElseIf png.InterlaceMethod = 1 Then
            writeInterlacedImageDataToStream(dataStream, png)
        Else
            Throw New ArgumentOutOfRangeException("Invalid Interlace Method")
        End If

        dataStream.Seek(0, SeekOrigin.Begin)
        Dim adler As UInteger = Adler32.adler32(dataStream)

        dataStream.Seek(0, SeekOrigin.Begin)
        Dim compressedDataStream As New MemoryStream

        'Write signature
        writeChunkSignature(compressedDataStream, PngChunk.ChunkNames.IDAT)

        'zlib compression method and compression info bits
        Dim CMFByte As Byte = (7 << 4) + 8

        'flag bits indicating no preset dictionary and highest compression method
        Dim FLGByte As Byte = (3 << 6)

        'add checkbits
        FLGByte += getFCheckBits(CMFByte, FLGByte)

        compressedDataStream.WriteByte(CMFByte)
        compressedDataStream.WriteByte(FLGByte)

        Dim compressedDataWriter As New Compression.DeflateStream(compressedDataStream, _
                                                                  Compression.CompressionMode.Compress, _
                                                                  True)

        Dim compressionBuffer(32768) As Byte
        Dim numberRead As Integer = dataStream.Read(compressionBuffer, _
                                                 0, _
                                                 compressionBuffer.Length)

        While numberRead <> 0
            compressedDataWriter.Write(compressionBuffer, 0, numberRead)
            numberRead = dataStream.Read(compressionBuffer, 0, compressionBuffer.Length)
        End While

        dataStream.Close()
        compressedDataWriter.Close()

        'Write zlib adler checksum
        writeUInt(compressedDataStream, adler)

        Return compressedDataStream
    End Function

    Private Sub writeInterlacedImageDataToStream( _
        ByVal dataStream As MemoryStream, _
        ByVal png As PngImage _
    )
        For passNumber As Integer = 1 To 7
            Dim startScanlineIndex As Integer = _
                PngImage.InterlacingParameters.START_SCANLINE_INDEX(passNumber - 1)
            Dim scanlineIndexIncrement As Integer = _
                PngImage.InterlacingParameters.SCANLINE_INCREMENT(passNumber - 1)
            Dim startPixelIndex As Integer = _
                PngImage.InterlacingParameters.START_PIXEL_INDEX(passNumber - 1)
            Dim pixelIndexIncrement As Integer = _
                PngImage.InterlacingParameters.PIXEL_INCREMENT(passNumber - 1)

            Dim passWidth As Integer = PngLibUtil.calcPassWidth(startPixelIndex, _
                                                                pixelIndexIncrement, _
                                                                png.Width)
            Dim passHeight As Integer = PngLibUtil.calcPassHeight(startScanlineIndex, _
                                                                  scanlineIndexIncrement, _
                                                                  png.Height)

            If passWidth > 0 And passHeight > 0 Then
                Dim passScanlines As PngScanline() = getPassScanlines(passNumber, _
                                                                      dataStream, _
                                                                      png).ToArray
                writeScanlinesToDataStream(dataStream, _
                                           passScanlines, _
                                           png.BitDepth, _
                                           png.ColorType)
            End If
        Next
    End Sub

    Private Function getPassScanlines( _
        ByVal passNumber As Integer, _
        ByVal dataStream As MemoryStream, _
        ByVal png As PngImage _
    ) As List(Of PngScanline)
        Dim passScanlines As New List(Of PngScanline)

        Dim startScanlineIndex As Integer = _
            PngImage.InterlacingParameters.START_SCANLINE_INDEX(passNumber - 1)
        Dim scanlineIndexIncrement As Integer = _
            PngImage.InterlacingParameters.SCANLINE_INCREMENT(passNumber - 1)
        Dim startPixelIndex As Integer = _
            PngImage.InterlacingParameters.START_PIXEL_INDEX(passNumber - 1)
        Dim pixelIndexIncrement As Integer = _
            PngImage.InterlacingParameters.PIXEL_INCREMENT(passNumber - 1)
        Dim scanlineIndex As Integer = startScanlineIndex

        While scanlineIndex < png.Height
            Dim pixelIndex As Integer = startPixelIndex

            Dim filter As PngScanline.FilterTypes = PngScanline.FilterTypes.NoFilter
            Dim pixels As New List(Of PngPixel)

            While pixelIndex < png.Width
                pixels.Add(png.getPixel(scanlineIndex, _
                         pixelIndex))
                pixelIndex += pixelIndexIncrement
            End While

            passScanlines.Add(New PngScanline(filter, pixels))

            scanlineIndex += scanlineIndexIncrement
        End While

        Return passScanlines
    End Function

    Private Sub writeScanlinesToDataStream( _
        ByVal dataStream As MemoryStream, _
        ByVal scanlines As PngScanline(), _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    )
        Dim previousScanlineBytes As List(Of Byte()) = Nothing

        For scanlineIndex As Integer = 0 To scanlines.Length - 1
            Dim scanline As PngScanline = scanlines(scanlineIndex)

            Dim scanlineBytes As New List(Of Byte())
            scanlineBytes.Add(New Byte() {CByte(scanline.Filter)})
            'scanlineBytes.Add(New Byte() {0})

            'Used for bit depths 1, 2 and 4
            Dim bufferByte As Byte = 0
            Dim numberOfBitsWritten As Integer = 0
            Dim firstWriteIndicator As Boolean = True

            For pixelIndex As Integer = 0 To scanline.Pixels.Count - 1
                Dim pixel As PngPixel = scanline.Pixels(pixelIndex)

                Select Case bitDepth
                    Case 1, 2, 4
                        packBitsIntoBytes(pixel, _
                                         scanlineBytes, _
                                         bitDepth, _
                                         bufferByte, _
                                         numberOfBitsWritten, _
                                         firstWriteIndicator)
                    Case 8, 16
                        packBytes(scanlineBytes, pixel, bitDepth, colorType)
                    Case Else
                        Throw New ArgumentOutOfRangeException("Invalid Bit Depth")
                End Select
            Next

            If bitDepth = 1 _
            Or bitDepth = 2 _
            Or bitDepth = 4 Then
                scanlineBytes.Add(New Byte(0) {bufferByte})
            End If

            Dim filteredBytes As New List(Of Byte())
            filteredBytes.Add(New Byte() {CByte(scanline.Filter)})
            filteredBytes.AddRange(PngLibUtil.ApplyFilter(scanlineBytes, previousScanlineBytes))

            For bytesIndex As Integer = 0 To filteredBytes.Count - 1
                dataStream.Write(filteredBytes(bytesIndex), 0, filteredBytes(bytesIndex).Length)
            Next

            previousScanlineBytes = scanlineBytes
        Next
    End Sub

    Private Sub packBitsIntoBytes( _
        ByVal pixel As PngPixel, _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal bitDepth As Integer, _
        ByRef bufferByte As Byte, _
        ByRef numberOfBitsWritten As Integer, _
        ByRef firstWriteIndicator As Boolean _
    )
        If firstWriteIndicator Then
            firstWriteIndicator = False
        ElseIf numberOfBitsWritten = 0 Then
            scanlineBytes.Add(New Byte(0) {bufferByte})
            bufferByte = 0
        End If

        Select Case bitDepth
            Case 1, 2, 4
                If pixel.Color Is Nothing Then
                    bufferByte = CByte(bufferByte Xor (pixel.PaletteIndex << (8 - numberOfBitsWritten - bitDepth)))
                Else
                    bufferByte = CByte(bufferByte Xor (pixel.Color.Samples(0) << (8 - numberOfBitsWritten - bitDepth)))
                End If

                numberOfBitsWritten = (numberOfBitsWritten + bitDepth) Mod 8
            Case Else
                Throw New ArgumentOutOfRangeException("BitDepth")
        End Select
    End Sub

    Private Sub packBytes( _
        ByVal scanlineBytes As List(Of Byte()), _
        ByVal pixel As PngPixel, _
        ByVal bitDepth As Integer, _
        ByVal colorType As Integer _
    )
        If pixel.Color Is Nothing Then
            scanlineBytes.Add(New Byte(0) {CByte(pixel.PaletteIndex)})
        Else
            Select Case colorType
                Case PngImage.colorTypes.GREYSCALE
                    scanlineBytes.Add(pixel.Color.R)
                Case PngImage.colorTypes.GREYSCALE_ALPHA
                    Dim bytes As New List(Of Byte)
                    bytes.AddRange(pixel.Color.R)
                    bytes.AddRange(pixel.Color.Alpha)

                    scanlineBytes.Add(bytes.ToArray)
                Case PngImage.colorTypes.TRUECOLOR
                    Dim bytes As New List(Of Byte)

                    bytes.AddRange(pixel.Color.R)
                    bytes.AddRange(pixel.Color.G)
                    bytes.AddRange(pixel.Color.B)

                    scanlineBytes.Add(bytes.ToArray)
                Case PngImage.colorTypes.TRUECOLOR_ALPHA
                    Dim bytes As New List(Of Byte)

                    bytes.AddRange(pixel.Color.R)
                    bytes.AddRange(pixel.Color.G)
                    bytes.AddRange(pixel.Color.B)
                    bytes.AddRange(pixel.Color.Alpha)

                    scanlineBytes.Add(bytes.ToArray)
                Case Else
                    Throw New ArgumentOutOfRangeException("Invalid Color Type")
            End Select

        End If
    End Sub

    Private Function getFCheckBits(ByVal CMF As Byte, ByVal FLG As Byte) As Byte
        Dim testVal As UShort = CUShort(CMF * 256 + FLG)
        Dim FCheck As UShort = 0

        While testVal Mod 31 <> 0
            FCheck += 1US
            testVal = CUShort(CMF * 256 + FLG) + FCheck
        End While

        Return CByte(FCheck)
    End Function

    Private Function writeEndChunk() As MemoryStream
        Dim dataStream As New MemoryStream

        'Write signature
        writeChunkSignature(dataStream, PngChunk.ChunkNames.IEND)

        Return dataStream
    End Function

    Private Sub writeChunkSignature( _
        ByVal writer As Stream, _
        ByVal chunkNm As String _
    )
        For Each signatureChar As Char In chunkNm.ToCharArray
            writer.WriteByte(CByte(AscW(signatureChar)))
        Next
    End Sub

    ''' <summary>
    ''' Converts UInt from LSB-order to MSB-order and writes it to the memory stream
    ''' </summary>
    Private Sub writeUInt(ByVal writeStream As Stream, ByVal val As UInteger)
        writeStream.WriteByte(CByte((val And &HFF000000UI) >> 24))
        writeStream.WriteByte(CByte((val And &HFF0000UI) >> 16))
        writeStream.WriteByte(CByte((val And &HFF00UI) >> 8))
        writeStream.WriteByte(CByte(val And &HFFUI))
    End Sub
End Class
