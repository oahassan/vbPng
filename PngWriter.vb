Option Explicit On
Option Strict On

Imports System.IO

Public Class PngWriter
    Private Class ChunkSignatures
        Public Shared IHDR() As Byte = {73, 72, 68, 82}
        Public Shared PLTE() As Byte = {80, 76, 84, 69}
        Public Shared IDAT() As Byte = {73, 68, 65, 84}
        Public Shared IEND() As Byte = {73, 69, 78, 68}
        Public Shared tRNS() As Byte = {116, 82, 78, 83}
    End Class

    Public Sub Write( _
        ByVal filePath As String, _
        ByVal png As PngImage _
    )
        Dim pngStream As New FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)

        writePngSignature(pngStream)

        writeChunk(png, pngStream, "IHDR")
        If png.ColorType = PngImage.colorTypes.INDEXED Then
            writeChunk(png, pngStream, "PLTE")
        End If

        If png.Palette.HasNonOpaqueEntries Then
            Select Case png.ColorType
                Case PngImage.colorTypes.GREYSCALE, PngImage.colorTypes.INDEXED, PngImage.colorTypes.TRUECOLOR
                    writeChunk(png, pngStream, "tRNS")
                Case Else
                    'no trasparency is valid for other color types
            End Select
        End If

        writeChunk(png, pngStream, "IDAT")
        writeChunk(png, pngStream, "IEND")

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
        ByVal chunkNm As String _
    )
        Dim chunkDataStream As MemoryStream = Nothing

        Select Case chunkNm
            Case "IHDR"
                chunkDataStream = getHeaderChunkData(png)
            Case "PLTE"
                chunkDataStream = getPaletteChunkData(png)
            Case "tRNS"
                chunkDataStream = getTransparencyChunkData(png)
            Case "IDAT"
                chunkDataStream = writeImageDataChunk(png)
            Case "IEND"
                chunkDataStream = writeEndChunk()
            Case Else
                'nothing for now
        End Select

        chunkDataStream.Seek(0, SeekOrigin.Begin)
        Dim crc As UInteger = CRCGenerator.crc(chunkDataStream)

        'Write chunk length
        writeUInt(pngStream, CUInt(chunkDataStream.Length - 4))

        'Write signature and data
        chunkDataStream.Seek(0, SeekOrigin.Begin)

        Dim bufferBytes(4096) As Byte
        Dim nbrRead As Integer = chunkDataStream.Read(bufferBytes, 0, bufferBytes.Length)

        While nbrRead <> 0
            pngStream.Write(bufferBytes, 0, nbrRead)
            nbrRead = chunkDataStream.Read(bufferBytes, 0, bufferBytes.Length)
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

        writeChunkSignature(dataStream, ChunkSignatures.IHDR)

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

        writeChunkSignature(dataStream, ChunkSignatures.PLTE)

        For Each color As PngColor In png.Palette.Entries
            For byteIndex = 0 To color.ColorSamples.Length - 1
                dataStream.WriteByte(color.ColorSamples(byteIndex))
            Next
        Next

        Return dataStream
    End Function

    Private Function getTransparencyChunkData(ByVal png As PngImage) As MemoryStream
        Dim dataStream As New MemoryStream

        writeChunkSignature(dataStream, ChunkSignatures.tRNS)

        Select Case png.ColorType
            Case PngImage.colorTypes.GREYSCALE
                Dim transparencyPalette As PngPalette

                If png.Palette Is Nothing Then
                    transparencyPalette = New PngPalette(New List(Of PngScanline)(png.Scanlines))
                Else
                    transparencyPalette = png.Palette
                End If

                transparencyPalette.truncateEntries()

                For Each entry As PngColor In transparencyPalette.Entries
                    If entry.AlphaSample Is Nothing Then
                        'nothing
                    Else
                        If png.BitDepth = 8 Then
                            If entry.AlphaSample(0) = 0 Then
                                dataStream.WriteByte(0)
                                dataStream.WriteByte(entry.ColorSamples(0))
                            End If

                            Exit For
                        ElseIf png.BitDepth = 16 Then
                            If entry.AlphaSample(0) = 0 And entry.AlphaSample(1) = 0 Then
                                For Each sampleByte As Byte In entry.ColorSamples
                                    dataStream.WriteByte(sampleByte)
                                Next
                            End If

                            Exit For
                        End If
                    End If
                Next
            Case PngImage.colorTypes.TRUECOLOR
                Dim transparencyPalette As PngPalette

                If png.Palette Is Nothing Then
                    transparencyPalette = New PngPalette(New List(Of PngScanline)(png.Scanlines))
                Else
                    transparencyPalette = png.Palette
                End If

                transparencyPalette.truncateEntries()

                For Each entry As PngColor In transparencyPalette.Entries
                    If entry.AlphaSample Is Nothing Then
                        'nothing
                    Else
                        If png.BitDepth = 8 Then
                            If entry.AlphaSample(0) = 0 Then
                                dataStream.WriteByte(0)
                                dataStream.WriteByte(entry.R(0))

                                dataStream.WriteByte(0)
                                dataStream.WriteByte(entry.G(0))

                                dataStream.WriteByte(0)
                                dataStream.WriteByte(entry.B(0))

                                Exit For
                            End If
                        ElseIf png.BitDepth = 16 Then
                            If entry.AlphaSample(0) = 0 And entry.AlphaSample(1) = 0 Then
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
                            End If
                        End If
                    End If
                Next
            Case PngImage.colorTypes.INDEXED
                Dim transparencyPalette As PngPalette

                If png.Palette Is Nothing Then
                    transparencyPalette = New PngPalette(New List(Of PngScanline)(png.Scanlines))
                Else
                    transparencyPalette = png.Palette
                End If

                transparencyPalette.truncateEntries()

                For Each color As PngColor In transparencyPalette.Entries
                    If color.AlphaSample Is Nothing Then
                        dataStream.WriteByte(255)
                    Else
                        If color.AlphaSample.Length = 1 Then
                            dataStream.WriteByte(color.AlphaSample(0))
                        ElseIf color.AlphaSample.Length = 2 Then
                            dataStream.WriteByte(color.AlphaSample(0))
                            dataStream.WriteByte(color.AlphaSample(1))
                        End If

                    End If
                Next
        End Select

        Return dataStream
    End Function

    Private Function writeImageDataChunk(ByVal png As PngImage) As MemoryStream
        Dim dataStream As New MemoryStream

        For scanlineIndex As Integer = 0 To png.Scanlines.Length - 1
            Dim scanline As PngScanline = png.Scanlines(scanlineIndex)

            If scanlineIndex = 0 Then
                scanline.ApplyFilter(Nothing)
            Else
                scanline.ApplyFilter(png.Scanlines(scanlineIndex - 1))
            End If

            dataStream.WriteByte(scanline.Filter(0))

            For pixelIndex As Integer = 0 To scanline.Pixels.Count - 1
                Dim pixel As PngPixel = scanline.Pixels(pixelIndex)

                Select Case True
                    Case png.ColorType = PngImage.colorTypes.GREYSCALE, _
                        png.ColorType = PngImage.colorTypes.TRUECOLOR

                        If pixel.Color Is Nothing Then
                            For Each sampleByte As Byte In png.Palette.Entries(pixel.PaletteIndex).ColorSamples
                                dataStream.WriteByte(sampleByte)
                            Next
                        Else
                            For Each sampleByte As Byte In pixel.Color.ColorSamples
                                dataStream.WriteByte(sampleByte)
                            Next
                        End If

                    Case png.ColorType = PngImage.colorTypes.GREYSCALE_ALPHA, _
                        png.ColorType = PngImage.colorTypes.TRUECOLOR_ALPHA

                        If pixel.Color Is Nothing Then
                            For Each sampleByte As Byte In png.Palette.Entries(pixel.PaletteIndex).ColorSamples
                                dataStream.WriteByte(sampleByte)
                            Next
                        Else
                            For Each sampleByte As Byte In pixel.Color.ColorSamples
                                dataStream.WriteByte(sampleByte)
                            Next

                            For byteIndex As Integer = 0 To pixel.Color.AlphaSample.Length - 1
                                Dim sampleByte As Byte = pixel.Color.AlphaSample(byteIndex)
                                dataStream.WriteByte(sampleByte)
                            Next
                        End If

                    Case png.ColorType = PngImage.colorTypes.INDEXED
                        dataStream.WriteByte(CByte(pixel.PaletteIndex))
                    Case Else
                        Throw New ArgumentOutOfRangeException("png.ColorType")
                End Select
            Next
        Next

        dataStream.Seek(0, SeekOrigin.Begin)
        Dim adler As UInteger = Adler32.adler32(dataStream)

        dataStream.Seek(0, SeekOrigin.Begin)
        Dim compressedDataStream As New MemoryStream

        writeChunkSignature(compressedDataStream, ChunkSignatures.IDAT)

        'Write zlib compression method and compression info bits
        Dim CMFByte As Byte = (7 << 4) + 8

        'add flag bits indicating no preset dictionary and highest compression method
        Dim FLGByte As Byte = (3 << 6)

        'add checkbits
        FLGByte += getFCheckBits(CMFByte, FLGByte)

        compressedDataStream.WriteByte(CMFByte)
        compressedDataStream.WriteByte(FLGByte)

        Dim compressedDataWriter As New Compression.DeflateStream(compressedDataStream, Compression.CompressionMode.Compress, True)

        Dim compressionBuffer(32768) As Byte
        Dim nbrRead As Integer = dataStream.Read(compressionBuffer, 0, compressionBuffer.Length)

        While nbrRead <> 0
            compressedDataWriter.Write(compressionBuffer, 0, nbrRead)
            nbrRead = dataStream.Read(compressionBuffer, 0, compressionBuffer.Length)
        End While

        dataStream.Close()
        compressedDataWriter.Close()

        'Write zlib adler checksum
        writeUInt(compressedDataStream, adler)

        Return compressedDataStream
    End Function

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

        writeChunkSignature(dataStream, ChunkSignatures.IEND)

        Return dataStream
    End Function

    Private Sub writeChunkSignature( _
        ByVal writer As Stream, _
        ByVal chunkSignature As Byte() _
    )
        For Each signatureByte As Byte In chunkSignature
            writer.WriteByte(signatureByte)
        Next
    End Sub

    Private Sub writeUInt(ByVal writeStream As Stream, ByVal val As UInteger)
        writeStream.WriteByte(CByte((val And &HFF000000UI) >> 24))
        writeStream.WriteByte(CByte((val And &HFF0000UI) >> 16))
        writeStream.WriteByte(CByte((val And &HFF00UI) >> 8))
        writeStream.WriteByte(CByte(val And &HFFUI))
    End Sub
End Class
