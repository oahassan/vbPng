Option Strict On
Option Explicit On

Imports System.IO
Imports vbPng.PngLibUtil

Public Class PngReader
    Private Shared pngSignature() As Integer = {137, 80, 78, 71, 13, 10, 26, 10}

    Public Shared Function ReadPng(ByVal pngStream As FileStream) As PngImage
        Dim signature(7) As Byte
        pngStream.Read(signature, 0, 8)

        'Make sure that png has the correct
        If testSignature(signature) = False Then
            Throw New ArgumentException("Stream has an invalid signature!")
        End If

        Dim chunks As New List(Of PngChunk)
        Dim chunk As PngChunk = readChunk(pngStream)

        Do Until chunk.Name = PngChunk.ChunkNames.IEND
            chunks.Add(chunk)
            chunk = readChunk(pngStream)

        Loop

        Return New PngImage(chunks)
    End Function

    Private Shared Function testSignature(ByVal streamSignature As Byte()) As Boolean
        Dim indicator As Boolean = True

        If streamSignature.Length = pngSignature.Length Then
            For sigIndex As Integer = 0 To pngSignature.Length - 1
                If pngSignature(sigIndex) <> streamSignature(sigIndex) Then
                    indicator = False
                    Exit For
                End If
            Next
        Else
            indicator = False
        End If

        Return indicator
    End Function

    Public Shared Function readChunk(ByVal pngStream As FileStream) As PngChunk

        Dim lengthBytes(3) As Byte
        pngStream.Read(lengthBytes, 0, 4)

        'Can't use binary reader's readInt32 because it expects little endian byte order,
        'but the png specification calls for big endian byte order.
        Dim length As Long = byteArrayToLong(lengthBytes)

        'Get chunk name and data
        Dim crcInput As New MemoryStream()
        Dim byteBuffer(0) As Byte

        For i As Long = 1 To length + 4
            pngStream.Read(byteBuffer, 0, 1)
            crcInput.WriteByte(byteBuffer(0))
        Next

        'Get check bits val.  No need to convert the bytes to little endian order since 
        'we don't care about the value
        Dim crcByteBuffer(3) As Byte
        pngStream.Read(crcByteBuffer, 0, 4)
        Dim crcVal As UInteger = CUInt(byteArrayToLong(crcByteBuffer))

        crcInput.Seek(0, SeekOrigin.Begin)

        'Read chunk name into long variable
        Dim hdrNmBytes(3) As Byte
        crcInput.Read(hdrNmBytes, 0, 4)

        Dim name As String = readChunkName(hdrNmBytes)

        'Write data to stream
        Dim dataBytes As New MemoryStream

        For i As Long = 1 To length
            crcInput.Read(byteBuffer, 0, 1)
            dataBytes.WriteByte(byteBuffer(0))
        Next

        'TODO - CRC calculation
        'Set position to beginning to calculate CRC over chunk name and data
        crcInput.Seek(0, SeekOrigin.Begin)

        Dim calcCrc As UInteger = CRCGenerator.crc(crcInput)

        If crcVal <> calcCrc Then
            Throw New Exception("CRC check failed!")
        End If

        dataBytes.Seek(0, SeekOrigin.Begin)

        Return New PngChunk(CUInt(length), name, dataBytes, crcVal)
    End Function

    Private Shared Function readChunkName(ByVal bytes() As Byte) As String
        Dim name As String = String.Empty

        For Each nameByte As Byte In bytes
            name += ChrW(nameByte)
        Next

        Return name
    End Function
End Class
