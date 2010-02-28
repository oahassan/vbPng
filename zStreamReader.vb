Option Strict On
Option Explicit

Imports System.IO

Public Class zStreamReader

    Private Const MaxWindowSize As Byte = &H7

    Private Class BitMasks
        Public Const CompressionMethod As Byte = &HF0
        Public Const CompressionInfo As Byte = &HF
        Public Const CheckBits As Byte = &HF0
        Public Const Flags As Byte = &HF
        Public Const DictionaryFlag As Byte = &H8
        Public Const CompressionLevel As Byte = &H3
    End Class

    Private Class CompressionMethods
        Public Const DEFLATE As Byte = &H80
        Public Const NONE As Byte = &H0
    End Class

    Public Shared Function ReadStream(ByVal zStream As MemoryStream) As MemoryStream

        Dim dataStream As New MemoryStream

        'Read past CMF and FLG bytes
        zStream.Seek(2, SeekOrigin.Begin)

        'Data stream length is stream length minus 2 bytes for CMF and FLGs and minus
        'another 4 bytes for the checksum at the end
        For i As Long = 1 To zStream.Length - 6
            Dim bufferBytes(0) As byte
            zStream.Read(bufferBytes, 0, 1)
            dataStream.Write(bufferBytes, 0, 1)
        Next

        dataStream.Seek(0, SeekOrigin.Begin)

        Dim decompressedStream As New System.IO.Compression.DeflateStream(dataStream, Compression.CompressionMode.Decompress, True)
        Dim decompressedData As New MemoryStream

        Dim buffer As Byte() = New Byte(32768) {}
        Dim numRead As Integer
        numRead = decompressedStream.Read(buffer, 0, buffer.Length)

        Do While numRead <> 0
            decompressedData.Write(buffer, 0, numRead)
            numRead = decompressedStream.Read(buffer, 0, buffer.Length)
        Loop

        decompressedStream.Close()

        decompressedData.Seek(0, SeekOrigin.Begin)

        Dim adlerBytes(3) As Byte
        zStream.Read(adlerBytes, 0, 4)
        Dim adlerVal As UInteger = CUInt(PngLibUtil.byteArrayToLong(adlerBytes))
        Dim calcAdler As UInteger = Adler32.adler32(decompressedData)

        If calcAdler <> adlerVal Then
            Throw New ArgumentException("Adler does not match!")
        End If

        Return decompressedData
    End Function

End Class
