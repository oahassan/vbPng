Option Strict On
Option Explicit On

Imports System.IO

''' <summary>
''' Ported from RFC 1591 sample code
''' </summary>
''' <remarks></remarks>
Public Class Adler32
    Private Const BASE As UShort = 65521 '/* largest prime smaller than 65536 */
    '/*
    'Update a running Adler-32 checksum with the bytes buf[0..len-1]
    'and return the updated checksum. The Adler-32 checksum should be
    'initialized to 1.
    'Usage example:
    'unsigned long adler = 1L;
    'while (read buffer(buffer, length) != EOF) f
    'adler = update adler32(adler, buffer, length);
    '
    'if (adler != original adler) error();
    '*/
    Private Shared Function updateAdler32(ByVal adler As UInteger, ByVal buffer As Stream) As UInteger

        Dim s1 As UInteger = adler And &HFFFFUI
        Dim s2 As UInteger = (adler >> 16) And &HFFFFUI

        Dim byteBuffer(0) As Byte
        Dim numberRead As Integer = buffer.Read(byteBuffer, 0, 1)

        While numberRead <> 0
            'Cast in modular arithmetic to avoid overflows
            s1 = CUInt((CLng(s1) + CLng(byteBuffer(0))) Mod BASE)
            s2 = CUInt((CLng(s2) + CLng(s1)) Mod BASE)
            numberRead = buffer.Read(byteBuffer, 0, 1)
        End While

        Return (s2 << 16) + s1
    End Function

    '/* Return the adler32 of the bytes buf[0..len-1] */
    Public Shared Function adler32(ByVal buffer As Stream) As UInteger

        Return updateAdler32(1UI, buffer)
    End Function
End Class
