Option Explicit On
Option Strict On

Imports System.IO

''' <summary>
''' Represents a chunk in a png datastream
''' </summary>
Public Class PngChunk
#Region "Declarations"
    Private _length As UInteger = 0
    Private _name As String = String.Empty
    Private _data As MemoryStream = Nothing
    Private _crc As UInteger = 0

    Public Class ChunkNames
        Public Const IHDR As String = "IHDR"
        Public Const IDAT As String = "IDAT"
        Public Const PLTE As String = "PLTE"
        Public Const IEND As String = "IEND"
        Public Const TRNS As String = "tRNS"
    End Class
#End Region

#Region "Constructors"
    Public Sub New( _
            ByVal length As UInteger, _
            ByVal name As String, _
            ByVal data As MemoryStream, _
            ByVal crc As UInteger _
        )
        _length = length
        _name = name
        _data = data
        _crc = crc
    End Sub
#End Region

#Region "Public Properties"
    Public ReadOnly Property Name() As String
        Get
            Return _name
        End Get
    End Property

    Public ReadOnly Property Data() As MemoryStream
        Get
            Return _data
        End Get
    End Property
#End Region

#Region "Public functions"
    Public Shared Function Compare(ByVal chunk1 As PngChunk, ByVal chunk2 As PngChunk) As Boolean
        Dim indicator As Boolean = False

        If chunk1.Name = chunk2.Name _
        And chunk1._crc = chunk2._crc Then
            indicator = True
        End If

        Return indicator
    End Function
#End Region

End Class
