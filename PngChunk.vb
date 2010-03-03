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

    ''' <summary>
    ''' Names of supported chunks
    ''' </summary>
    Public Class ChunkNames
        Public Const IHDR As String = "IHDR"
        Public Const IDAT As String = "IDAT"
        Public Const PLTE As String = "PLTE"
        Public Const IEND As String = "IEND"
        Public Const tRNS As String = "tRNS"
        Public Const cHRM As String = "cHRM"
        Public Const gAMA As String = "gAMA"
        Public Const iCCP As String = "iCCP"
        Public Const sBIT As String = "sBIT"
        Public Const sRGB As String = "sRGB"
        Public Const bKGD As String = "bKGD"
        Public Const hIST As String = "hIST"
        Public Const pHYs As String = "pHYs"
        Public Const sPLT As String = "sPLT"
        Public Const tIME As String = "tIME"
        Public Const iTXt As String = "iTXt"
        Public Const tEXt As String = "tEXt"
        Public Const zTXt As String = "zTXt"
    End Class

    Friend Shared _KnownChunks As New List(Of String)(New String() { _
        "IHDR", _
        "PLTE", _
        "IDAT", _
        "IEND", _
        "cHRM", _
        "gAMA", _
        "iCCP", _
        "sBIT", _
        "sRGB", _
        "pHYs", _
        "sPLT", _
        "tIME", _
        "iTXt", _
        "tEXt", _
        "zTXt", _
        "bKGD", _
        "hIST", _
        "tRNS" _
    })

    Friend Shared _PrePLTEChunks As New List(Of String)(New String() { _
        "cHRM", _
        "gAMA", _
        "iCCP", _
        "sBIT", _
        "sRGB", _
        "pHYs", _
        "sPLT", _
        "tIME", _
        "iTXt", _
        "tEXt", _
        "zTXt" _
    })

    Friend Shared _PostPLTEChunks As New List(Of String)(New String() { _
        "bKGD", _
        "hIST", _
        "tRNS" _
    })
#End Region

#Region "Constructors"
    ''' <summary>
    ''' Instantiates a new PngChunk
    ''' </summary>
    ''' <param name="length">
    ''' length of the chunk data
    ''' </param>
    ''' <param name="name">
    ''' four character name of the chunk
    ''' </param>
    ''' <param name="data">
    ''' binary data of the chunk
    ''' </param>
    ''' <param name="crc">
    ''' The cyclic redundancy
    ''' </param>
    ''' <remarks></remarks>
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
    ''' <summary>
    ''' The four character name of a chunk
    ''' </summary>
    Public ReadOnly Property Name() As String
        Get
            Return _name
        End Get
    End Property

    ''' <summary>
    ''' The binary data of a chunk
    ''' </summary>
    Public ReadOnly Property Data() As MemoryStream
        Get
            Return _data
        End Get
    End Property
#End Region

#Region "Public functions"
    ''' <summary>
    ''' Compares two chunks on their name and CRC value
    ''' </summary>
    ''' <param name="chunk1">
    ''' first chunk to compare
    ''' </param>
    ''' <param name="chunk2">
    ''' second chunk to compare
    ''' </param>
    ''' <returns>
    ''' A boolean value indicating the equivalence of two chunks
    ''' </returns>
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
