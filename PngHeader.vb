Option Strict On
Option Explicit On

Public Class PngHeader
#Region "Declarations"
    Private _Width As Long = 0
    Private _Height As Long = 0
    Private _BitDepth As Integer = 0
    Private _ColorType As Integer = 0
    Private _CompressionMethod As Integer = 0
    Private _FilterMethod As Integer = 0
    Private _InterlaceMethod As Integer = 0
#End Region

#Region "Constructors"
    Public Sub New()

    End Sub

    Public Sub New( _
        ByVal inputWidth As Long, _
        ByVal inputHeight As Long, _
        ByVal inputBitDepth As Integer, _
        ByVal inputColorType As Integer, _
        ByVal inputCompressionMethod As Integer, _
        ByVal inputFilterMethod As Integer, _
        ByVal inputInterlaceMethod As Integer _
    )
        _Width = inputWidth
        _Height = inputHeight
        _BitDepth = inputBitDepth
        _ColorType = inputColorType
        _CompressionMethod = inputCompressionMethod
        _FilterMethod = inputFilterMethod
        _InterlaceMethod = inputInterlaceMethod
    End Sub

    Public Sub New(ByVal ihdrChunk As PngChunk)
        Dim buffer(3) As Byte
        ihdrChunk.Data.Read(buffer, 0, 4)
        _Width = PngLibUtil.byteArrayToLong(buffer)

        ihdrChunk.Data.Read(buffer, 0, 4)
        _Height = PngLibUtil.byteArrayToLong(buffer)

        _BitDepth = ihdrChunk.Data.ReadByte
        _ColorType = ihdrChunk.Data.ReadByte
        _CompressionMethod = ihdrChunk.Data.ReadByte
        _FilterMethod = ihdrChunk.Data.ReadByte
        _InterlaceMethod = ihdrChunk.Data.ReadByte

        ihdrChunk.Data.Close()
    End Sub
#End Region

#Region "Public Properties"
    Public ReadOnly Property Width() As Long
        Get
            Return _Width
        End Get
    End Property

    Public ReadOnly Property Height() As Long
        Get
            Return _Height
        End Get
    End Property

    Public ReadOnly Property BitDepth() As Integer
        Get
            Return _BitDepth
        End Get
    End Property

    Public ReadOnly Property ColorType() As Integer
        Get
            Return _ColorType
        End Get
    End Property

    Public ReadOnly Property CompressionMethod() As Integer
        Get
            Return _CompressionMethod
        End Get
    End Property

    Public ReadOnly Property FilterMethod() As Integer
        Get
            Return _FilterMethod
        End Get
    End Property

    Public ReadOnly Property InterlaceMethod() As Integer
        Get
            Return _InterlaceMethod
        End Get
    End Property
#End Region

End Class
