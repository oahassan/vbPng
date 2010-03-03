Option Strict On
Option Explicit On

''' <summary>
''' Defines the values in the header chunk of a png image, which defines how critical 
''' chunks should be read and written.
''' </summary>
Public Class PngHeader
#Region "Declarations"
    Private _Width As UInteger = 0
    Private _Height As UInteger = 0
    Private _BitDepth As PngImage.BitDepths = 0
    Private _ColorType As PngImage.colorTypes = 0
    Private _CompressionMethod As PngImage.CompressionMethods = 0
    Private _FilterMethod As PngImage.FilterMethods = 0
    Private _InterlaceMethod As PngImage.InterlacingMethods = 0
#End Region

#Region "Constructors"
    ''' <summary>
    ''' Instantiates a new PngHeader
    ''' </summary>
    Public Sub New()

    End Sub

    ''' <summary>
    ''' Instantiates a new PngHeader
    ''' </summary>
    ''' <param name="inputWidth">
    ''' Width in pixels
    ''' </param>
    ''' <param name="inputHeight">
    ''' Height in pixels
    ''' </param>
    ''' <param name="inputBitDepth">
    ''' The number of bits in a color or alpha sample
    ''' </param>
    ''' <param name="inputColorType">
    ''' The color type of a pixel
    ''' </param>
    ''' <param name="inputCompressionMethod">
    ''' The compression alogrithm to apply to image data
    ''' </param>
    ''' <param name="inputFilterMethod">
    ''' The filter method to apply to image data
    ''' </param>
    ''' <param name="inputInterlaceMethod">
    ''' The interlacing method to apply to image data
    ''' </param>
    ''' <remarks></remarks>
    Public Sub New( _
        ByVal inputWidth As UInteger, _
        ByVal inputHeight As UInteger, _
        ByVal inputBitDepth As PngImage.BitDepths, _
        ByVal inputColorType As PngImage.colorTypes, _
        ByVal inputCompressionMethod As PngImage.CompressionMethods, _
        ByVal inputFilterMethod As PngImage.FilterMethods, _
        ByVal inputInterlaceMethod As PngImage.InterlacingMethods _
    )
        _Width = inputWidth
        _Height = inputHeight
        _BitDepth = inputBitDepth
        _ColorType = inputColorType
        _CompressionMethod = inputCompressionMethod
        _FilterMethod = inputFilterMethod
        _InterlaceMethod = inputInterlaceMethod
    End Sub

    ''' <summary>
    ''' Creates a new header chunk from a png chunk containg header data
    ''' </summary>
    ''' <param name="ihdrChunk">
    ''' A png chunk containing header data
    ''' </param>
    Public Sub New(ByVal ihdrChunk As PngChunk)
        Dim buffer(3) As Byte
        ihdrChunk.Data.Read(buffer, 0, 4)
        _Width = PngLibUtil.byteArrayToLong(buffer)

        ihdrChunk.Data.Read(buffer, 0, 4)
        _Height = PngLibUtil.byteArrayToLong(buffer)

        _BitDepth = CType(ihdrChunk.Data.ReadByte, PngImage.BitDepths)
        _ColorType = CType(ihdrChunk.Data.ReadByte, PngImage.colorTypes)
        _CompressionMethod = CType(ihdrChunk.Data.ReadByte, PngImage.CompressionMethods)
        _FilterMethod = CType(ihdrChunk.Data.ReadByte, PngImage.FilterMethods)
        _InterlaceMethod = CType(ihdrChunk.Data.ReadByte, PngImage.InterlacingMethods)

        ihdrChunk.Data.Close()
    End Sub
#End Region
    
#Region "Public Properties"
    ''' <summary>
    ''' Width of the image in pixels
    ''' </summary>
    Public ReadOnly Property Width() As Long
        Get
            Return _Width
        End Get
    End Property

    ''' <summary>
    ''' Height of the image in pixels
    ''' </summary>
    Public ReadOnly Property Height() As Long
        Get
            Return _Height
        End Get
    End Property

    ''' <summary>
    ''' Number of bits in a color or alpha sample
    ''' </summary>
    Public ReadOnly Property BitDepth() As Integer
        Get
            Return _BitDepth
        End Get
    End Property

    ''' <summary>
    ''' The color type of a pixel
    ''' </summary>
    Public ReadOnly Property ColorType() As PngImage.colorTypes
        Get
            Return _ColorType
        End Get
    End Property

    ''' <summary>
    ''' The compression alogrithm to apply to image data
    ''' </summary>
    Public ReadOnly Property CompressionMethod() As PngImage.CompressionMethods
        Get
            Return _CompressionMethod
        End Get
    End Property

    ''' <summary>
    ''' The filter method to apply to image data
    ''' </summary>
    Public ReadOnly Property FilterMethod() As PngImage.FilterMethods
        Get
            Return _FilterMethod
        End Get
    End Property

    ''' <summary>
    ''' The interlacing method to apply to image data
    ''' </summary>
    Public ReadOnly Property InterlaceMethod() As PngImage.InterlacingMethods
        Get
            Return _InterlaceMethod
        End Get
    End Property
#End Region

End Class
