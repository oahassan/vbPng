Option Strict On
Option Explicit On

Public Class PngScanline
#Region "Declarations"
    ''' <summary>
    ''' Types of filters defined for filter method 0
    ''' </summary>
    ''' <remarks></remarks>
    Public Enum FilterTypes
        NoFilter = 0
        SubFilter = 1
        UpFilter = 2
        AverageFilter = 3
        PaethFilter = 4
    End Enum

    Private _Filter As FilterTypes = Nothing
    Private _Pixels As New List(Of PngPixel)
#End Region

#Region "Constructors"
    ''' <summary>
    ''' Instantiates a new png scanline
    ''' </summary>
    ''' <param name="filter">
    ''' Type of filter to apply to scanline bytes when reading and writing the PNG
    ''' </param>
    ''' <param name="pixels">
    ''' List of pixels in this scanline
    ''' </param>
    ''' <remarks></remarks>
    Public Sub New(ByVal filter As FilterTypes, ByVal pixels As List(Of PngPixel))
        _Filter = filter
        _Pixels = pixels
    End Sub
#End Region

#Region "Public Properties"
    ''' <summary>
    ''' Type of filter to apply to scanline bytes when reading and writing the PNG
    ''' </summary>
    Public ReadOnly Property Filter() As FilterTypes
        Get
            Return _Filter
        End Get
    End Property

    ''' <summary>
    ''' List of pixels in this scanline
    ''' </summary>
    Public ReadOnly Property Pixels() As List(Of PngPixel)
        Get
            Return _Pixels
        End Get
    End Property
#End Region

#Region "Filtering"
    ''' <summary>
    ''' Sets the filter of this scanline
    ''' </summary>
    ''' <param name="filter">
    ''' Type of filter to apply to scanline bytes when reading and writing the PNG
    ''' </param>
    ''' <remarks></remarks>
    Public Sub setFilterBytes(ByVal filter As FilterTypes)
        _Filter = filter
    End Sub
#End Region
End Class
