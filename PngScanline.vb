Option Strict On
Option Explicit On

Public Class PngScanline
#Region "Declarations"
    Private _Filter() As Byte = Nothing
    Private _Pixels As New List(Of PngPixel)
#End Region

#Region "Constructors"
    Public Sub New(ByVal filter() As Byte, ByVal pixels As List(Of PngPixel))
        _Filter = filter
        _Pixels = pixels
    End Sub
#End Region

#Region "Public Properties"
    Public ReadOnly Property Filter() As Byte()
        Get
            Return _Filter
        End Get
    End Property

    Public ReadOnly Property Pixels() As List(Of PngPixel)
        Get
            Return _Pixels
        End Get
    End Property
#End Region

#Region "Filtering"
    
#End Region
End Class
