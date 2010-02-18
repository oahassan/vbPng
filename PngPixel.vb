Public Class PngPixel
#Region "Declarations"
    Private _PaletteIndex As Integer = -1
    Private _Color As PngColor = Nothing
#End Region

#Region "Constructors"
    Public Sub New(ByVal paletteIndex As Integer)
        _PaletteIndex = paletteIndex
    End Sub

    Public Sub New(ByVal color As PngColor)
        _Color = color
    End Sub
#End Region

#Region "Public Properties"
    Public ReadOnly Property Color() As PngColor
        Get
            Return _Color
        End Get
    End Property

    Public ReadOnly Property PaletteIndex() As Integer
        Get
            Return _PaletteIndex
        End Get
    End Property
#End Region

#Region "Public Methods"
    Public Sub ReplaceColorWithPaletteEntry(ByVal entryIndex As Integer)
        Me._Color = Nothing
        Me._PaletteIndex = entryIndex
    End Sub
#End Region
End Class
