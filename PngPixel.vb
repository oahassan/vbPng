Public Class PngPixel
#Region "Declarations"
    Private _PaletteIndex As Integer = -1
    Private _Color As PngColor = Nothing
#End Region

#Region "Constructors"
    ''' <summary>
    ''' Instantiates a new PngPixel with the given palette index
    ''' </summary>
    ''' <param name="paletteIndex">
    ''' Entry index in a PngPalette of this PngPixel's color
    ''' </param>
    ''' <remarks></remarks>
    Public Sub New(ByVal paletteIndex As Integer)
        _PaletteIndex = paletteIndex
    End Sub

    ''' <summary>
    ''' Instantiates a new PngPixel with a PngColor
    ''' </summary>
    ''' <param name="color">
    ''' PngColor for this PngPixel
    ''' </param>
    Public Sub New(ByVal color As PngColor)
        _Color = color
    End Sub
#End Region

#Region "Public Properties"
    ''' <summary>
    ''' The PngColor of this PngPixel
    ''' </summary>
    Public ReadOnly Property Color() As PngColor
        Get
            Return _Color
        End Get
    End Property

    ''' <summary>
    ''' The PngPalette entry index of this PngPixel's PngColor
    ''' </summary>
    Public ReadOnly Property PaletteIndex() As Integer
        Get
            Return _PaletteIndex
        End Get
    End Property
#End Region

#Region "Friend Methods"
    Friend Sub ReplaceColorWithPaletteEntry(ByVal entryIndex As Integer)
        Me._Color = Nothing
        Me._PaletteIndex = entryIndex
    End Sub
#End Region
End Class
