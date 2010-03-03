Option Explicit On
Option Strict On

''' <summary>
''' Encapsulates the color of a PngPixel
''' </summary>
''' <remarks>
''' Bit depths samller than a byte in size are expanded to a byte.  Greyscale values
''' are represented by giving the R, G, and B samples the same values.
''' </remarks>
Public Class PngColor

#Region "Declarations"
    Private _R As Byte() = Nothing
    Private _G As Byte() = Nothing
    Private _B As Byte() = Nothing
    Private _Alpha As Byte() = Nothing
#End Region

#Region "Constructors"
    ''' <summary>
    ''' Instantiates a new png color
    ''' </summary>
    ''' <param name="R">
    ''' Samples of the red channel
    ''' </param>
    ''' <param name="G">
    ''' Samples of the green channel
    ''' </param>
    ''' <param name="B">
    ''' Samples of the blue channel
    ''' </param>
    ''' <param name="alpha">
    ''' Samples of the alpha channel
    ''' </param>
    ''' <remarks>
    ''' Each sample must be 1 or 2 bytes in size
    ''' </remarks>
    Public Sub New( _
        ByVal R As Byte(), _
        ByVal G As Byte(), _
        ByVal B As Byte(), _
        Optional ByVal alpha As Byte() = Nothing _
    )
        _R = R
        _G = G
        _B = B
        _Alpha = alpha
    End Sub
#End Region

#Region "Public Properties"
    ''' <summary>
    ''' Returns an array of bytes for the samples of each color channel.  Samples
    ''' come in R, G, B, Alpha order.
    ''' </summary>
    Public ReadOnly Property Samples() As Byte()
        Get
            Dim sampleList As New List(Of Byte)
            sampleList.AddRange(_R)
            sampleList.AddRange(_G)
            sampleList.AddRange(_B)

            If Alpha Is Nothing = False Then
                sampleList.AddRange(_Alpha)
            End If

            Return sampleList.ToArray
        End Get
    End Property

    ''' <summary>
    ''' Bytes of red channel samples
    ''' </summary>
    Public ReadOnly Property R() As Byte()
        Get
            Return _R
        End Get
    End Property

    ''' <summary>
    ''' Bytes of green channel samples
    ''' </summary>
    Public ReadOnly Property G() As Byte()
        Get
            Return _G
        End Get
    End Property

    ''' <summary>
    ''' Bytes of blue channel samples
    ''' </summary>
    Public ReadOnly Property B() As Byte()
        Get
            Return _B
        End Get
    End Property

    ''' <summary>
    ''' Bytes of alpha channel samples
    ''' </summary>
    Public ReadOnly Property Alpha() As Byte()
        Get
            Return _Alpha
        End Get
    End Property
#End Region

#Region "Public Functions"
    ''' <summary>
    ''' Creates a deep copy of a PngColor
    ''' </summary>
    ''' <returns>
    ''' A new PngColor
    ''' </returns>
    Public Function Clone() As PngColor
        Dim alpha As Byte() = Nothing

        If alpha Is Nothing = False Then
            alpha = CType(_Alpha.Clone, Byte())
        End If

        Dim R As Byte() = CType(_R.Clone, Byte())
        Dim G As Byte() = CType(_G.Clone, Byte())
        Dim B As Byte() = CType(_B.Clone, Byte())

        Return New PngColor(R, G, B, alpha)
    End Function

    ''' <summary>
    ''' Compares two PngColors based on their color samples and alpha samples
    ''' </summary>
    ''' <param name="color1">
    ''' First PngColor to compare
    ''' </param>
    ''' <param name="color2">
    ''' Second PncColor to compare
    ''' </param>
    ''' <returns>
    ''' Boolean value indicating the equivalence of two PngColors
    ''' </returns>
    Public Shared Function Compare(ByVal color1 As PngColor, ByVal color2 As PngColor) As Boolean
        Dim indicator As Boolean = False

        If CompareAlphas(color1, color2) _
        AndAlso CompareColorSamples(color1, color2) Then
            indicator = True
        End If

        Return indicator
    End Function

    ''' <summary>
    ''' Compares the color samples of two PngColors
    ''' </summary>
    ''' <param name="color1">
    ''' First PngColor to compare
    ''' </param>
    ''' <param name="color2">
    ''' Second PngColor to compare
    ''' </param>
    ''' <returns>
    ''' A boolean value indicating the equivalence of the color samples of two PngColors
    ''' </returns>
    Public Shared Function CompareColorSamples( _
        ByVal color1 As PngColor, _
        ByVal color2 As PngColor _
    ) As Boolean
        Return PngLibUtil.compareByteArrays(color1.R, color2.R) _
        AndAlso PngLibUtil.compareByteArrays(color1.G, color2.G) _
        AndAlso PngLibUtil.compareByteArrays(color1.B, color2.B)
    End Function

    ''' <summary>
    ''' Compares the alpha samples of two PngColors
    ''' </summary>
    ''' <param name="color1">
    ''' First PngColor to compare
    ''' </param>
    ''' <param name="color2">
    ''' Second PngColor to compare
    ''' </param>
    ''' <returns>
    ''' A boolean value indicating the equivalence of two PngColors
    ''' </returns>
    ''' <remarks></remarks>
    Public Shared Function CompareAlphas( _
        ByVal color1 As PngColor, _
        ByVal color2 As PngColor _
    ) As Boolean
        Return PngLibUtil.compareByteArrays(color1.Alpha, color2.Alpha)
    End Function

    ''' <summary>
    ''' Indicates if a PngColor is opaque
    ''' </summary>
    Public Function IsOpaque() As Boolean
        Dim indicator As Boolean = False

        If Me.Alpha Is Nothing Then
            indicator = True
        ElseIf Me.Alpha.Length = 1 Then
            If Me.Alpha(0) = 255 Then
                indicator = True
            End If
        ElseIf Me.Alpha.Length = 2 Then
            If Me.Alpha(0) = 255 _
            AndAlso Me.Alpha(1) = 255 Then
                indicator = True
            End If
        End If

        Return indicator
    End Function

    ''' <summary>
    ''' Sets the alpha value of a PngColor
    ''' </summary>
    ''' <param name="alpha">
    ''' The bytes for an alpha channel to give to a PngColor
    ''' </param>
    Public Sub setAlpha(ByVal alpha() As Byte)
        Dim sampleCount As Integer = R.Length

        Select Case sampleCount
            Case 1
                If alpha.Length <> 1 Then
                    Throw New ArgumentOutOfRangeException("Invalid alpha sample")
                End If

            Case 2
                If alpha.Length <> 2 Then
                    Throw New ArgumentOutOfRangeException("Invalid alpha sample")
                End If

            Case Else
                Throw New ArgumentOutOfRangeException("Invalid Sample Count")
        End Select

        _Alpha = alpha
    End Sub

#End Region
End Class
