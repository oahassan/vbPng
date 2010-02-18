Option Explicit On
Option Strict On

''' <summary>
''' represents the color samples
''' </summary>
Public Class PngColor

#Region "Declarations"
    Dim _ColorSamples As Byte() = Nothing
    Dim _AlphaSample As Byte() = Nothing
#End Region

#Region "Constructors"
    Public Sub New(ByVal colorSamples() As Byte, ByVal alphaSample() As Byte)
        _ColorSamples = colorSamples
        _AlphaSample = alphaSample
    End Sub
#End Region

#Region "Public Properties"
    Public ReadOnly Property ColorSamples() As Byte()
        Get
            Return _ColorSamples
        End Get
    End Property

    Public ReadOnly Property AlphaSample() As Byte()
        Get
            Return _AlphaSample
        End Get
    End Property

    Public ReadOnly Property R() As Byte()
        Get
            Dim samples As Byte() = Nothing

            Select Case _ColorSamples.Length
                Case 3
                    samples = New Byte() {_ColorSamples(0)}
                Case 6
                    samples = New Byte() {_ColorSamples(0), _ColorSamples(1)}
                Case Else
                    Throw New Exception("Invalid sample size!")
            End Select

            Return samples
        End Get
    End Property

    Public ReadOnly Property G() As Byte()
        Get
            Dim samples As Byte() = Nothing

            Select Case _ColorSamples.Length
                Case 3
                    samples = New Byte() {_ColorSamples(1)}
                Case 6
                    samples = New Byte() {_ColorSamples(2), _ColorSamples(3)}
                Case Else
                    Throw New Exception("Invalid sample size!")
            End Select

            Return samples
        End Get
    End Property

    Public ReadOnly Property B() As Byte()
        Get
            Dim samples As Byte() = Nothing

            Select Case _ColorSamples.Length
                Case 3
                    samples = New Byte() {_ColorSamples(2)}
                Case 6
                    samples = New Byte() {_ColorSamples(4), _ColorSamples(5)}
                Case Else
                    Throw New Exception("Invalid sample size!")
            End Select

            Return samples
        End Get
    End Property
#End Region

#Region "Public Functions"
    Public Function Clone() As PngColor
        Dim alpha As Byte() = Nothing

        If _AlphaSample Is Nothing Then

        Else
            alpha = CType(_AlphaSample.Clone, Byte())
        End If

        Return New PngColor(CType(_ColorSamples.Clone, Byte()), alpha)
    End Function

    Public Shared Function compare(ByVal color1 As PngColor, ByVal color2 As PngColor) As Boolean
        Dim indicator As Boolean = False

        If CompareAlphaSamples(color1, color2) _
        AndAlso CompareColorSamples(color1, color2) Then
            indicator = True
        End If

        Return indicator
    End Function

    Public Shared Function CompareColorSamples( _
        ByVal color1 As PngColor, _
        ByVal color2 As PngColor _
    ) As Boolean
        Return PngLibUtil.compareByteArrays(color1.ColorSamples, color2.ColorSamples)
    End Function

    Public Shared Function CompareAlphaSamples( _
        ByVal color1 As PngColor, _
        ByVal color2 As PngColor _
    ) As Boolean
        Return PngLibUtil.compareByteArrays(color1.AlphaSample, color2.AlphaSample)
    End Function

    Public Function IsOpaque() As Boolean
        Dim indicator As Boolean = False

        If _AlphaSample Is Nothing Then
            indicator = True
        ElseIf _AlphaSample.Length = 1 Then
            If _AlphaSample(0) = 255 Then
                indicator = True
            End If
        ElseIf _AlphaSample.Length = 2 Then
            If _AlphaSample(0) = 255 _
            AndAlso AlphaSample(1) = 255 Then
                indicator = True
            End If
        End If

        Return indicator
    End Function
#End Region

#Region "Friend Functions"
    Friend Sub setAlpha(ByVal alpha() As Byte)
        _AlphaSample = alpha
    End Sub
#End Region
End Class
