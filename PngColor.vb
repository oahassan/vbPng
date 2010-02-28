Option Explicit On
Option Strict On

''' <summary>
''' represents the color samples
''' </summary>
Public Class PngColor

#Region "Declarations"
    Private _R As Byte() = Nothing
    Private _G As Byte() = Nothing
    Private _B As Byte() = Nothing
    Private _Alpha As Byte() = Nothing
#End Region

#Region "Constructors"
    Public Sub New( _
        ByVal R As Byte(), _
        ByVal G As Byte(), _
        ByVal B As Byte(), _
        ByVal Alpha As Byte() _
    )
        _R = R
        _G = G
        _B = B
        _Alpha = Alpha
    End Sub
#End Region

#Region "Public Properties"
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

    Public ReadOnly Property R() As Byte()
        Get
            Return _R
        End Get
    End Property

    Public ReadOnly Property G() As Byte()
        Get
            Return _G
        End Get
    End Property

    Public ReadOnly Property B() As Byte()
        Get
            Return _B
        End Get
    End Property

    Public ReadOnly Property Alpha() As Byte()
        Get
            Return _Alpha
        End Get
    End Property
#End Region

#Region "Public Functions"
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

    Public Shared Function compare(ByVal color1 As PngColor, ByVal color2 As PngColor) As Boolean
        Dim indicator As Boolean = False

        If CompareAlphas(color1, color2) _
        AndAlso CompareColorSamples(color1, color2) Then
            indicator = True
        End If

        Return indicator
    End Function

    Public Shared Function CompareColorSamples( _
        ByVal color1 As PngColor, _
        ByVal color2 As PngColor _
    ) As Boolean
        Return PngLibUtil.compareByteArrays(color1.R, color2.R) _
        AndAlso PngLibUtil.compareByteArrays(color1.G, color2.G) _
        AndAlso PngLibUtil.compareByteArrays(color1.B, color2.B)
    End Function

    Public Shared Function CompareAlphas( _
        ByVal color1 As PngColor, _
        ByVal color2 As PngColor _
    ) As Boolean
        Return PngLibUtil.compareByteArrays(color1.Alpha, color2.Alpha)
    End Function

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
