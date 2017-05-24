Module Ejercicio4_3
    Sub main()

        Dim codnota(1, 1) As Single
        Dim codnombre(1) As String
        Dim cantalu As Integer
        Dim cantnotas As Integer
        Dim nom As String
        Dim nota As Integer
        Do
            Console.WriteLine("Ingrese Cantidad de Alumnos ")
            cantalu = Console.ReadLine
        Loop Until cantidades(cantalu, 40)
        Do
            Console.WriteLine("Ingrese Cantidad de Notas")
            cantnotas = Console.ReadLine
        Loop Until cantidades(cantnotas, 4)
        ReDim codnombre(cantalu)
        ReDim codnota(cantalu, cantnotas)

        For x = 0 To cantalu - 1
            Do
                Console.WriteLine("Ingrese Nombre Alumno: ")
                nom = Console.ReadLine
            Loop Until Not validarNombre(nom, codnombre)
            codnombre(x) = nom
            For y = 0 To cantnotas - 1
                Do
                    Console.WriteLine("Ingrese Nota {0}", y)
                    nota = Console.ReadLine
                Loop Until validarNotas(nota)
                codnota(x, y) = nota
            Next
        Next
        Console.ReadKey()



        For x = 0 To cantalu - 1
            Console.WriteLine("Alumno: {0}", codnombre(x))
            Dim aux = 0
            For y = 0 To cantnotas - 1

                Console.WriteLine(codnota(x, y))
                aux += codnota(x, y)
            Next
            Console.WriteLine("Su promedio es = {0}", promedio(aux, cantnotas))
            estado(promedio(aux, cantnotas))
        Next
        Console.ReadKey()

    End Sub
    Private Function cantidades(valor, rango) As Boolean
        If valor > 0 And valor <= rango Then
            Return True
        Else
            Return False
        End If
    End Function
    Private Function validarNotas(nota) As Integer
        If nota >= 0 And nota <= 10 Then
            Return True
        Else
            Return False
        End If
    End Function
    Private Function validarNombre(nom As String, codnombre As Array) As Boolean
        For Each item In codnombre
            If item = nom Then
                Return True
            End If
        Next
        Return False
    End Function
    Private Function promedio(notas, cantnotas) As Single
        Return notas / cantnotas
    End Function
    Private Sub estado(promedio)
        If promedio > 5 And promedio <= 10 Then
            Console.WriteLine("Su estado es Aprobado")
        Else
            Console.WriteLine("Su Estado es No Aprobado")
        End If
    End Sub
End Module
