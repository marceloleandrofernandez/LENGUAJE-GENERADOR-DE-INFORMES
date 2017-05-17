Module Ejercicio4_1

    Sub Main()
        Dim arreglo(4) As Integer
        Dim media As Single
        For i = 0 To 4
            arreglo(i) = Console.ReadLine()
            media += arreglo(i)
        Next
        media = media / arreglo.Count
        Console.WriteLine("La Media de Los elementos es {0}", media)
        For Each item In arreglo
            Console.WriteLine("La Desviacion de {0} es = {1}", item, item - media)
        Next

        Console.ReadKey()
    End Sub

End Module
