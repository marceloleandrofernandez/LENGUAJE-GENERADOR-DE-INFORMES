Module PruebasColecciones
    Sub main()
        Dim codnota(1, 3) As Single
        Dim codnombre As New Collection
        Dim cantnotas As Integer = 3
        codnota(0, 0) = 7
        codnota(0, 1) = 8
        codnota(0, 2) = 6
        codnota(0, 3) = 5

        codnota(1, 0) = 7
        codnota(1, 1) = 8
        codnota(1, 2) = 5
        codnota(1, 3) = 6

        codnombre.Add("Marcelo", CStr(0))
        codnombre.Add("Ramon", CStr(1))
        For Each item In codnombre
            Console.WriteLine(item)
            For x = 0 To cantnotas
                ' Console.WriteLine("Nota {0}-{1}", codnota(x + 1), codnombre(item))
            Next
        Next
        Console.WriteLine(codnombre(1))

        Console.ReadKey()
    End Sub
End Module
