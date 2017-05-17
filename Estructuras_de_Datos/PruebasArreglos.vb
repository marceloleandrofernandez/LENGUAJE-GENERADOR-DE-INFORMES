Module PruebasArreglos
    Sub main()
        Dim cod As Integer

        Dim codigo(4) As Integer
        codigo(0) = 1
        codigo(1) = 2
        codigo(2) = 3
        codigo(3) = 4
        codigo(4) = 5

        For Each item In codigo
            'Console.WriteLine(item)

        Next
        Do
            Console.WriteLine("Ingresar Codigo")
            cod = Console.ReadLine
            Console.WriteLine("El Indice es {0}", Array.IndexOf(codigo, cod))
        Loop While cod

        Console.ReadKey()
    End Sub
End Module
