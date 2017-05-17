Module Ejercicio4_2
    Sub main()
        Dim cod As Integer
        Dim cant As Integer
        Dim totalG As Single
        Dim codigo(4) As Integer
        codigo(0) = 1
        codigo(1) = 2
        codigo(2) = 3
        codigo(3) = 4
        codigo(4) = 5
        Dim descripcion(4) As String
        descripcion(0) = "Guayaba"
        descripcion(1) = "Totin"
        descripcion(2) = "Prity"
        descripcion(3) = "Queso"
        descripcion(4) = "Pan"
        Dim precio(4) As Single
        precio(0) = 20.35
        precio(1) = 10.5
        precio(2) = 12.3
        precio(3) = 15
        precio(4) = 7.9

        Do
            Console.WriteLine("Ingrese Codigo de Producto o 0 para Salir")
            cod = Console.ReadLine
            If codigo.Contains(cod) Then
                Console.WriteLine("El codigo es {0} la descripcion es '{1}' el precio unitario es ${2}", codigo(Array.IndexOf(codigo, cod)), descripcion(Array.IndexOf(codigo, cod)), precio(Array.IndexOf(codigo, cod)))
                Console.WriteLine("Ingrese Cantidad de Producto ")
                cant = Console.ReadLine()
                Console.WriteLine("Total del Producto {0}", cant * precio(Array.IndexOf(codigo, cod)))
                totalG = totalG + cant * precio(Array.IndexOf(codigo, cod))
                Console.WriteLine("Total General {0}", totalG)
            End If
        Loop While cod
        Console.WriteLine("Gracias Vuelvas Prontos")
        Console.ReadKey()
    End Sub

End Module
