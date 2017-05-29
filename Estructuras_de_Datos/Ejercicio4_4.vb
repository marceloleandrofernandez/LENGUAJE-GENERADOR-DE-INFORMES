Module Ejercicio4_4
    Sub main()
        Dim paises As New Collection
        Dim opc As String
        Dim nombre As String
        Dim dominio As String
        paises.Add("Argentina", "ar")
        paises.Add("Peru", "pe")
        paises.Add("Paraguay", "py")
        paises.Add("Chile", "cl")
        Do
            Console.WriteLine(" 1-Muestra Todos 2-Buscar 3-Agregar 4-Editar 5-Eliminar ")
            opc = Console.ReadLine()
            Select Case opc
                Case 1
                    muestratodo(paises)
                Case 2
                    Do
                        Console.WriteLine("Ingrese Dominio: ")
                        dominio = Console.ReadLine
                    Loop Until valida(dominio)
                    busca(paises, dominio)
                Case 3
                    Do
                        Console.WriteLine("Ingrese Dominio: ")
                        dominio = Console.ReadLine
                        If paises.Contains(dominio) Then
                            Console.WriteLine("El dominio ya existe")
                        End If
                    Loop Until valida(dominio) And Not paises.Contains(dominio)
                    Do
                        Console.WriteLine("Ingrese Pais: ")
                        nombre = Console.ReadLine
                    Loop Until valida(dominio)
                    agrega(paises, nombre, dominio)
                    Console.WriteLine("Se Agrego el Pais {0} con el dominio {1}", paises(dominio), dominio)
                Case 4
                    Do
                        Console.WriteLine("Ingrese Dominio: ")
                        dominio = Console.ReadLine
                    Loop Until valida(dominio)
                    If paises.Contains(dominio) Then
                        Do
                            Console.WriteLine("Ingrese Pais: ")
                            nombre = Console.ReadLine
                        Loop Until valida(nombre)
                        elimina(paises, dominio)
                        agrega(paises, nombre, dominio)
                    Else
                        busca(paises, dominio)
                    End If
                Case 5
                    Do
                        Console.WriteLine("Ingrese Dominio: ")
                        dominio = Console.ReadLine
                    Loop Until valida(dominio)
                    If paises.Contains(dominio) Then
                        Console.WriteLine("El dominio {0} del pais {1} ah sido eliminado ", dominio, paises(dominio))
                        elimina(paises, dominio)
                    End If

                Case Else
                    Console.WriteLine("Opcion no Valida")
            End Select
        Loop While valida(opc)

    End Sub

    Private Function valida(cadena As String) As Boolean
        If cadena <> "" Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Sub busca(paises As Collection, dominio As String)
        If paises.Contains(dominio) Then
            Console.WriteLine("Pais: {0} Dominio: {1}", paises.Item(dominio), dominio)
        Else
            Console.WriteLine("No existe Pais para el Dominio Solicitado")
        End If
    End Sub
    Private Sub muestratodo(paises As Collection)
        For Each pais In paises
            Console.WriteLine(pais)
        Next
        Console.WriteLine("Cantidad de Dominios {0}", paises.Count)
    End Sub
    Private Sub agrega(ByRef paises As Collection, nombre As String, dominio As String)
        paises.Add(nombre, dominio)
    End Sub
    Private Sub elimina(ByRef paises As Collection, dominio As String)
        paises.Remove(dominio)
    End Sub
End Module
