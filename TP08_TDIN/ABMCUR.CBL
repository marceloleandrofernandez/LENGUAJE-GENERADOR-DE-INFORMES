      *-- TRABAJO PRACTICO Nº1 EJEMPLO DE A-B-M --**
      *-- EN ESTA DIVISION SE IDENTIFICA EL PGM "MODABM" --**
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODABM.
      ****
      *-- DIVISION DE ENTORNO --*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *-- PARA CAMBIAR LA COMA POR PUNTO --*
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
      *-- SE DEFINEN LAS BASES DE DATOS, DONDE ESTA, --*
      *-- ORGANIZACION, COMO VOY A ACCEDER --**
      *-- SI TIENE CLAVE CUAL ES Y FILE STATUS --**
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ****  MAESTRO DE EMPLEADOS    **********************

           SELECT MAESTRO ASSIGN TO DISK "MAEEMP.IND"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS MCA
           ALTERNATE RECORD KEY IS MAN DUPLICATES
           FILE STATUS IS FST.

      ****  MAESTRO DE LOCALIDAD   **********************

           SELECT MAELOC ASSIGN TO DISK "LOCALIDAD.IND"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS MLOC
           ALTERNATE RECORD KEY IS MLDESC DUPLICATES
           FILE STATUS IS FSTLOC.

       DATA DIVISION.
       FILE SECTION.
        FD MAESTRO
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 44 CHARACTERS
           DATA RECORD IS R-MAE.

         01 R-MAE.
            02 MCA  PIC 9(08).
            02 MAN  PIC X(30).
            02 MSB  PIC 9(05).
            02 MLO  PIC 9(1).
        
        FD MAELOC
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 16 CHARACTERS
           DATA RECORD IS R-LOC.

         01 R-LOC.
            02 MLOC  PIC 9(1).
            02 MLDESC  PIC X(15).

      *-- AREA DE TRABAJO SE DEFINEN LAS VARIABLES Y CONSTANTES --*
      *--- NIVEL 77 O NIVEL 01 --*
       WORKING-STORAGE SECTION.
      *--- VARIABLES QUE VOY A USAR EN EL PGM --*
       77 FST                     PIC XX.
       77 FSTLOC                  PIC XX.
       77 L-TIT                   PIC X(20).
       77 C-T                     PIC 99.
       77 TECLA                   PIC X.
       77 OP                      PIC 9  VALUE 0.
       77 IND                     PIC 99 VALUE 0.
      *--- RAYA SIMPLE
      *77 SUBRAYADO               PIC X(80) VALUE ALL "-".
      *--- RAYA DOBLE
       77 SUBRAYADO               PIC X(80) VALUE ALL "Í".
       77 PRI                     PIC X VALUE "P".
       77 LINEA                   PIC X(80) VALUE SPACES.
       77 INCIDENCIA-P            PIC X(30).
       77 CONFIRMA                PIC X(30).
       77 VAL                     PIC 99.
      *---- reemplazo la variable OPCION POR OP-EMC.
       77 OP-EMC                  PIC X.
      *---- reemplazo la variable DATOS POR OP-SN.
       77 OP-SN                   PIC X.
      *---- reemplazo la variable MAESTRO POR WMAE.
       77 WMAE                    PIC XX VALUE SPACES.
      *---- reemplazo la variable MAELOC POR WLOC.
       77 WLOC                    PIC XX VALUE SPACES.
       

      **** TABLA-DIN* EN EL SIGUIENTE CAMPO CARGO Y MUESTRO LA DESCRIPCION *******
       77 WMDE                    PIC X(30) VALUE SPACES.
      **** TABLA-DIN* "APYNOM" VA A LA T-DINAMICA Y VUELVE CON DATOS *******
       01 APYNOM.
          02 WMCA  PIC 9(08) VALUE 0.
          02 WMAN  PIC X(30) VALUE " ".


      **** TABLA-DIN* EN EL SIGUIENTE CAMPO CARGO Y MUESTRO LA DESCRIPCION *******
       77 WMLOC                    PIC X(15) VALUE SPACES.
      **** TABLA-DIN* "APYNOM" VA A LA T-DINAMICA Y VUELVE CON DATOS *******
       01 TDYLOC.
          02 WMLO      PIC 99 VALUE 0.
          02 WMDESC    PIC X(15) VALUE " ".

      ** CAMPOS DONDE VOY A CARGAR LA FECHA DEL SISTEMA**
       01 FECHA.
         02 ANIO                  PIC 99.
         02 MES                   PIC 99.
         02 DIA                   PIC 99.

      *-- LEYENDAS DEL MENU DE INICIO DEL PGM --*
       01 TITULO.
         02 FILLER PIC X(21) VALUE  "ALTAS DE EMLEADOS    ".
         02 FILLER PIC X(21) VALUE  "BAJAS DE EMLEADOS    ".
         02 FILLER PIC X(21) VALUE  "MODIF.DE EMLEADOS    ".
         02 FILLER PIC X(21) VALUE  "ING.COD.DE EMPLEADOS ".
      *-- REDEFINICION DE LA TABLA "TITULO" --*
      *-- DEBO ACLARAR CUANTAS OCURRENCIAS TIENE --*
      *-- EN EL PGM VOY ATRABAJAR CON "TIT1(X)" - X = INDICE--*
       01 TABLA-TITULO REDEFINES TITULO.
         02 TIT1 OCCURS 4 TIMES PIC X(21).
      *--------------------------------------------------------*
      *-- MENSAJES AL USUARIO --*
       01 MENSAJES.
         02 FILLER PIC X(30) VALUE  "01- REGISTRO DADO DE ALTA     ".
         02 FILLER PIC X(30) VALUE  "02- REGISTRO DADO DE BAJA     ".
         02 FILLER PIC X(30) VALUE  "03- REGISTRO MODIFICADO       ".
         02 FILLER PIC X(30) VALUE  "04- REGISTRO GRABADO          ".
         02 FILLER PIC X(30) VALUE  "05- REGISTRO NO GRABADO       ".
         02 FILLER PIC X(30) VALUE  "06- INGRESE DATOS PARA ALTA   ".
         02 FILLER PIC X(30) VALUE  "07- INGRESE MODIFICACIONES    ".
         02 FILLER PIC X(30) VALUE  "08-MODIFICACIONES NO EFECTUADA".
         02 FILLER PIC X(30) VALUE  "09-       ERROR FATAL         ".
         02 FILLER PIC X(30) VALUE  "10-                           ".
         02 FILLER PIC X(30) VALUE  "11-                           ".
         02 FILLER PIC X(30) VALUE  "                              ".
         02 FILLER PIC X(30) VALUE  "13- CODIGO EN BLANCO VERIFIQUE".
         02 FILLER PIC X(30) VALUE  "14- VERIFIQUE LA OPCION: 1 A 4".
         02 FILLER PIC X(30) VALUE  "15- NO SE PUEDE DAR DE BAJA   ".
         02 FILLER PIC X(30) VALUE  "16- REGISTRO NO DADO DE BAJA  ".
         02 FILLER PIC X(30) VALUE  "17- ERROR *APELLIDO Y NOMBRES*".
         02 FILLER PIC X(30) VALUE  "18- ERROR *SALDO        *     ".
         02 FILLER PIC X(30) VALUE  "19- ERROR *lOCALIDAD*        ".
         02 FILLER PIC X(30) VALUE  "20- ERROR *ESTADO CIVIL*      ".
         02 FILLER PIC X(30) VALUE  "21- ERROR *FECHA DE INGRESO*  ".
         02 FILLER PIC X(30) VALUE  "22- ERROR *TELEFONO       *   ".
      *   02 FILLER PIC X(30) VALUE  "22- LA OPCION ES* S/N * O.K. *".
         02 FILLER PIC X(30) VALUE  "23- DESEA DAR DE ALTA* S/N   *".
         02 FILLER PIC X(30) VALUE  "24- M=MOD.- B=:BAJA- C=CANCEL.".
         02 FILLER PIC X(30) VALUE  "25- LA OPCION ES: M-B-C * O.K.".

      *-- REDEFINICION DE LA TABLA "MENSAJE" --*
      *-- DEBO ACLARAR CUANTAS OCURRENCIAS TIENE --*
      *-- EN EL PGM VOY ATRABAJAR CON "MENSAJE(X)" - X = INDICE--*
       01 TABLA-MENSAJES REDEFINES MENSAJES.
         02 MENSAJE OCCURS 25 TIMES PIC X(30).

      *-- AREA DE PANTALLAS ---*
       SCREEN SECTION.
      *-- PARA FECHA Y TITULO --*
        01  PANTALLA-TITULOS.
           02 BLANK SCREEN.
           02 LINE  1 COLUMN 60 VALUE  "FECHA".
           02 LINE  1 COLUMN 67 PIC X9/ FROM DIA.
           02 LINE  1 COLUMN 70 PIC X9/ FROM MES.
           02 LINE  1 COLUMN 73 PIC X9 FROM ANIO.
           02 LINE  2 COLUMN 30 REVERSE PIC X(20) FROM L-TIT.
           02 LINE 21 COLUMN  1 PIC X(80) FROM SUBRAYADO.

       01  PANTALLA-INGRESAR-CODIGO AUTO.
           02 LINE 4 COLUMN 5  VALUE  "DNI: ".
           02 LINE 4 COLUMN 25 REVERSE PIC 9(08) TO MCA.
           02 LINE 6 COLUMN 1 PIC X(80) FROM SUBRAYADO.

       01  PANTALLA-MOSTRAR-CODIGO AUTO.
           02 LINE 4 COLUMN 5  VALUE  "DNI: ".
           02 LINE 4 COLUMN 25 REVERSE PIC 9(08) FROM MCA.
           02 LINE 6 COLUMN 1 PIC X(80) FROM SUBRAYADO.

      *-- PARA INGRESAR DATOS DEL EMLEADO - DE PANTALLA A B.DATOS-*
       01  PANTALLA-RECIBIR-DATOS AUTO.
           02 LINE 08 COLUMN  05 VALUE "APELLIDO Y NOMBRES: ".
           02 LINE 08 COLUMN  28 PIC X(30) TO MAN.
           02 LINE 09 COLUMN  05 VALUE "SUELDO BASICO: ".
           02 LINE 09 COLUMN  28 PIC 9(05) TO MSB.


       01  PANTALLA-RECIBIR-DATOS0 AUTO.
           02 LINE 11 COLUMN  05 VALUE "CODIGO LOCALIDAD: ".
           02 LINE 11 COLUMN  23 PIC 9(1) TO MLO.
           02 LINE 11 COLUMN  26 VALUE "DESC: ".
           02 LINE 11 COLUMN  35 PIC X(15) TO WMLOC.

      *-- DE B. DATOS A PANTALLA --*
       01  PANTALLA-ENVIAR-DATOS.
           02 LINE 08 COLUMN  05 VALUE "APELLIDO Y NOMBRES: ".
           02 LINE 08 COLUMN  28 PIC X(30) FROM MAN.
           02 LINE 09 COLUMN  05 VALUE "SUELDO BASICO".
           02 LINE 09 COLUMN  28 PIC 9(05) FROM MSB.


       01  PANTALLA-ENVIAR-DATOS0.
           02 LINE 11 COLUMN  05 VALUE "CODIGO LOCALIDAD: ".
           02 LINE 11 COLUMN  23 PIC 9(1) FROM MLO.
           02 LINE 11 COLUMN  26 VALUE "DESCR: ".
           02 LINE 11 COLUMN  35 PIC X(15) FROM WMLOC.

      *-- PARA MODIFICAR DATOS --*
       01 PANTALLA-MODIFICAR-DATOS AUTO.
           02 LINE 08 COLUMN  05 VALUE "APELLIDO Y NOMBRES: ".
           02 LINE 08 COLUMN  28 PIC X(30) USING MAN.
           02 LINE 09 COLUMN  05 VALUE "SUELDO BASICO: ".
           02 LINE 09 COLUMN  28 PIC 9(05) USING MSB.


       01 PANTALLA-MODIFICAR-DATOS0 AUTO.
           02 LINE 11 COLUMN  05 VALUE "CODIGO LOCALIDAD: ".
           02 LINE 11 COLUMN  23 PIC 9(1) USING MLO.
           02 LINE 11 COLUMN  26 VALUE "DESC: ".
           02 LINE 11 COLUMN  35 PIC X(15) USING WMLOC.

      *-- PARA LOS MENSAJES -- DEFINIDOS ANTERIORMENTE --*
      *-- SE LE DA LA UBICACION Y LA VARIABLE ES "INCIDENCIA-P" --*
       01  PANTALLA-INCIDENCIAS.
         02 LINE 22 COLUMN 26 PIC X(30) FROM INCIDENCIA-P.

      *-- PARA CONFIRMAR E-M-C  --*
       01  PANTALLA-OPCIONES.
           02 LINE 22 COLUMN 26 PIC X(30) FROM CONFIRMA.

      *-- PARA CONFIRMAR SI-NO  --*
       01  PANTALLA-CONFIRMAR.
           02 LINE 22 COLUMN 26 PIC X(30) FROM CONFIRMA.

       01  P-PIE.
         02 LINE 21 COLUMN  1 PIC X(80) FROM SUBRAYADO.
         02 LINE 23 COLUMN  1 REVERSE PIC X(79) FROM LINEA.
         02 LINE 23 COLUMN  1 VALUE "  ESCAPE = SALIR     ".
         02 LINE 23 COLUMN 60 VALUE "  ENTER = CONTINUAR  ".


      ****
      *-- DIVISION DE PROCEDIMIENTOS --  PROGRAMACIÓN --*
       PROCEDURE DIVISION.
      *-- DECLARATIVES PARA TRATAMIENTO DE LAS BASES DE DATOS --*
      *-- SI NO EXISTE -> CREA --*
      *-- SI EXISTE -> OMITE EL PROCESO DE CREACIÓN Y CONTINÚA --*
       DECLARATIVES.
       PRIMERA SECTION.
       USE AFTER ERROR PROCEDURE ON MAESTRO.
       ERROR-APERTURA.
           IF FST NOT = "00"
              OPEN OUTPUT MAESTRO
              CLOSE MAESTRO
              OPEN I-O MAESTRO
            END-IF.
       ERROR-APERTURA.
       END DECLARATIVES.
       SEGUNDA SECTION.
       INICIO.
           OPEN I-O MAESTRO.
      *---- PARA DAR COLOR A LA PANTALLA -- LA FOTOCOPIA NO TIENE -----
           DISPLAY "  " CONTROL "FCOLOR = BLACK , BCOLOR = GREEN"
           MOVE 0 TO OP C-T.
      *-- CARGO EL CAMPO "FECHA" CON LA FECHA DEL SISTEMA --*
           ACCEPT FECHA FROM DATE.
           PERFORM MENUS  THRU F-MENUS UNTIL C-T = 27 .

       FIN.
           DISPLAY " FIN DE TRABAJO PRACTICO N 8 "
           LINE 23, POSITION 26 CONTROL "FCOLOR = WHITE,BCOLOR = BLACK".
           ACCEPT TECLA
      *-- ANTES DE SALIR DEBO CERRAR LA BASE DE DATOS --*
           CLOSE MAESTRO
           EXIT PROGRAM.
      *     STOP RUN.

       MENUS.
            MOVE 4 TO OP
            PERFORM CODIGO THRU F-CODIGO UNTIL C-T = 27.

       F-MENUS.
           EXIT.

       CODIGO.
           DISPLAY "  " CONTROL "FCOLOR = BLACK , BCOLOR = GREEN"
           MOVE TIT1(OP) TO L-TIT.
           DISPLAY PANTALLA-TITULOS.
           DISPLAY PANTALLA-INGRESAR-CODIGO.
           DISPLAY P-PIE
           ACCEPT  PANTALLA-INGRESAR-CODIGO ON ESCAPE GO FIN.
      *     IF MCA = "________"
      *       GO FIN
      *     END-IF
           IF MCA IS NOT NUMERIC
             MOVE 00000000 TO MCA
           END-IF
      *---------- LLAMO A LA TABLA DINAMICA DE APELLIDO Y NOMBRES ------*
           IF MCA = 00000000 OR MCA = "        "
              CALL "TDYAPE" USING APYNOM
              CANCEL "TDYAPE"
              DISPLAY " " CONTROL "FCOLOR = BLACK , BCOLOR = GREEN"
              MOVE WMCA TO MCA
              MOVE WMAN TO MAN
              IF MCA = 0
                GO MENUS
              END-IF
          END-IF.
       COMIENZO.
           PERFORM LIMPIAR-CAMPOS.
           PERFORM LEER-MAESTRO.

             IF WMAE = "NO"
                 MOVE 1 TO  OP
             END-IF.
      * SI VIENE ALGO DEL LA TABLA DINAMICA
             IF WMAE = "SI"
                    PERFORM LEER-LOC
                    DISPLAY " " ERASE
                    DISPLAY PANTALLA-TITULOS
                    DISPLAY PANTALLA-MOSTRAR-CODIGO
                    DISPLAY PANTALLA-ENVIAR-DATOS
                    DISPLAY PANTALLA-ENVIAR-DATOS0
                    PERFORM CONFIRMAR-OPCIONES

             IF OP-EMC = "C"
                 GO MENUS
             ELSE  IF OP-EMC = "B"
                 MOVE 2 TO OP
             ELSE IF OP-EMC = "M"
                 MOVE 3 TO OP
                END-IF
             END-IF.

           MOVE TIT1(OP) TO L-TIT.
           DISPLAY PANTALLA-TITULOS.
           IF OP = 1
             PERFORM ALTAS
              ELSE IF OP = 2
                PERFORM BAJAS
                   ELSE IF OP = 3
                     PERFORM MODIFICACIONES
           END-IF.
           PERFORM PULSAR-TECLA
           IF C-T = 27 GO FIN.
       F-CODIGO.
           EXIT.

       LIMPIAR-CAMPOS.
            MOVE SPACES TO  WMAE MAN.
            MOVE 0  TO  MSB MLO.
       LEER-MAESTRO.
                READ MAESTRO INVALID KEY MOVE "NO" TO WMAE.
                    IF FST = "00" AND PRI = "P"
                        MOVE "*" TO PRI
                        GO TO LEER-MAESTRO
                    END-IF.
                    IF FST = "00" AND PRI = "*"
                        MOVE "SI" TO WMAE
                        MOVE "P" TO PRI
                    END-IF.

        ALTAS.
            MOVE 06 TO IND
              PERFORM ENVIAR-MENSAJES
                DISPLAY PANTALLA-INCIDENCIAS
                 DISPLAY PANTALLA-MOSTRAR-CODIGO
                   DISPLAY PANTALLA-RECIBIR-DATOS
                   DISPLAY PANTALLA-RECIBIR-DATOS0
                    ACCEPT  PANTALLA-RECIBIR-DATOS ON ESCAPE GO FIN.
      ********* TABLA DINAMICA --- LOCALIDAD *********   ALTAS

                DISPLAY PANTALLA-RECIBIR-DATOS0
                ACCEPT PANTALLA-RECIBIR-DATOS0 ON ESCAPE GO FIN.
                PERFORM TABLA-DINAMICA-LOC.
      ************************************************************
            PERFORM GRABAR-ALTAS.


       BAJAS.
             DISPLAY PANTALLA-MOSTRAR-CODIGO
              DISPLAY PANTALLA-ENVIAR-DATOS
               DISPLAY PANTALLA-ENVIAR-DATOS0
                PERFORM GRABAR-BAJAS.

       MODIFICACIONES.
              MOVE 07 TO IND
              PERFORM ENVIAR-MENSAJES
              DISPLAY PANTALLA-MODIFICAR-DATOS
              DISPLAY PANTALLA-MODIFICAR-DATOS0
              DISPLAY PANTALLA-MOSTRAR-CODIGO
              DISPLAY PANTALLA-INCIDENCIAS
              ACCEPT PANTALLA-MODIFICAR-DATOS
              DISPLAY PANTALLA-INCIDENCIAS.

      ********* TABLA DINAMICA --- LOCALIDAD *********  MODIFICACIONES
               DISPLAY PANTALLA-MODIFICAR-DATOS0
               ACCEPT PANTALLA-MODIFICAR-DATOS0 ON ESCAPE GO FIN.
               IF MLO = 0 OR MLO = " "
                  PERFORM TABLA-DINAMICA-LOC
               END-IF.

              PERFORM GRABAR-MODIFICACIONES.

       VALIDAR-CAMPOS.
               MOVE 0 TO IND VAL
               IF MAN = SPACES
                 MOVE 17  TO VAL
               END-IF
               IF MSB < 1 AND VAL = 0
                 MOVE 18  TO VAL
               END-IF
      *          IF MLO < 1 AND VAL = 0
      *           MOVE 19  TO VAL
      *         END-IF.
               IF VAL > 0
                 MOVE "X" TO OP-SN
               END-IF.
       GRABAR-ALTAS.
                 PERFORM VALIDAR-CAMPOS
              IF VAL = 0
                  MOVE  "   DESEA DAR DE ALTA S/N  " TO CONFIRMA
                  PERFORM CONFIRMAR-GRABACION
                 IF OP-SN = "S"
                   DISPLAY MENSAJE(12)  LINE 19, POSITION 26
                   MOVE 01 TO IND
                   PERFORM ENVIAR-MENSAJES
                   DISPLAY PANTALLA-INCIDENCIAS
                   WRITE R-MAE INVALID KEY
                   DISPLAY MENSAJE(09) LINE 23, POSITION 26 GO TO FIN
                 ELSE IF OP-SN = "N"
                   MOVE 05 TO IND
                   PERFORM ENVIAR-MENSAJES
                   DISPLAY PANTALLA-INCIDENCIAS
                 END-IF
              ELSE IF VAL > 0
                DISPLAY MENSAJE(12)  LINE 19, POSITION 26
                DISPLAY MENSAJE(VAL) LINE 23, POSITION 26
                MOVE 0 TO VAL
                PERFORM PULSAR-TECLA
                 MOVE 06 TO IND
                 PERFORM ENVIAR-MENSAJES
                 DISPLAY PANTALLA-INCIDENCIAS
                 DISPLAY  P-PIE
                 ACCEPT  PANTALLA-RECIBIR-DATOS
                 GO GRABAR-ALTAS
             END-IF.
       GRABAR-BAJAS.
              MOVE  "    DESEA DAR DE BAJA S/N:   " TO CONFIRMA
              PERFORM CONFIRMAR-GRABACION
              IF OP-SN = "S"
                  DISPLAY MENSAJE(12)  LINE 19, POSITION 26
                  MOVE 02 TO IND
                  PERFORM ENVIAR-MENSAJES
                  DISPLAY PANTALLA-INCIDENCIAS
                  DELETE MAESTRO INVALID KEY
                  DISPLAY MENSAJE(09) LINE 23, POSITION 26  GO TO FIN
               ELSE
                   MOVE 16 TO IND
                   PERFORM ENVIAR-MENSAJES
                   DISPLAY PANTALLA-INCIDENCIAS
              END-IF.
       GRABAR-MODIFICACIONES.
               DISPLAY MENSAJE(12) LINE 23, POSITION 26
               PERFORM VALIDAR-CAMPOS
             IF VAL = 0
              MOVE  "GRABA LAS MODIFICACIONES S/N: " TO CONFIRMA
              PERFORM CONFIRMAR-GRABACION
                 IF OP-SN = "S"
                  DISPLAY MENSAJE(12)  LINE 19, POSITION 26
                  MOVE 03 TO IND
                   PERFORM ENVIAR-MENSAJES
                   DISPLAY PANTALLA-INCIDENCIAS
                  REWRITE R-MAE INVALID KEY
                  DISPLAY MENSAJE(09) LINE 23, POSITION 26 GO TO FIN
                 ELSE IF OP-SN = "N"
                   MOVE 08 TO IND
                   PERFORM ENVIAR-MENSAJES
                   DISPLAY PANTALLA-INCIDENCIAS
                 END-IF
             ELSE IF VAL > 0
                DISPLAY MENSAJE(VAL) LINE 23, POSITION 26
                MOVE 0 TO VAL
                ACCEPT  PANTALLA-RECIBIR-DATOS
                GO TO GRABAR-MODIFICACIONES
             END-IF.

       ENVIAR-MENSAJES.
           MOVE MENSAJE(IND) TO INCIDENCIA-P.
      *---- ESTA LINEA PARA BORRAR MENSAJES
           DISPLAY MENSAJE(12)  LINE 22, POSITION 60.
           DISPLAY MENSAJE(12)  LINE 24, POSITION 60.
           DISPLAY P-PIE.
      *----------------------------------------------*

       CONFIRMAR-OPCIONES.
           DISPLAY PANTALLA-OPCIONES.
           DISPLAY P-PIE
            CALL   "MENU-EMC" USING OP-EMC
            CANCEL "MENU-EMC".
        F-CONFIRMAR-OPCIONES.
           EXIT.

       CONFIRMAR-GRABACION.
           DISPLAY PANTALLA-CONFIRMAR.
           DISPLAY P-PIE
            CALL   "MENU-SN" USING OP-SN
            CANCEL "MENU-SN".
        F-CONFIRMAR-GRABACION.
           EXIT.
       
      * TABLA-DINAMICA LOCALIDAD*
       TABLA-DINAMICA-LOC.
           CALL   "TDYLOC" USING TDYLOC
           CANCEL "TDYLOC"
           MOVE WMLO TO MLO.
           MOVE WMDESC TO WMLOC.
           DISPLAY "  " CONTROL "FCOLOR = BLACK , BCOLOR = GREEN"
           DISPLAY PANTALLA-TITULOS
           DISPLAY PANTALLA-MOSTRAR-CODIGO
           DISPLAY PANTALLA-ENVIAR-DATOS
           DISPLAY PANTALLA-ENVIAR-DATOS0.

      ***  NUEVO **  RUTINA DE BUSQUEDA MAS RAPIDA CON START ********
      ***  START DE LOCALIDAD ********
         LEER-LOC.
           OPEN I-O MAELOC
           MOVE MLO TO MLOC
           START MAELOC KEY IS EQUAL MLOC
                 INVALID KEY MOVE "NO" TO WLOC
                 GO F-L-LOC.

            READ MAELOC NOT INVALID
                 MOVE "SI" TO WLOC
                 MOVE  MLDESC TO WMLOC
           CLOSE MAELOC.
       F-L-LOC.

           EXIT.

       PULSAR-TECLA.

      *    EN C-T SE ALMACENA EL VALOR ASOCIADO CON LA
      *    TECLA PULSADA
           ACCEPT TECLA ON EXCEPTION C-T
           IF C-T = 27
            GO TO FIN

           END-IF.








