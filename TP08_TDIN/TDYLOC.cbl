       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDYLOC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77 L1 PIC 9999 VALUE 0.                            
       77 L2 PIC 9999 VALUE 0.
       77 L3 PIC 9999 VALUE 0.

       77 FILA PIC 99.
       77 NRO PIC 9999.

      *///////// BANDERAS /////////////
       77 SALIR        PIC X VALUE "N".
       77 BUSXCOD PIC X VALUE "N".
       77 COINCIDE     PIC X VALUE "N".
       77 SEGUIR       PIC X.
      *///////// BANDERAS /////////////
      *///////// CONTADORES ///////////
       77 CERO      PIC 99.
       77 CANT      PIC 9999 VALUE 1.
       77 POSLET    PIC 99 VALUE 1.
       77 POSCERO   PIC 99 VALUE 1.
      *///////// CONTADORES ///////////
       77 BOT2 PIC X VALUE "N".
      ************************************************************************
      *********** VARIABLES PARA EL PIKORACT**********************************
       77 F PIC 99 BINARY VALUE 99.
       77 C PIC 99 BINARY VALUE 99.
       77 RES PIC 999 BINARY VALUE 999.
       77 BOT PIC 9 BINARY VALUE 1.
      ************************************************************************
       77 RESX PIC 999 BINARY VALUE 999.
      ************************************************************************
      *77 PALABRA2 PIC X(50).
       77 COLU2 PIC 99.
       77 COLU2B PIC 99.
       77 TECLA PIC X .
       77 LL PIC X .
       77 ANT PIC 99.
       77 ACT PIC 99.
       77 V PIC 99.
       77 CANTL PIC 99 VALUE 0.
       77 ANCHO PIC 99 value 30.
       77 FILAE PIC 99 value 0.
       77 COLU  PIC 99 value 0.
      ************************************************************************
      *********** MASCARAS PARA SALIDA DE DATOS  ***********************
       77 CODMAS PIC Z(02).
       77 MASCA  PIC Z(15).
      ************************************************************************
       77 SS2 PIC 99.
      ************************************************************************
      *********** VECTORES PARA COMPARAR LOS CAMPOS********************
       01 PALABRA.
          02 LETRA  OCCURS  50  TIMES  PIC  X.
       01 DIVIDEX.
          02 LETRA2 OCCURS  50  TIMES  PIC  X.
      *****************************************************************
      *********** MATRICES ALMACENAN LOS DATOS DE LAS TABLAS **********
      *-----------  TABLA-1 SE CARGA EN LA TABLA DINAMICA = TDYNAPE --*
       01 TABLA-1.
          02  ELEM1 OCCURS 1500 TIMES ASCENDING KEY TMDE1 INDEXED BY I3.
              03  TMCO1 PIC 99.
              03  TMDE1 PIC X(15).

       01 TABLA-2.
          02  ELEM2 OCCURS 1500 TIMES ASCENDING KEY TMDE2 INDEXED BY I.
              03  TMCO2 PIC 99.
              03  TMDE2 PIC X(15).

      *--- TABLA-3 CANTIDAD DE REGISTRO A MOSTRAR POR PRIMERA VEZ ----*
       01 TABLA-3.
          02  ELEM3 OCCURS 10 TIMES INDEXED BY I2  .
              03  TMCO3 PIC 99.
              03  TMDE3 PIC X(15).
      *****************************************************************
      *****************************************************************
      *********** VARIABLES PARA EL PROGRAMA GETWKEY ******************
      *********** GETWKEY: SIRVE PARA CAPTURAR LA TRECLA PULSADA ******
       01 ARGS.
              03  ERRORX    PIC 9(2).
              03  ASCII     PIC X(1).
              03  KEYTECLA  PIC 9(3).

      *****************************************************************
       LINKAGE SECTION.
      *****************************************************************
      *********** VARIABLES QUE VIENEN DEL PROGRAMA DONDE SE LLAMO A  *
      *********** LA TABLA DINAMICA  **********************************
      *****************************************************************
      ***  WMNC = "CODIGO DEL CONCEPTO" *******************************
      ***  WMDC = "DESCRIPCION " *************************************
      *****************************************************************
       01 CONCEPT.
          02  WMNC PIC 99.
          02  WMDC PIC X(15).
      *****************************************************************
      ****** SE CARGAN LAS  VARIABLES DEL REGISTRO QUE SE ELIGE EN LA *
      **********************  TABLA DINAMICA  *************************
      *****************************************************************

       PROCEDURE DIVISION USING CONCEPT.
       INICIO.
      *****************************************************************
      *********** CARGA DATOS DE LA TALA EN LA MATRIZ *****************
      *****************************************************************
           CALL   "CARGALOC" USING TABLA-1
           CANCEL "CARGALOC"
           PERFORM RESETEAR.
      *****************************************************************
      *-- FILAE = POS. DEL CAMPO DE ENTRADA PARA EL PGM "&GETWKEY" ---*
           MOVE 3 TO FILAE
           MOVE 29 TO COLU
      ************************************************************************
      *********** IMPRIME RECUADRO Y TITULO **********************************
           CALL    "MENSAJE" USING 10 02 75 04 "W".
           CANCEL  "MENSAJE".
           CALL    "MENSAJE" USING 10 05 75 16 "B".
           CANCEL  "MENSAJE".
      *-- AQUI LEYENDA DEL CAMPO DE ENTRADA DONDE SE ESCRIBE --*
           DISPLAY "INGRESE CODIGO:               "
           LINE 03 POSITION 11 LOW
           CONTROL "FCOLOR = WHITE, BCOLOR = RED".
      *----------------------------------------------------------------------*
           PERFORM IMPRIMIR-MATRIZ.
           GO TO ACEPTAR2.
      ************************************************************************
       FINX.
            DISPLAY "CANCELADO POR EL USUARIO" LINE 3, POSITION 30,
            BLINK
            ACCEPT TECLA
            MOVE "CANCELADO" TO WMDC
            MOVE  00000000   TO WMNC
      *     MOVE  PALABRA TO PALABRA2.
      *     GO TO NAVEGACION.
            GOBACK.
       FIN.
           GOBACK.

       NAVEGACION.
      ************************************************************************
      *------ FILA = VER -----------*
           MOVE 1 TO NRO.
           MOVE 0 TO FILA.
           PERFORM MOSTRAR-OPCION.
           PERFORM UNTIL SALIR = "S"
           DISPLAY " " LINE 1 POSITION 80 LOW
           CONTROL "FCOLOR = WHITE, BCOLOR = BLUE"
      ************************************************************************
           CALL "PIKORATC" USING F, C, RES, BOT
      ************************************************************************
           PERFORM EVALUAR-TECLA
           IF BOT2 = "N"
           PERFORM MOSTRAR-OPCION
           END-IF
           END-PERFORM.

       EVALUAR-TECLA.
           IF NOT (RES = 99) AND BOT2 = "S"
              MOVE "N" TO BOT2
              PERFORM REFRESCAR END-IF.
                EVALUATE RES
      *------- Flecha Arriba----------------------*
                WHEN 72
                          EVALUATE NRO
                          WHEN 1
                          IF CANT > 1
                          SUBTRACT 1 FROM CANT PERFORM IMPRIMIR-MATRIZ
                          ELSE
                          GO TO ACEPTAR2
                          END-IF
      *---------------- CANTIDAD DE VECES A LIMPIAR (10) -------------*
                          WHEN 2 THRU 10
                          PERFORM LIMPIAR  SUBTRACT 1 FROM NRO
                          END-EVALUATE
      *------- Flecha Abajo ----------------------*
               WHEN 80
                           EVALUATE NRO
      *---------------- CANTIDAD DE REGISTROS (10) -------------------*
                           WHEN 10
                           IF (CANT + 6) < 9998 ADD 1 TO CANT
      *---------------- CANTIDAD DE VECES A IMPRIMIR MATRIZ(10) ------*
                           PERFORM IMPRIMIR-MATRIZ END-IF
                             WHEN 1 THRU 10
                             PERFORM LIMPIAR  ADD 1 TO NRO
                           END-EVALUATE

      *------  Enter arriba del Registro ---------*
               WHEN 28 PERFORM EJECUTAR-OPCION
      *------  Cancel ----------------------------*
               WHEN 01 GO FIN
               END-EVALUATE.

       EJECUTAR-OPCION.
           SET I2 TO NRO.
           SET I3 TO 1.
           MOVE "S" TO SALIR.
           SEARCH ELEM1 WHEN TMCO1(I3) = TMCO3(I2)
           MOVE TMCO1(I3) TO WMNC
           MOVE TMDE1(I3) TO WMDC
           GO FIN.

       REFRESCAR.
            PERFORM IMPRIMIR-MATRIZ.
            PERFORM MOSTRAR-OPCION.

       IMPRIMIR-MATRIZ.
      *-----------------------------------------------------*
      *--- L1 = POSICION DEL LOS REGS. POR PRIMERA VES -----*
      *----L2 = POSICION DEL LOS REGS. POR SEGUNDA VES -----*
      *-----------------------------------------------------*
           SET I  TO CANT.
           SET I2 TO 1.
           MOVE 5 TO L1 L2.
      *** EL PERFORM SE EJECUTA LA CANT. DE OCCURS QUE TIENE TABLA-3 ***
           PERFORM 10 TIMES
             ADD 1 TO L1
             MOVE TMDE2(I)  TO TMDE3(I2)
             MOVE TMCO2(I)  TO TMCO3(I2) CODMAS
             DISPLAY CODMAS  LINE L1 POSITION 11 LOW
             CONTROL "FCOLOR = WHITE, BCOLOR = BLUE"
             DISPLAY TMDE3(I2) LINE L1 POSITION 22 LOW
             SET I  UP BY 1
             SET I2 UP BY 1
           END-PERFORM.


       LIMPIAR.
           COMPUTE FILA = NRO + L2.
           MOVE TMCO3(NRO) TO CODMAS
           DISPLAY CODMAS LINE FILA POSITION 11 LOW
           CONTROL "FCOLOR = WHITE, BCOLOR = BLUE".
           DISPLAY TMDE3(NRO) LINE FILA POSITION 22 LOW.

       MOSTRAR-OPCION.
           COMPUTE FILA = NRO + L2.
           DISPLAY " "
           LINE 1 POSITION 79
           CONTROL "FCOLOR = BLACK, BCOLOR = BLUE".

           DISPLAY TMDE3(NRO) LINE FILA POSITION 22 LOW.

           MOVE TMCO3(NRO) TO CODMAS
           DISPLAY CODMAS LINE FILA POSITION 11 LOW.


       ACEPTAR2.
      ********* PROCEDIMIENTO PARA EVALUAR TECLA PULSADA***********

           MOVE 1 TO POSLET.
           MOVE COLU TO COLU2.

           PERFORM MOSTRAR.

           PERFORM  UNTIL ANCHO < POSLET

           DISPLAY LETRA(POSLET)  LINE FILAE POSITION COLU2 LOW
           CONTROL "FCOLOR = WHITE, BCOLOR = RED"

      ******************************************************************
      ********* LLAMA AL PROGRAMA QUE ESTA A LA ESPERA  QUE SE  ********
      ********* PULSE UNA TECLA - ES PARECIDO AL PIKORACT PERO ES SOLO *
      ********* PARA EL TECLADO. ***************************************
      ******************************************************************
           CALL "&GETWKEY" USING ARGS
      ******************************************************************
      ******************************************************************
            MOVE KEYTECLA TO RESX
            IF ERRORX = 00
              EVALUATE RESX

      ******* NUMEROS   ************************************************
              WHEN 43 THRU 57
                   IF POSLET = 1 MOVE "S" TO BUSXCOD END-IF
                   MOVE ASCII TO LL
                   IF RESX = 47 MOVE "-" TO LL END-IF
                   PERFORM CARGAR

      ******* LETRAS MAYUSCUKAS ****************************************
              WHEN 65 THRU 90
                  MOVE "N" TO BUSXCOD
                  MOVE ASCII TO LL PERFORM CARGAR

      ******* LETRAS MINUSCULAS LO TRASFORMA A MAYUSCULAS **************
              WHEN 97 THRU 122
                  MOVE "N" TO BUSXCOD
                  INSPECT ASCII CONVERTING "abcdefghijklmnopqrstuvwxyz"
                  TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  MOVE ASCII TO LL
                  PERFORM CARGAR

      ******* CARACTERES ESPECIALES ************************************
              WHEN 63  MOVE "_" TO LL PERFORM CARGAR
              WHEN 32  MOVE " " TO LL PERFORM CARGAR
              WHEN 42  MOVE "(" TO LL PERFORM CARGAR
              WHEN 40  MOVE ")" TO LL PERFORM CARGAR
              WHEN 38  MOVE "/" TO LL PERFORM CARGAR

      ******* TECLA DE BORRADO *****************************************
             WHEN 08
             IF NOT (COLU = COLU2)
             SUBTRACT 1 FROM POSLET COLU2
             MOVE " " TO LETRA (POSLET)
             MOVE " " TO TABLA-2
             PERFORM IMPRIMIR-MATRIZ
             SUBTRACT 1 FROM POSLET
             IF POSLET = 0
             PERFORM LOAD
             ELSE
             PERFORM LOAD2
             END-IF
             PERFORM IMPRIMIR-MATRIZ
             ADD 1 TO POSLET
             END-IF

      ******* TECLA DE ESCAPE ******************************************
      *       WHEN 27 MOVE 6 TO FOCO GO FINX
             WHEN 27 GO FINX
      ******* TECLA DE ENTER *******************************************
             WHEN 13  GO TO  NAVEGACION
             END-EVALUATE

           ELSE

             EVALUATE RESX
      ******* TECLA DE FLECHA HACIA ABAJO ******************************
             WHEN 80  GO TO  NAVEGACION

      ******* TECLA DE SUPRIMIR ****************************************
             WHEN 83 PERFORM SUP

      ******* TECLA DE (ALT+2) PARA EL ARROBA **************************
             WHEN 121
                   MOVE "@" TO LL PERFORM CARGAR

             END-EVALUATE
             END-IF
            END-PERFORM.

       SUP.
      **** PROCEDIMIENTO QUE SE ***********************
      **** EJECUTA CUANDO SE PRESIONA LA TECLA SUP ****
           MOVE POSLET TO ACT ANT.
           ADD 1 TO ANT.
           PERFORM UNTIL ANT > ANCHO
           MOVE LETRA (ANT) TO LETRA (ACT)
           IF ANT = ANCHO
           MOVE " " TO LETRA (ANCHO)
           END-IF
           PERFORM MOSTRAR
           ADD 1 TO ANT ACT
           END-PERFORM.
      *************************************************


       DESPLAZAR.
           MOVE ANCHO TO ACT ANT.
           SUBTRACT 1 FROM  ANT
           PERFORM UNTIL ANT < POSLET
           MOVE LETRA (ANT) TO LETRA (ACT)
           SUBTRACT 1 FROM  ANT ACT
           END-PERFORM.



       MOSTRAR.
           MOVE COLU TO COLU2B.
           MOVE 1 TO V .
           PERFORM UNTIL V > ANCHO
           DISPLAY LETRA (V) LINE FILAE POSITION COLU2B LOW
           CONTROL "FCOLOR = WHITE, BCOLOR = RED"
           ADD 1 TO V COLU2B
           END-PERFORM.
      *****************************************************************
       LOAD2.
           MOVE "S" TO SEGUIR.
           MOVE 0 TO L1 L3.
           PERFORM UNTIL SEGUIR = "N"
           PERFORM LEER2
           IF SEGUIR = "S"
           PERFORM MOVER2
           END-IF
           END-PERFORM.


       LEER2.
            MOVE 0 TO CANTL.
            ADD 1 TO L3
            SET I3 TO L3
            IF TMDE1(I3) = " "
            MOVE "N" TO SEGUIR
            ELSE
            IF BUSXCOD = "S"
            MOVE TMCO1(I3) TO DIVIDEX
            PERFORM SACAR-CEROS
            ELSE
            MOVE TMDE1(I3) TO DIVIDEX
            END-IF
            END-IF.

       MOVER2.
           PERFORM UNTIL POSLET  = CANTL
            ADD 1 TO CANTL
               IF CANTL > 1
               SUBTRACT 1 FROM CANTL
               ADD 1 TO CANTL
               END-IF

           IF LETRA(CANTL) = LETRA2(CANTL)
           MOVE "S" TO COINCIDE
           ELSE
           MOVE "N" TO COINCIDE
           MOVE POSLET TO CANTL
           END-IF

           END-PERFORM
           IF COINCIDE = "S"
           ADD 1 TO L1
           SET I TO L1
           MOVE TMCO1(I3) TO TMCO2(I)
           MOVE TMDE1(I3) TO TMDE2(I)
           END-IF.


       CARGAR.
           MOVE LL TO LETRA (POSLET).
           DISPLAY LETRA (POSLET) LINE FILAE POSITION COLU2 LOW.
           MOVE " " TO TABLA-2.
           PERFORM LOAD2.
           PERFORM IMPRIMIR-MATRIZ.
           ADD 1 TO POSLET COLU2.


       RESETEAR.
             MOVE TABLA-1 TO TABLA-2.

       LOAD.
           MOVE 0 TO L1.
           PERFORM RESETEAR.


       SACAR-CEROS.
           MOVE 1 TO CERO
           MOVE 1 TO POSCERO
           PERFORM UNTIL LETRA2(POSCERO) > 0
             COMPUTE CERO = CERO + 1
             ADD 1 TO POSCERO
           END-PERFORM

           MOVE 1 TO POSCERO
           PERFORM UNTIL CERO = 30
             MOVE LETRA2(CERO) TO LETRA2(POSCERO)
             COMPUTE CERO = CERO + 1
             ADD 1 TO POSCERO
           END-PERFORM.



