      ** SORT = ORDENA LA POR EL CAMPO SOLICITADO "OPCION"**
        IDENTIFICATION DIVISION.
        PROGRAM-ID. ORDMA-01.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
      *- SIEMPRE SE DEFINEN 3 B.DATOS: ENTRADA-SAlIDA-TRABAJO--*

      *-- B.DATOS QUE ENTRA -- DEFINIDO EN EL PGM "MODABM-1"-*
            SELECT MAEPERS-E ASSIGN TO DISK "PERSONA.IND"
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS MCA
                ALTERNATE RECORD KEY IS MAN DUPLICATES
                FILE STATUS IS FST.

      *-- B.DATOS QUE SALE -- ORDENADA - VA A ENTRAR A LISTAR -*
      *-- PUEDE TENER CUALQUIER EXTENSIÓN EN ESTE CASO ".ORD"-*
            SELECT MAEPERS-S ASSIGN TO DISK "MAEPERS.ORD"
                ORGANIZATION IS SEQUENTIAL
                ACCESS IS SEQUENTIAL.

      *-- B.DATOS DE TRABAJO - SE OCUPA EN EL PROCESO DE ORDENAR-*
           SELECT MAEPERS-W ASSIGN TO DISK "MAEPERS.WWW".

       DATA DIVISION.
       FILE SECTION.

       FD MAEPERS-E
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 45 CHARACTERS
           DATA RECORD IS R-MAE-E.

         01 R-MAE-E.
            02 MCA  PIC 9(08).
            02 MAN  PIC X(30).
            02 MSA  PIC 9(05).
            02 MTS  PIC 9.
            02 MSUC  PIC 9(1).


       FD  MAEPERS-S
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 45 CHARACTERS
           DATA RECORD IS R-MAE-S.
       01  R-MAE-S.
            02 FILLER  PIC X(43).

       SD  MAEPERS-W
           RECORD CONTAINS 45 CHARACTERS
           DATA RECORD IS R-MAE-W.
       01  R-MAE-W.
            02 WCA  PIC 9(08).
            02 WAN  PIC X(30).
            02 WSA  PIC 9(05).
            02 WTS  PIC 9.
            02 WSUC  PIC 9(1).

       WORKING-STORAGE SECTION.
       77 TECLA                   PIC X.
       77 FST                     PIC XX.
       77 OPCION                  PIC 9.

       LINKAGE SECTION.
       77  OPC    PIC     9.

       PROCEDURE DIVISION USING OPC.
       INICIO.
             CALL   "MENOR-01" USING OPCION
             CANCEL "MENOR-01"
             IF OPCION = 1
                SORT MAEPERS-W
                ASCENDING KEY WCA
                USING  MAEPERS-E
                GIVING MAEPERS-S
             END-IF
             IF OPCION = 2
                SORT MAEPERS-W
                DESCENDING KEY WSA
                USING  MAEPERS-E
                GIVING MAEPERS-S
             END-IF
             IF OPCION = 3
                SORT MAEPERS-W
                ASCENDING KEY WTS
                USING  MAEPERS-E
                GIVING MAEPERS-S
             END-IF
             IF OPCION = 4
                SORT MAEPERS-W
                ASCENDING KEY WSUC
                USING  MAEPERS-E
                GIVING MAEPERS-S
             END-IF
      *       IF OPCION = 5
      *          SORT MAEEMP-W
      *          ASCENDING KEY WCATE
      *          USING  MAEEMP-E
      *          GIVING MAEEMP-S
      *       END-IF

            IF OPCION = 5
            MOVE OPCION TO OPC
            DISPLAY "* CANCELADO POR EL USUARIO *"  LINE 24, POSITION 01
            DISPLAY "PULSE UNA TECLA PARA CONTINUAR" LINE 24 POSITION 30
            CONTROL "FCOLOR=RED", BLINK
             END-IF
            IF OPCION < 5
            DISPLAY "**  REGISTROS ORDENADOS ** "  LINE 24, POSITION 01
            DISPLAY "PULSE UNA TECLA PARA CONTINUAR" LINE 24 POSITION 30
            CONTROL "FCOLOR=RED", BLINK
            END-IF.
            ACCEPT TECLA
      *      STOP RUN.
            EXIT PROGRAM.
