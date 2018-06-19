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
           SELECT MAEEMP-E ASSIGN TO DISK  "MAEEMP.IND"
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS MCA
           ALTERNATE RECORD KEY IS MAN DUPLICATES
           FILE STATUS IS FST.

      *-- B.DATOS QUE SALE -- ORDENADA - VA A ENTRAR A LISTAR -*
      *-- PUEDE TENER CUALQUIER EXTENSIÓN EN ESTE CASO ".ORD"-*
           SELECT MAEEMP-S ASSIGN TO DISK "MAEEMP.ORD"
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL.

      *-- B.DATOS DE TRABAJO - SE OCUPA EN EL PROCESO DE ORDENAR-*
           SELECT MAEEMP-W ASSIGN TO DISK "MAEEMP.WWW".

       DATA DIVISION.
       FILE SECTION.

       FD  MAEEMP-E
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 42 CHARACTERS
           DATA RECORD IS R-MAE-E.
       01  R-MAE-E.
            02 MCA     PIC 9(8).
            02 MAN     PIC X(30).
            02 MANT    PIC 9(2).
            02 MSE     PIC 9.
            02 MCATE   PIC 9.

       FD  MAEEMP-S
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 42 CHARACTERS
           DATA RECORD IS R-MAE-S.
       01  R-MAE-S.
            02 FILLER  PIC X(42).

       SD  MAEEMP-W
           RECORD CONTAINS 42 CHARACTERS
           DATA RECORD IS R-MAE-W.
       01  R-MAE-W.
            02 WCA     PIC 9(8).
            02 WAN     PIC X(30).
            02 WANT    PIC 9(2).
            02 WSE     PIC 9.
            02 WCATE   PIC 9.

       WORKING-STORAGE SECTION.
       77 TECLA                   PIC X.
       77 FST                     PIC XX.
       77 OPCION    PIC     9.

       LINKAGE SECTION.
       77  OPC    PIC     9.

       PROCEDURE DIVISION USING OPC.
       INICIO.
             CALL   "MENOR-01" USING OPCION
             CANCEL "MENOR-01"
             IF OPCION = 1
                SORT MAEEMP-W
                ASCENDING KEY WCA
                USING  MAEEMP-E
                GIVING MAEEMP-S
             END-IF
             IF OPCION = 2
                SORT MAEEMP-W
                ASCENDING KEY WAN
                USING  MAEEMP-E
                GIVING MAEEMP-S
             END-IF
             IF OPCION = 3
                SORT MAEEMP-W
                ASCENDING KEY WSE
                USING  MAEEMP-E
                GIVING MAEEMP-S
             END-IF
             IF OPCION = 4
                SORT MAEEMP-W
                ASCENDING KEY WANT
                USING  MAEEMP-E
                GIVING MAEEMP-S
             END-IF
             IF OPCION = 5
                SORT MAEEMP-W
                ASCENDING KEY WCATE
                USING  MAEEMP-E
                GIVING MAEEMP-S
             END-IF

            IF OPCION = 6
            MOVE OPCION TO OPC
            DISPLAY "* CANCELADO POR EL USUARIO *"  LINE 24, POSITION 01
            DISPLAY "PULSE UNA TECLA PARA CONTINUAR" LINE 24 POSITION 30
            CONTROL "FCOLOR=RED", BLINK
             END-IF
            IF OPCION < 6
            DISPLAY "**  REGISTRUS ORDENADOS ** "  LINE 24, POSITION 01
            DISPLAY "PULSE UNA TECLA PARA CONTINUAR" LINE 24 POSITION 30
            CONTROL "FCOLOR=RED", BLINK
            END-IF.
            ACCEPT TECLA
      *      STOP RUN.
            EXIT PROGRAM.
