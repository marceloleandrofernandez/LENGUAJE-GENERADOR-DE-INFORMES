      ** SORT = ORDENA LA POR EL CAMPO SOLICITADO "OPCION"**
        IDENTIFICATION DIVISION.
        PROGRAM-ID. ORDMA-04.
        ENVIRONMENT DIVISION.                      
        CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *- SIEMPRE SE DEFINEN 3 B.DATOS: ENTRADA-SAlIDA-TRABAJO--*

      *-- B.DATOS QUE ENTRA -- DEFINIDO EN EL PGM "MODABM-1"-*
           SELECT MAESTRO-E ASSIGN TO DISK  "MALU01.IND"
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS MCA
           FILE STATUS IS FST.

      *-- B.DATOS QUE SALE -- ORDENADA - VA A ENTRAR A LISTAR -*
      *-- PUEDE TENER CUALQUIER EXTENSIÓN EN ESTE CASO ".SEC"-*
           SELECT MAESTRO-S ASSIGN TO DISK "MALU01.SEC"
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL.

      *-- B.DATOS DE TRABAJO - SE OCUPA EN EL PROCESO DE ORDENAR-*
           SELECT MAESTRO-W ASSIGN TO DISK "MALU01.WWW".

       DATA DIVISION.
       FILE SECTION.

       FD  MAESTRO-E
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 51 CHARACTERS
           DATA RECORD IS R-MAE-E.
       01  R-MAE-E.
 	    02 MCA  PIC 9(8).
	    02 MAN  PIC X(30).
	    02 MED  PIC 9(3).
	    02 MSE  PIC 9.
	    02 MEC  PIC 9.
            02 MFI.
	       03  MDD	PIC 9(2).
	       03  MMM	PIC 9(2).
	       03  MAA	PIC 9(2).
            02 MCU  PIC 9(2).
       FD  MAESTRO-S
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 51 CHARACTERS
           DATA RECORD IS R-MAE-S.
       01  R-MAE-S.
            02 FILLER  PIC X(51).

       SD  MAESTRO-W
           RECORD CONTAINS 51 CHARACTERS
           DATA RECORD IS R-MAE-W.
       01  R-MAE-W.
	    02 WCA  PIC 9(8).
	    02 WAN  PIC X(30).
	    02 WED  PIC 9(3).
	    02 WSE  PIC 9.
	    02 WEC  PIC 9.
	    02 WFI.
	       03  WDD	PIC 9(2).
	       03  WMM	PIC 9(2).
	       03  WAA	PIC 9(2).
            02 WCU  PIC 9(2).

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
                SORT MAESTRO-W
                ASCENDING KEY WCA
                USING  MAESTRO-E
                GIVING MAESTRO-S
             END-IF
             IF OPCION = 2
                SORT MAESTRO-W
                ASCENDING KEY WAN
                USING  MAESTRO-E
                GIVING MAESTRO-S
             END-IF
             IF OPCION = 3
                SORT MAESTRO-W
                ASCENDING KEY WED
                USING  MAESTRO-E
                GIVING MAESTRO-S
             END-IF
             IF OPCION = 4
                SORT MAESTRO-W
                ASCENDING KEY WSE 
                USING  MAESTRO-E
                GIVING MAESTRO-S
             END-IF
             IF OPCION = 5
                SORT MAESTRO-W
                ASCENDING KEY WEC
                USING  MAESTRO-E
                GIVING MAESTRO-S
             END-IF
             IF OPCION = 6
                SORT MAESTRO-W
                ASCENDING KEY WCU
                USING  MAESTRO-E
                GIVING MAESTRO-S
             END-IF
            IF OPCION = 7
            MOVE OPCION TO OPC
            DISPLAY "* CANCELADO POR EL USUARIO *"  LINE 24, POSITION 01
            DISPLAY "PULSE UNA TECLA PARA CONTINUAR" LINE 24 POSITION 30
            CONTROL "FCOLOR=RED", BLINK
             END-IF
            IF OPCION < 6
            DISPLAY "**  REGISTROS ORDENADOS ** "  LINE 24, POSITION 01
            DISPLAY "PULSE UNA TECLA PARA CONTINUAR" LINE 24 POSITION 30
            CONTROL "FCOLOR=RED", BLINK
            END-IF
            ACCEPT TECLA
            EXIT PROGRAM.
      *      STOP RUN.