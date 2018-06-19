       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARGAPE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAESTRO ASSIGN TO DISK "MAEEMP.IND"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS MCA
           ALTERNATE RECORD KEY IS MAN DUPLICATES
           FILE STATUS IS FSTM.

       DATA DIVISION.
       FILE SECTION.
        FD MAESTRO
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 42 CHARACTERS
           DATA RECORD IS R-MAE.

         01 R-MAE.
            02 MCA     PIC 9(8).
            02 MAN     PIC X(30).
            02 MANT    PIC 9(2).
            02 MSE     PIC 9.
            02 MCATE   PIC 9.

       WORKING-STORAGE SECTION.
       77 M PIC 9999 VALUE 0.
       77 FSTM  PIC XX.
       77 TECLA PIC X.

      *****************************************************************
       LINKAGE SECTION.
       01 TABLA-1.
          02  ELEM1 OCCURS 1500 TIMES ASCENDING KEY TMCE1 INDEXED BY I.
              03  TMCE1  PIC 9(08).
              03  TMAN1  PIC X(30).

       PROCEDURE DIVISION USING TABLA-1.
       DECLARATIVES.
       TRATAMIENTO-ERROR SECTION.
       USE AFTER ERROR PROCEDURE ON MAESTRO.
       ERROR-APERTURA.
           IF FSTM NOT = "00"
           OPEN OUTPUT MAESTRO
           CLOSE MAESTRO
           OPEN I-O MAESTRO
           END-IF.
       ERROR-APERTURA.
       END DECLARATIVES.

       CONTINUACION SECTION.

       LOAD.
           MOVE 0 TO M.
           OPEN INPUT MAESTRO.
           PERFORM LEER.
           PERFORM MOVER THRU F-MOVER.
       LEER.
           READ MAESTRO NEXT RECORD AT END GO FIN.

       MOVER.
           ADD 1 TO M
           SET I TO M
      *---- PASA LOS DATOS DE LA TABLA A LA MATRIZ ----*
           MOVE MCA TO TMCE1(I)
           MOVE MAN TO TMAN1(I)
           GO LEER.
       F-MOVER.
               EXIT.
       FIN.
      *     CLOSE MAESTRO.
           GOBACK.
