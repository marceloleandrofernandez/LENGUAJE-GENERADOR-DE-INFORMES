       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARGCUR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAESTRO ASSIGN TO DISK "LOCALIDAD.IND"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS MLOC
           ALTERNATE RECORD KEY IS MLDESC DUPLICATES
           FILE STATUS IS FSTC.
      ****
      *-- DIVISION DE DATOS --*
      *-- DEFINICION DE LA BASE DE DATOS  --*
      *-- DEFINICION DE VARIABLES, CONSTANTES, PANTALLAS, ETC. --*
       DATA DIVISION.
       FILE SECTION.
       FD MAESTRO
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 16 CHARACTERS
           DATA RECORD IS R-LOC.

       01 R-LOC.
            02 MLOC     PIC 9(1).
            02 MLDESC   PIC X(15).



       WORKING-STORAGE SECTION.
       77 M PIC 9999 VALUE 0.
       77 FSTC  PIC XX.
       77 TECLA PIC X.

      *****************************************************************
       LINKAGE SECTION.
       01 TABLA-1.
          02  ELEM1 OCCURS 1500 TIMES ASCENDING KEY TMCE1 INDEXED BY I.
              03  TMCE1  PIC 99.
              03  TMAN1  PIC X(15).

       PROCEDURE DIVISION USING TABLA-1.
       DECLARATIVES.
       TRATAMIENTO-ERROR SECTION.
       USE AFTER ERROR PROCEDURE ON MAESTRO.
       ERROR-APERTURA.
           IF FSTC NOT = "00"
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
           MOVE MLOC TO TMCE1(I)
           MOVE MLDESC TO TMAN1(I)
           GO LEER.
       F-MOVER.
               EXIT.
       FIN.
      *     CLOSE MAESTRO.
           GOBACK.
