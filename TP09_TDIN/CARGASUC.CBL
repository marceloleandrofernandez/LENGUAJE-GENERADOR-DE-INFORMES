       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARGCUR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAESUC ASSIGN TO DISK "SUCURSAL.IND"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS MSUC
           ALTERNATE RECORD KEY IS MDSUC DUPLICATES
           FILE STATUS IS FSTSUC.
      ****
      *-- DIVISION DE DATOS --*
      *-- DEFINICION DE LA BASE DE DATOS  --*
      *-- DEFINICION DE VARIABLES, CONSTANTES, PANTALLAS, ETC. --*
       DATA DIVISION.
       FILE SECTION.
       FD MAESUC
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 16 CHARACTERS
           DATA RECORD IS R-SUC.

       01 R-SUC.
            02 MSUC     PIC 9(1).
            02 MDSUC    PIC X(15).



       WORKING-STORAGE SECTION.
       77 M         PIC 9999 VALUE 0.
       77 FSTSUC    PIC XX.
       77 TECLA     PIC X.

      *****************************************************************
       LINKAGE SECTION.
       01 TABLA-1.
          02  ELEM1 OCCURS 1500 TIMES ASCENDING KEY TMCE1 INDEXED BY I.
              03  TMCE1  PIC 99.
              03  TMAN1  PIC X(15).

       PROCEDURE DIVISION USING TABLA-1.
       DECLARATIVES.
       TRATAMIENTO-ERROR SECTION.
       USE AFTER ERROR PROCEDURE ON MAESUC.
       ERROR-APERTURA.
           IF FSTSUC NOT = "00"
           OPEN OUTPUT MAESUC
           CLOSE MAESUC
           OPEN I-O MAESUC
           END-IF.
       ERROR-APERTURA.
       END DECLARATIVES.

       CONTINUACION SECTION.

       LOAD.
           MOVE 0 TO M.
           OPEN INPUT MAESUC.
           PERFORM LEER.
           PERFORM MOVER THRU F-MOVER.
       LEER.
           READ MAESUC NEXT RECORD AT END GO FIN.

       MOVER.
           ADD 1 TO M
           SET I TO M
      *---- PASA LOS DATOS DE LA TABLA A LA MATRIZ ----*
           MOVE MSUC TO TMCE1(I)
           MOVE MDSUC TO TMAN1(I)
           GO LEER.
       F-MOVER.
               EXIT.
       FIN.
      *     CLOSE MAESTRO.
           GOBACK.
