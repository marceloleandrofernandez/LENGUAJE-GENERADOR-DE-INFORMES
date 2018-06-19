       IDENTIFICATION DIVISION.
       PROGRAM-ID. INICIAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CL      PIC 99.
       77 TECLA   PIC X.
       77 ESPACIO PIC X(80) VALUE ALL " ".
       77 LINEA  PIC X(80) VALUE ALL "±".
       SCREEN SECTION.
       01 P-SIS.
           02 LINE 10 COLUMN 25 VALUE "‹‹‹‹‹  ‹‹‹‹‹  ‹   ‹  €   €".
           02 LINE 11 COLUMN 25 VALUE "€ € €  €      €€  €  €   €".
           02 LINE 12 COLUMN 25 VALUE "€ € €  €‹‹‹‹  € € €  €   €".
           02 LINE 13 COLUMN 25 VALUE "€   €  €      €  €€  €   €".
           02 LINE 14 COLUMN 25 VALUE "€   €  €‹‹‹‹  €   €  €‹‹‹€".
      *     02 LINE 18 COLUMN 25 VALUE "ALUMNO : Villalba Nestor A.".
      * EN IMPRESO NO TIENE, EXPLICAR COMO PONE EL NOMBRE DEL ALUMNO
       PROCEDURE  DIVISION.
       INICIO.
           MOVE 6 TO CL.
           DISPLAY LINEA LINE  5 POSITION 1 LOW
           CONTROL "FCOLOR=BLACK, BCOLOR=GREEN".
           PERFORM LINEAS 18 TIMES.
           DISPLAY "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" LINE  7 POSITION 21 HIGH
           CONTROL "FCOLOR = GREEN, BCOLOR = BLACK".
           DISPLAY "ƒƒƒƒƒƒƒƒƒø" LINE  7 POSITION 44 HIGH.
           DISPLAY "≥" LINE  8 POSITION 21.
           DISPLAY "           " LINE  8 POSITION 22.
           DISPLAY "TRABAJO PRACTICO NRO.  4  0" LINE 8 POSITION 25 HIGH.
           DISPLAY "  " LINE  8 POSITION 51.
           DISPLAY "≥" LINE  8 POSITION 53 HIGH.
           DISPLAY "±" LINE  8 POSITION 54 LOW.
           DISPLAY "√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" LINE  9 POSITION 21 HIGH.
           DISPLAY "ƒƒƒƒƒƒƒƒƒ¥" LINE  9 POSITION 44 HIGH.
           DISPLAY "±" LINE  9 POSITION 54 LOW.
           DISPLAY "≥" LINE 10 POSITION 21 HIGH.
           DISPLAY "                   " LINE 10 POSITION 22 LOW.
           DISPLAY "            "        LINE 10 POSITION 41 LOW.
           DISPLAY "≥" LINE 10 POSITION 53 HIGH.
           DISPLAY "±" LINE 10 POSITION 54 LOW.
           DISPLAY "≥" LINE 11 POSITION 21 HIGH.
           DISPLAY "                   " LINE 11 POSITION 22 LOW.
           DISPLAY "            "        LINE 11 POSITION 41 LOW.
           DISPLAY "≥" LINE 11 POSITION 53 HIGH.
           DISPLAY "±" LINE 11 POSITION 54 LOW.
           DISPLAY "≥" LINE 12 POSITION 21 HIGH.
           DISPLAY "≥" LINE 12 POSITION 53 HIGH.
           DISPLAY "±" LINE 12 POSITION 54 LOW.
           DISPLAY "                   " LINE 12 POSITION 22 LOW.
           DISPLAY "            "        LINE 12 POSITION 41 LOW.
           DISPLAY "≥" LINE 13 POSITION 21 HIGH.
           DISPLAY "≥" LINE 13 POSITION 53 HIGH.
           DISPLAY "±" LINE 13 POSITION 54 LOW.
           DISPLAY "≥" LINE 14 POSITION 21 HIGH.
           DISPLAY "                   " LINE 13 POSITION 22 LOW.
           DISPLAY "            "        LINE 13 POSITION 41 LOW.
           DISPLAY "      " LINE 14 POSITION 22 LOW.
           DISPLAY "≥" LINE 14 POSITION 53 HIGH.
           DISPLAY "±" LINE 14 POSITION 54 LOW.
           DISPLAY "                   " LINE 14 POSITION 22 LOW.
           DISPLAY "            "        LINE 14 POSITION 41 LOW.
           DISPLAY "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" LINE 15 POSITION 21 HIGH.
           DISPLAY "ƒƒƒƒƒƒƒƒƒŸ" LINE 15 POSITION 44 HIGH.
           DISPLAY "±" LINE 15 POSITION 54 LOW.
           DISPLAY "±±±±±±±±±±±±±±±±±±±±±±±" LINE 16 POSITION 22 LOW.
           DISPLAY "±±±±±±±±±±" LINE 16 POSITION 45 LOW.
      * EN IMPRESO NO TIENE, EXPLICAR COMO PONE EL NOMBRE DEL ALUMNO
           DISPLAY "***************************" LINE 18 POSITION 25 LOW.
           DISPLAY "*ALUMNO: FERNANDEZ MARCELO " LINE 19 POSITION 25 LOW.
           DISPLAY "***************************" LINE 20 POSITION 25 LOW.
           display p-sis.
       LINEAS.
           DISPLAY ESPACIO LINE  CL POSITION 1 LOW
           CONTROL "FCOLOR=GREEN, BCOLOR=BLACK"
           ADD 1 TO CL.


