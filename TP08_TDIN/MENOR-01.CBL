      ***** MENU ORDENAR POR CAMPO*****
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENOR-01.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *--------PARA EL MOUSE - COBOL------------**
       77 X                 PIC 99  BINARY VALUE 10.
       77 Y                 PIC 99  BINARY VALUE 10.
       77 RES               PIC 99  BINARY VALUE 10.
       77 BOTON             PIC 9   BINARY.
       77 TECLA             PIC X.
       01 FI                PIC 99.
       01 LI                PIC 99.
      *-------       ----------------------------***

       01 TABLA-OPCIONES.
         02 FILLER PIC X(14) VALUE  "[NRO.DNI     ]".
         02 FILLER PIC X(14) VALUE  "[APELLIDO    ]".
         02 FILLER PIC X(14) VALUE  "[SUELDO      ]".
         02 FILLER PIC X(14) VALUE  "[LOCALIDAD   ]".
      *   02 FILLER PIC X(14) VALUE  "[CATEGORIA   ]".
         02 FILLER PIC X(14) VALUE  "[CANCELAR    ]".

       01 RTABLA REDEFINES TABLA-OPCIONES.
         02 ELEMEN PIC X(14) OCCURS 5 TIMES.

      *-- OPCION -> VIENE DEL PGM "ORDMA-01"--*

       LINKAGE SECTION.
       77  OPCION    PIC     9.

       SCREEN SECTION.
       01 P-TITULO.
          02 LINE 11 COLUMN 41 REVERSE VALUE "ORDENAR POR:".

       PROCEDURE DIVISION USING OPCION.
       INICIO.

      *-- LLAMA AL PGM "FONDO" CON "CALL" --*
      *-- LAS POSICIONES SON -- IZ-AR-DE-AB-COLOR --*
           CALL   "FONDO" USING 40 10 55 20 5.
           CANCEL "FONDO".

            DISPLAY P-TITULO.
            MOVE 0 TO FI.
            MOVE 1 TO LI.
       FILA.
           IF LI = 1 MOVE 13 TO FI.
           IF LI = 2 MOVE 14 TO FI.
           IF LI = 3 MOVE 15 TO FI.
           IF LI = 4 MOVE 16 TO FI.
           IF LI = 5 MOVE 17 TO FI.
      *     IF LI = 6 MOVE 18 TO FI.
       UNO.
           DISPLAY ELEMEN (LI) LINE FI POSITION 41 LOW
           CONTROL "WINDOW-CREATE, FCOLOR = BLUE, BCOLOR = CYAN".
           ADD 1 TO LI ADD 1 TO FI IF LI > 5 NEXT SENTENCE ELSE GO UNO.
       DOS.
           IF LI < 1 MOVE 5 TO LI.
           IF LI > 5 MOVE 1 TO LI.
           PERFORM FILA.
           DISPLAY ELEMEN (LI) LINE FI POSITION 41 LOW REVERSE
           CONTROL "WINDOW-CREATE, FCOLOR = BLUE, BCOLOR = CYAN".
       TRES.
      *     DISPLAY " " LINE 24 POSITION 01 LOW
           CALL "PIKORATC" USING X, Y, RES, BOTON.
           DISPLAY ELEMEN (LI) LINE FI POSITION 41 LOW
           CONTROL "WINDOW-CREATE, FCOLOR = BLUE, BCOLOR = CYAN".
           IF X = 13 AND Y >= 41 AND Y <= 55 MOVE 1 TO LI GO CUATRO.
           IF X = 14 AND Y >= 41 AND Y <= 55 MOVE 2 TO LI GO CUATRO.
           IF X = 15 AND Y >= 41 AND Y <= 55 MOVE 3 TO LI GO CUATRO.
           IF X = 16 AND Y >= 41 AND Y <= 55 MOVE 4 TO LI GO CUATRO.
           IF X = 17 AND Y >= 41 AND Y <= 55 MOVE 5 TO LI GO CUATRO.
      *     IF X = 18 AND Y >= 41 AND Y <= 55 MOVE 6 TO LI GO CUATRO.
           IF RES = 72 SUBTRACT 1 FROM LI GO DOS.
           IF RES = 80 ADD 1 TO LI GO DOS.
           IF RES = 01 MOVE 4 TO LI GO CUATRO.
           IF RES = 28 NEXT SENTENCE ELSE GO DOS.
           IF RES = 27 MOVE 4 TO LI GO CUATRO.

       CUATRO.
              MOVE LI TO OPCION
              IF LI = 5
               DISPLAY " " CONTROL "FCOLOR=BLUE, BCOLOR=CYAN"
              END-IF
              EXIT PROGRAM.

