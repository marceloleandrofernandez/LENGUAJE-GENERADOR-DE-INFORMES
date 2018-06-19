      *** MENU OPCIONES: LISTAR POR PANTALLA O IMPRESORA ***
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENLI-01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *-----* PARA EL MOUSE-COBOL *-----*
       77  X    PIC     99 binary value 10.
       77  Y    PIC     99 binary value 10.
       77  RES  PIC     99 binary value 10.
       77  BOTON PIC    9 BINARY.
      *----------------------------------*
       77 TECLA         PIC XX.
      *----------------------------------*
       01 TABLA-OPCIONES.
           02 FILLER PIC X(11) VALUE "[PANTALLA ]".
           02 FILLER PIC X(11) VALUE "[IMPRESORA]".
           02 FILLER PIC X(11) VALUE "[CANCELAR ]".
       01 RTABLA REDEFINES TABLA-OPCIONES.
           02 ELEMEN PIC X(11) OCCURS 3 TIMES.

       01 FI PIC 99.
       01 LI PIC 99.
       01 OP PIC X.


       SCREEN SECTION.
       01 P-TITULO.
          02 LINE 16 COLUMN 58 REVERSE VALUE "LISTAR POR:".

       PROCEDURE DIVISION.
        INICIO.
      *-- LLAMA AL PGM "FONDO" CON "CALL" --*
      *-LAS POSICIONES SON -- IZ-AR-DE-AB-COLOR --*
           CALL "FONDO" USING 57 15 69 21 1.
           CANCEL "FONDO"
           DISPLAY P-TITULO.

           DISPLAY "                           " LINE 24, POSITION 01
      **************** MOUSE **************
           MOVE 0 TO FI.
           MOVE 1 TO LI.
        FILA.
           IF LI = 1 MOVE 18 TO FI.
           IF LI = 2 MOVE 19 TO FI.
           IF LI = 3 MOVE 20 TO FI.

        UNO.
           DISPLAY ELEMEN (LI) LINE FI POSITION 58 LOW
           CONTROL "WINDOW-CREATE, FCOLOR = WHITE, BCOLOR = BLACK".
           ADD 1 TO LI ADD 1 TO FI IF LI > 3 NEXT SENTENCE ELSE GO UNO.

        DOS.
           IF LI < 1 MOVE 3 TO LI.
           IF LI > 3 MOVE 1 TO LI.
           PERFORM FILA.
           DISPLAY ELEMEN (LI) LINE FI POSITION 58 LOW REVERSE
           CONTROL "WINDOW-CREATE, FCOLOR = WHITE, BCOLOR = BLACK".

       TRES.
           CALL "pikoratc" USING X, Y, RES, BOTON.
           DISPLAY ELEMEN (LI) LINE FI POSITION 58 LOW
           CONTROL "WINDOW-CREATE, FCOLOR = WHITE, BCOLOR = BLACK".
           IF X = 18 AND Y >=  58 AND Y <= 69 MOVE 1 TO LI GO CUATRO.
           IF X = 19 AND Y >=  58 AND Y <= 69 MOVE 2 TO LI GO CUATRO.
           IF X = 20 AND Y >=  58 AND Y <= 69 MOVE 3 TO LI GO CUATRO.

           IF RES = 72 SUBTRACT 1 FROM LI GO DOS.
           IF RES = 80 ADD 1 TO LI GO DOS.
           IF RES = 01 MOVE 07 TO LI GO CUATRO.
           IF RES = 28 NEXT SENTENCE ELSE GO DOS.

        CUATRO.
            IF LI = 1
               CALL   "MLISP1-S"
               CANCEL "MLISP1-S"
              ELSE IF LI = 2
               CALL   "MLISI1-S"
               CANCEL "MLISI1-S"
             END-IF.
               EXIT PROGRAM.
      *         STOP RUN.

