      *** MENU DE OPCIONES ELIMINAR-MODIFICAR-SALIR***
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUSN.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77 TIT    PIC X(20).
       77 X      PIC 99 binary value 10.
       77 Y      PIC 99 binary value 10.
       77 RES    PIC 99 binary value 10.
       77 BOTON  PIC 9 BINARY.

       01 LIN    PIC 99 VALUE 23.
       01 LI     PIC 99.
       01 CO     PIC 99.
       01 OP2    PIC 9.

       01 TABLA-OPCIONES.
           02 FILLER PIC X(10) VALUE " ELIMINAR ".
           02 FILLER PIC X(10) VALUE " MODIFICAR".
           02 FILLER PIC X(10) VALUE " CANCELAR ".
       01 R-TABLA REDEFINES TABLA-OPCIONES.
           02 ELEMEN PIC X(10) OCCURS 3 TIMES.

       LINKAGE SECTION.
       77 OP-EMC  PIC X.

       SCREEN SECTION.
       PROCEDURE DIVISION USING OP-EMC.
       INICIO.
           MOVE 22 TO CO.
           MOVE 23 TO LIN.
           CALL "FONPR" USING 18 22 60 24 3.
           CANCEL "FONPR".

       MENU.
       COLUMNA.
           IF CO = 22 MOVE 01 TO LI.
           IF CO = 35 MOVE 02 TO LI.
           IF CO = 48 MOVE 03 TO LI.

       UNO.
           DISPLAY ELEMEN(LI) LINE LIN POSITION CO LOW
           CONTROL "WINDOW-CREATE, FCOLOR = RED, BCOLOR = BLACK".
           ADD 1 TO LI ADD 13 TO CO
           IF CO > 56 NEXT SENTENCE ELSE GO UNO.

       DOS.
           IF CO < 22 MOVE 58 TO CO.
           IF CO > 58 MOVE 22 TO CO.
           PERFORM COLUMNA.
           DISPLAY ELEMEN(LI) LINE LIN POSITION CO LOW REVERSE
           CONTROL "WINDOW-CREATE, FCOLOR = RED, BCOLOR = BLACK".

       TRES.
           CALL "pikoratc" USING X, Y, RES, BOTON.
           DISPLAY ELEMEN (LI) LINE LIN POSITION CO LOW
           CONTROL "WINDOW-CREATE, FCOLOR = RED, BCOLOR = BLACK".
           IF X = LIN AND Y >= 22 AND Y <= 32 MOVE 01 TO LI GO CUATRO.
           IF X = LIN AND Y >= 35 AND Y <= 45 MOVE 02 TO LI GO CUATRO.
           IF X = LIN AND Y >= 48 AND Y <= 58 MOVE 03 TO LI GO CUATRO.
           IF RES = 75 SUBTRACT 13 FROM CO GO DOS.
           IF RES = 77 ADD 13 TO CO GO DOS.
           IF RES = 28 NEXT SENTENCE ELSE GO MENU.

       CUATRO.
           IF LI = 1 MOVE "B" TO OP-EMC.
           IF LI = 2 MOVE "M" TO OP-EMC.
           IF LI = 3 MOVE "C" TO OP-EMC.
           DISPLAY "  " CONTROL "FCOLOR = BLACK , BCOLOR = GREEN"
           EXIT PROGRAM.
