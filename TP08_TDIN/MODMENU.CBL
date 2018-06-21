      *** MENU PRINCIPAL DESPLEGABLE ***
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODMEN2.
       DATA DIVISION.
       FILE SECTION.

        WORKING-STORAGE SECTION.
       77 X      PIC 99 binary value 10.
       77 Y      PIC 99 binary value 10.
       77 RES    PIC 99 binary value 10.
       77 BOTON  PIC 9 BINARY.

       01 LI     PIC 99.
       01 CO     PIC 99.
       01 OP2    PIC 9.
      *--------- PARA TRATAR EL MOVIMIENTO DEL PROG. OPC -------------*
       77 MOV   PIC 9 VALUE 0.
       77 TECLA             PIC X.
       77 RECTA1 PIC X(80) VALUE ALL "Ü".
       77 RECTA2 PIC X(80) VALUE ALL "Í".
       77 LINEA  PIC X(80) VALUE ALL "±".

       01 TABLA-OPCIONES.
           02 FILLER PIC X(15) VALUE "      ABM      ".
           02 FILLER PIC X(15) VALUE "    LISTADO    ".
           02 FILLER PIC X(15) VALUE "     SALIR     ".
       01 R-TABLA REDEFINES TABLA-OPCIONES.
           02 ELEMEN PIC X(15) OCCURS 3 TIMES.

       SCREEN SECTION.
       01  P-BORRAR.
           02 BLANK SCREEN.

       01  P-MENU.
           02 BLANK SCREEN.
           02 LINE 03 COLUMN 01 REVERSE PIC X(80) FROM LINEA.
           02 LINE 05 COLUMN 01 REVERSE PIC X(80) FROM LINEA.
           02 LINE 23 COLUMN 01 REVERSE PIC X(80) FROM LINEA.

       PROCEDURE DIVISION.
       INICIO.
           MOVE 0 TO LI MOV.
           MOVE 1 TO CO.
       MENU.
           DISPLAY P-MENU.
           call   "titulo"
           cancel "titulo".
       COLUMNA.
           IF CO =  1 MOVE 01 TO LI.
           IF CO = 33 MOVE 02 TO LI.
           IF CO = 65 MOVE 03 TO LI.

       UNO.
           DISPLAY ELEMEN (LI) LINE 4 POSITION CO LOW
           CONTROL "WINDOW-CREATE, FCOLOR = BLACK, BCOLOR = GREEN".
           ADD 1 TO LI ADD 32 TO CO
           IF CO > 65 NEXT SENTENCE ELSE GO UNO.

       DOS.
           IF CO = 31 MOVE 65 TO CO.
           IF CO > 65 MOVE  1 TO CO.
           PERFORM COLUMNA.
           DISPLAY ELEMEN (LI) LINE 4 POSITION CO LOW REVERSE
           CONTROL "WINDOW-CREATE, FCOLOR = BLACK, BCOLOR = GREEN".

       TRES.
           CALL "pikoratc" USING X, Y, RES, BOTON.
           DISPLAY ELEMEN (LI) LINE 4 POSITION CO LOW
           CONTROL "WINDOW-CREATE, FCOLOR = BLACK, BCOLOR = GREEN".
           IF X = 04 AND Y >= 01 AND Y <= 15 MOVE 01 TO LI GO CUATRO.
           IF X = 04 AND Y >= 33 AND Y <= 47 MOVE 02 TO LI GO CUATRO.
           IF X = 04 AND Y >= 65 AND Y <= 79 MOVE 03 TO LI GO CUATRO.
      *------  Escape ---------*
           IF RES = 01 MOVE 3 TO LI GO CUATRO.
      *------  flecha izquierda ---------*
           IF RES = 75
           SUBTRACT 32 FROM CO GO DOS.
      *------  flecha derecha   ---------*
           IF RES = 77
            ADD 32 TO CO GO DOS.
      *------  Enter arriba ---------*
           IF RES = 28 NEXT SENTENCE ELSE GO MENU.

       CUATRO.
           IF MOV = 3
              MOVE MOV TO LI
              GO INICIO
           END-IF.
           IF (LI = 1 AND MOV = 0) OR MOV = 1
              CALL   "titulo"
              CANCEL "titulo"
              CALL "OPC-ABM" USING MOV
              CANCEL "OPC-ABM"
              DISPLAY "  " CONTROL "FCOLOR = GREEN, BCOLOR = BLACK"
              GO CUATRO
           END-IF.
           IF (LI = 2 AND MOV = 0) OR MOV = 2
              CALL   "titulo"
              CANCEL "titulo"
              CALL "OPC-LIS" USING MOV
              CANCEL "OPC-LIS"
              DISPLAY "  " CONTROL "FCOLOR = GREEN, BCOLOR = BLACK"
              GO CUATRO
           END-IF.
           IF (LI = 3 AND MOV = 0)
           STOP RUN
           END-IF.

      ** TECLAS PARA DERECHA E IZQUIERDA SON 4 DE Y 19 IZ EN ASCII
