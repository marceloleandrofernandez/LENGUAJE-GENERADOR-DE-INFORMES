      ** PARA LA OPCION SI-NO **
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU-SN.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77 TECLA PIC X.
      *--------------------------------------------------------*
       77 X PIC 99 BINARY VALUE 10.
       77 Y PIC 99 BINARY VALUE 10.
      *--------------------------------------------------------*
       77 RES PIC 99 BINARY VALUE 10.
       77 BOTON PIC 9 BINARY.
       77 SUBRAYADO PIC X(80) VALUE ALL "Í".
       01 LIN    PIC 99 VALUE 22.
       01 FI PIC 99.
       01 PO PIC 99.
       01 LI PIC 99.
      *--------------------------------------------------------*

       01 TABLA-OPCIONES.
           02 FILLER PIC X(05) VALUE " SI  ".
           02 FILLER PIC X(05) VALUE " NO  ".
       01 RTABLA REDEFINES TABLA-OPCIONES.
           02 ELEMENT PIC X(05) OCCURS 2 TIMES.

       LINKAGE SECTION.
       77 OP-SN PIC X.

       SCREEN SECTION.
       PROCEDURE DIVISION USING OP-SN.
       INICIO.
           MOVE SPACES TO OP-SN.
           MOVE 23 TO PO.
           MOVE 1 TO LI.
      *---------------------  IZ AR DE AB COLOR
           CALL "FONPR" USING 62 22 78 24 3.
           CANCEL "FONPR".
       FILA.
           IF LI = 1 OR LI > 2 MOVE 65 TO FI.
           IF LI = 2 OR LI < 1 MOVE 72 TO FI.

       UNO.
           DISPLAY ELEMENT(LI) LINE PO POSITION FI LOW
           CONTROL "WINDOWS-CREATE, FCOLOR = WHITE, BCOLOR = BLACK".
           ADD 1 TO LI ADD 7 TO FI IF LI > 2 NEXT SENTENCE ELSE GO UNO.

       DOS.
           IF LI < 1 MOVE 2 TO LI.
           IF LI > 2 MOVE 1 TO LI.
           PERFORM FILA.
           DISPLAY ELEMENT(LI) LINE PO POSITION FI LOW REVERSE
           CONTROL "WINDOWS-CREATE, FCOLOR = WHITE, BCOLOR = BLACK".

       TRES.
           CALL "PIKORATC" USING X, Y, RES, BOTON.
           DISPLAY ELEMENT(LI) LINE PO POSITION FI LOW
           CONTROL "WINDOWS-CREATE, FCOLOR = WHITE, BCOLOR = BLACK".
           IF X = LIN AND Y >= 65 AND Y <= 69 MOVE 1 TO LI GO CUATRO.
           IF X = LIN AND Y >= 72 AND Y <= 76 MOVE 2 TO LI GO CUATRO.
           IF RES = 75 SUBTRACT 1 FROM LI GO DOS.
           IF RES = 77 ADD 1 TO LI GO DOS.
           IF RES = 1  GO CUATRO.
           IF RES = 28 NEXT SENTENCE ELSE GO DOS.

       CUATRO.
           IF LI = 1 MOVE "S" TO OP-SN.
           IF LI = 2 MOVE "N" TO OP-SN.
           DISPLAY "  " CONTROL "FCOLOR = BLACK , BCOLOR = GREEN"
           EXIT PROGRAM.
