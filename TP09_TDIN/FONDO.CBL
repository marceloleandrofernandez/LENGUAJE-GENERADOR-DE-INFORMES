      **** GENERA RECUADRO EN EL LUGAR QUE LE INDICO ********
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FONDO.
      ***********************************************************************
      ***********************************************************************
       DATA DIVISION.
      ***********************************************************************
       WORKING-STORAGE SECTION.
      ***********************************************************************
       77 pX   PIC 99.
       77 pY   PIC 99.
       77 Char PIC X.

       01 arrColor.
        02 COLOR PIC X(30) OCCURS 5 TIMES.
      ***********************************************************************

       LINKAGE SECTION.
      ***********************************************************************
       77 pizq    PIC 99.
       77 pTop    PIC 99.
       77 pder    PIC 99.
       77 paba    PIC 99.
       77 pColor  PIC 9.
      ***********************************************************************

      ***********************************************************************
       PROCEDURE DIVISION USING pizq, pTop, pder, paba, pColor.
      ***********************************************************************
       INIT.
      ***********************************************************************
      *-- PARA DAR COLOR A LA SOMBRA--*

           DISPLAY "�" LINE pTop POSITION pizq
           CONTROL "BCOLOR = WHITE, FCOLOR = BLACK".

      *-- ASIGNA LOS COLORES S/EL N�MERO --*

           MOVE "BCOLOR = BLUE,  FCOLOR = WHITE" TO COLOR(1).
           MOVE "BCOLOR = WHITE, FCOLOR = WHITE" TO COLOR(2).
           MOVE "BCOLOR = RED,   FCOLOR = WHITE" TO COLOR(3).
           MOVE "BCOLOR = GREEN, FCOLOR = WHITE" TO COLOR(4).
           MOVE "BCOLOR = BLACK, FCOLOR = WHITE" TO COLOR(5).

           MOVE pder TO pX.
           MOVE pTop   TO pY.

           ADD 1 TO pX.
           ADD 1 TO pY.

           PERFORM DRAW-SHADOW-V UNTIL pY = paba  + 2.

           MOVE pizq TO pX.
           MOVE paba TO pY.

           ADD 1 TO pX.
           ADD 1 TO pY.

           PERFORM DRAW-SHADOW-H UNTIL pX = pder + 2.

           MOVE pizq TO pX.
           MOVE pTop  TO pY.



      *-- DA EL COLOR AL FONDO DEL CUADRO --*
           DISPLAY "�" LINE pTop POSITION pizq CONTROL COLOR(pColor).

           PERFORM DRAW-FRAME UNTIL pY = paba + 1.

      *-- V�RTICE IZQ. - ARRIBA --*
           DISPLAY "�" LINE pTop  POSITION pizq.
      *-- V�RTICE DER. - ARRIBA --*
           DISPLAY "�" LINE pTop  POSITION pder.
      *-- V�RTICE IZQ. - ABAJO --*
           DISPLAY "�" LINE paba POSITION pizq.
      *-- V�RTICE DER. - ABAJO --*
           DISPLAY "�" LINE paba POSITION pder.
           
           EXIT PROGRAM.
      ***********************************************************************

       DRAW-SHADOW-V.
      ***********************************************************************
      *-- SOMBRA DERECHA --*
           DISPLAY "�" LINE pY POSITION pX.
           ADD 1 TO pY.
      **********************************************************************

       DRAW-SHADOW-H.
      ***********************************************************************
      *-- SOMBRA ABAJO --*
           DISPLAY "�" LINE pY POSITION pX.
           ADD 1 TO pX.
      ***********************************************************************

       DRAW-FRAME.
      ***********************************************************************
           PERFORM DRAW-FRAME1 UNTIL pX = pder + 1.
           ADD  1     TO pY.
           MOVE pizq TO pX.
      ***********************************************************************

       DRAW-FRAME1.
      ***********************************************************************
      *-- RAYA: IZQ. Y DER.--*
           IF pX = pizq OR pX = pder
           MOVE "�" TO Char
           ELSE

      *-- RAYA: ARRIBA Y ABAJO --*
            IF pY = pTop OR pY = paba
            MOVE "�" TO Char
            ELSE
             MOVE " " TO Char
            END-IF

           END-IF.

           DISPLAY Char LINE pY POSITION pX.
           ADD 1 TO pX.
      ***********************************************************************
