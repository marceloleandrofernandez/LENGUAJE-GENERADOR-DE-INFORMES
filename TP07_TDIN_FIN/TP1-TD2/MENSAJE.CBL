       IDENTIFICATION DIVISION.
       PROGRAM-ID. REC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 F PIC 99 .
       77 C PIC 99 .
       77 DC PIC X VALUE "Û".
       77 CO2 PIC 99 .
       77 FI2 PIC 99 .
       77 COLOR1 PIC X(38) .
       77 W PIC 99 .
       77 WC PIC 99 .


       LINKAGE SECTION.
       77 EF PIC 99 .
       77 EC PIC 99 .
       77 CO PIC 99 .
       77 FI PIC 99 .
       77 CLS PIC X .


       PROCEDURE DIVISION USING EC EF CO FI CLS.
       INICIO.

           COMPUTE CO2 = CO - EC + 1.
           COMPUTE FI2 = FI - EF + 1.
           MOVE EF TO F.
           MOVE EC TO C.

           EVALUATE CLS

           WHEN "X"
           MOVE "FCOLOR = WHITE,  BCOLOR = BLACK"   TO COLOR1

           WHEN "Y"
           MOVE "FCOLOR = WHITE,  BCOLOR = WHITE"   TO COLOR1

           WHEN "N"
           MOVE "FCOLOR = BLACK,  BCOLOR = WHITE"   TO COLOR1

           WHEN "G"
           MOVE "FCOLOR = WHITE,  BCOLOR = GREEN"   TO COLOR1

           WHEN "B"
           MOVE "FCOLOR = WHITE,  BCOLOR = BLUE "   TO COLOR1

           WHEN "S"
           MOVE "FCOLOR = BLUE ,  BCOLOR = BLUE "   TO COLOR1

           WHEN "Z"
           MOVE "FCOLOR = BLACK,  BCOLOR = BLACK"   TO COLOR1

           WHEN "C"
           MOVE "FCOLOR = BLACK,  BCOLOR = CYAN "   TO COLOR1

           WHEN "W"
           MOVE "FCOLOR = WHITE,  BCOLOR = RED  "   TO COLOR1

           WHEN "M"
           MOVE "FCOLOR = MAGENTA,  BCOLOR = BLACK" TO COLOR1

           END-EVALUATE.

           PERFORM FI2 TIMES
           PERFORM CO2 TIMES
           EVALUATE C
           WHEN  EC
                 EVALUATE F
                 WHEN EF
                    DISPLAY "Ú" LINE F POSITION C
                    CONTROL COLOR1
                 WHEN FI
                    DISPLAY "À" LINE F POSITION C
                    CONTROL COLOR1
                 WHEN OTHER
                    DISPLAY "³" LINE F POSITION C
                    CONTROL COLOR1
                 END-EVALUATE
           WHEN  CO
                 EVALUATE F
                 WHEN EF
                   DISPLAY "¿" LINE F POSITION C
                   CONTROL COLOR1
                 COMPUTE W = CO + 1
                 WHEN FI
                   DISPLAY "Ù" LINE F POSITION C
                   CONTROL COLOR1

                 WHEN OTHER
                   DISPLAY "³" LINE F POSITION C
                   CONTROL COLOR1
                 END-EVALUATE
           WHEN  OTHER
                 EVALUATE F
                 WHEN EF
                   DISPLAY "Ä" LINE F POSITION C
                   CONTROL COLOR1
                 WHEN FI
                   DISPLAY "Ä" LINE F POSITION C
                   CONTROL COLOR1
                  WHEN OTHER
                   DISPLAY "Û" LINE F POSITION C LOW REVERSE
                   CONTROL COLOR1
                 END-EVALUATE
           END-EVALUATE
           COMPUTE C = C + 1
           END-PERFORM
           MOVE EC TO C
           COMPUTE F = F + 1
           END-PERFORM.
        FIN.

           GOBACK.
