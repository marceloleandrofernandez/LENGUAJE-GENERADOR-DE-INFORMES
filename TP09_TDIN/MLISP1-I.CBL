      *** VISUALIZAR MAESTRO -ENTRADA ARCHIVO INDEXADO *****
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MLISP1-I.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT MAESTRO ASSIGN TO DISK "PERSONA.IND"
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS MCA
                ALTERNATE RECORD KEY IS MAN DUPLICATES
                FILE STATUS IS FSTM.

            SELECT MAESUC ASSIGN TO DISK "SUCURSAL.IND"
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS MCSUC
                ALTERNATE RECORD KEY IS MDSUC DUPLICATES
                FILE STATUS IS FSTSUC.

	       SELECT IMPRE ASSIGN TO PRINT  "LPT1".
       DATA DIVISION.
       FILE SECTION.
        FD MAESTRO
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 45 CHARACTERS
           DATA RECORD IS R-MAE.

         01 R-MAE.
            02 MCA  PIC 9(08).
            02 MAN  PIC X(30).
            02 MSA  PIC 9(05).
            02 MTS  PIC 9.
            02 MSUC  PIC 9(1).

       FD   IMPRE
	    LABEL RECORD IS OMITTED
	    RECORD CONTAINS 80 CHARACTERS
	    LINAGE 24 FOOTING 20

	    DATA RECORD IS R-IMP.
        01   R-IMP  PIC X(80).

       FD MAESUC
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 16 CHARACTERS
           DATA RECORD IS R-SUC.

       01 R-SUC.
            02 MCSUC     PIC 9(1).
            02 MDSUC    PIC X(15).

       WORKING-STORAGE SECTION.
       77 FSTM  		     PIC XX.
       77 FSTSUC             PIC XX.
       77 DEBE               PIC 9(04) VALUE 0.
       77 MTSD               PIC 9(05) VALUE 0.
       77 MAYD               PIC 9(04) VALUE 0.
       77 MAND               PIC X(18).
       77 MTED               PIC X(15).
       77 HABER              PIC 9(04) VALUE 0.
       77 MTSH               PIC 9(05) VALUE 0.
       77 MENH               PIC 9(04) VALUE 9999.
       77 MANH               PIC X(18).
       77 MTEH               PIC X(15).
       77 WDSUC              PIC X(15).
       77 L		             PIC 9(02) VALUE 0.
       77 C-T		         PIC 99.
       77 TECLA 	         PIC X.

       01  WFECHA.
    	   02 WAA		  PIC 99.
    	   02 WMM		  PIC 99.
    	   02 WDD		  PIC 99.

       01 TITULO-CABECERA.
    	  02 FILLER PIC X(30) VALUE  SPACES.
    	  02 FILLER PIC X(20) VALUE  "INFORME POR CLIENTES".
    	  02 FILLER PIC X(20) VALUE  SPACES.
    	  02 SDD    PIC Z9/.
    	  02 SMM    PIC 99/.
    	  02 SAA    PIC 9(2).

       01 TITULOS-DETALLES.
    	  02 FILLER PIC X(07) VALUE  "CODIGO ".
    	  02 FILLER PIC X(05) VALUE  SPACES.
    	  02 FILLER PIC X(18) VALUE  "APELLIDO Y NOMBRES".
    	  02 FILLER PIC X(10) VALUE  SPACES.
    	  02 FILLER PIC X(04) VALUE  "DEBE".
    	  02 FILLER PIC X(10) VALUE  SPACES.
    	  02 FILLER PIC X(05) VALUE  "HABER".
    	  02 FILLER PIC X(10) VALUE  SPACES.
    	  02 FILLER PIC X(08) VALUE  "SUCURSAL".

       01  SUBRAYAR-TITULOS-DETALLES.
    	   02 FILLER PIC X(07) VALUE ALL "-".
    	   02 FILLER PIC X(05) VALUE  SPACES.
    	   02 FILLER PIC X(18) VALUE ALL "-".
    	   02 FILLER PIC X(10) VALUE  SPACES.
    	   02 FILLER PIC X(04) VALUE ALL "-".
    	   02 FILLER PIC X(10) VALUE  SPACES.
    	   02 FILLER PIC X(05) VALUE ALL "-".
    	   02 FILLER PIC X(10) VALUE  SPACES.
    	   02 FILLER PIC X(08) VALUE ALL "-".

       01  SALIDAS-DE-DETALLES.
    	   02 DCA    PIC 9(08).
    	   02 FILLER PIC X(05) VALUE  SPACES.
    	   02 DAN    PIC X(18).
    	   02 FILLER PIC X(09) VALUE  SPACES.
    	   02 DTSD   PIC 9(04).
    	   02 FILLER PIC X(10) VALUE  SPACES.
    	   02 DTSH   PIC 9(04).
    	   02 FILLER PIC X(10) VALUE  SPACES.
    	   02 DTE    PIC X(15).

       01  SALIDA-DE-TOTALES1.
    	   02 FILLER PIC X(22) VALUE SPACES.
    	   02 FILLER PIC X(11) VALUE " TOTALES = ".
    	   02 FILLER PIC X(06) VALUE SPACES.
    	   02 STSD   PIC ZZ.ZZ9.
    	   02 FILLER PIC X(08) VALUE SPACES.
    	   02 STSH   PIC ZZ.ZZ9.

       01  SALIDA-DE-TOTALES2.
    	   02 FILLER PIC X(05) VALUE SPACES.
    	   02 FILLER PIC X(08) VALUE "EL MAYOR".
    	   02 FILLER PIC X(11) VALUE " DEBE ES = ".
    	   02 SMAYD  PIC 9(04).
    	   02 FILLER PIC X(22) VALUE SPACES.

       01  SALIDA-DE-TOTALES3.
    	   02 FILLER PIC X(05) VALUE SPACES.
    	   02 FILLER PIC X(14) VALUE "CORRESPONDE AL".
    	   02 FILLER PIC X(11) VALUE " CLIENTE = ".
    	   02 SAND   PIC X(18).
    	   02 FILLER PIC X(22) VALUE SPACES.

       01  SALIDA-DE-TOTALES4.
    	   02 FILLER PIC X(05) VALUE SPACES.
    	   02 FILLER PIC X(16) VALUE "SU IMPORTE ES = ".
    	   02 SSAD   PIC X(04).
    	   02 FILLER PIC X(22) VALUE SPACES.

       01  SALIDA-DE-TOTALES5.
    	   02 FILLER PIC X(05) VALUE SPACES.
    	   02 FILLER PIC X(17) VALUE "LA SUCURSAL ES = ".
    	   02 STED   PIC X(15).
    	   02 FILLER PIC X(22) VALUE SPACES.

       01  SALIDA-DE-TOTALES6.
    	   02 FILLER PIC X(05) VALUE SPACES.
    	   02 FILLER PIC X(08) VALUE "EL MENOR".
    	   02 FILLER PIC X(12) VALUE " HABER ES = ".
    	   02 SMENH  PIC 9(04).
    	   02 FILLER PIC X(22) VALUE SPACES.

       01  SALIDA-DE-TOTALES7.
    	   02 FILLER PIC X(05) VALUE SPACES.
    	   02 FILLER PIC X(14) VALUE "CORRESPONDE AL".
    	   02 FILLER PIC X(11) VALUE " CLIENTE = ".
    	   02 SANH   PIC X(18).
    	   02 FILLER PIC X(22) VALUE SPACES.

       01  SALIDA-DE-TOTALES8.
    	   02 FILLER PIC X(05) VALUE SPACES.
    	   02 FILLER PIC X(16) VALUE "SU IMPORTE ES = ".
    	   02 SSAH   PIC X(04).
    	   02 FILLER PIC X(22) VALUE SPACES.

       01  SALIDA-DE-TOTALES9.
    	   02 FILLER PIC X(05) VALUE SPACES.
    	   02 FILLER PIC X(17) VALUE "LA SUCURSAL ES = ".
    	   02 STEH   PIC X(15).
    	   02 FILLER PIC X(22) VALUE SPACES.



       PROCEDURE DIVISION.
       INICIO.
	    OPEN I-O MAESTRO
                   OUTPUT IMPRE
	    ACCEPT WFECHA FROM DATE.
       SALIDA-TITULOS.
	    PERFORM IMPRIMIR-TITULOS.


       PROCESO.
	    PERFORM LEER-MAESTRO.
	    PERFORM PROCESAR-REGISTRO.
	    PERFORM MOVER-A-SALIDA.
	    PERFORM IMPRIMIR-DETALLES.
	    GO TO PROCESO.
       LEER-MAESTRO.
	      READ MAESTRO NEXT RECORD AT END GO TO IMPRIMIR-TOTALES.

       PROCESAR-REGISTRO.
            OPEN I-O MAESUC
            MOVE MSUC TO MCSUC
            START MAESUC KEY IS EQUAL MCSUC
                 INVALID KEY MOVE "NO" TO WDSUC.

            READ MAESUC NOT INVALID
                 MOVE "SI" TO WDSUC
                 MOVE  MDSUC TO WDSUC
           CLOSE MAESUC.

            IF MTS = 1
               MOVE MSA TO DEBE
               MOVE "0" TO HABER
               ADD MSA TO MTSD
               IF MSA > MAYD
                  MOVE MSA TO MAYD
                  MOVE MAN TO MAND
                  MOVE WDSUC TO MTED
               END-IF
            ELSE IF MTS = 2
                 MOVE MSA TO HABER
                 MOVE "0" TO DEBE
                 ADD MSA TO MTSH
                 IF MSA < MENH
                    MOVE MSA TO MENH
                    MOVE MAN TO MANH
                    MOVE WDSUC TO MTEH
                 END-IF
            END-IF.

        MOVER-A-SALIDA.
	        MOVE MCA TO DCA.
	        MOVE MAN TO DAN.
	        MOVE DEBE TO DTSD.
            MOVE HABER TO DTSH.
            MOVE WDSUC TO DTE.



       IMPRIMIR-TITULOS.
	    DISPLAY " " ERASE.
	    MOVE 2 TO L.
	    MOVE SPACES TO R-IMP.
	    MOVE WDD TO SDD.
	    MOVE WMM TO SMM.
	    MOVE WAA TO SAA.
	    MOVE TITULO-CABECERA TO R-IMP.
	    DISPLAY R-IMP LINE L CONTROL "FCOLOR=WHITE,BCOLOR=BLACK".
		MOVE SPACES TO R-IMP.
		MOVE TITULOS-DETALLES TO R-IMP.
		ADD 1 TO L.
	      DISPLAY R-IMP LINE L CONTROL "FCOLOR=WHITE,BCOLOR=BLACK".
		MOVE SPACES  TO R-IMP.
		ADD 1 TO L.
		MOVE SUBRAYAR-TITULOS-DETALLES TO R-IMP.
		DISPLAY R-IMP LINE L.

        IMPRIMIR-DETALLES.
	       MOVE SPACES TO R-IMP.
	       MOVE SALIDAS-DE-DETALLES TO R-IMP.
	       ADD 1 TO L.
	       DISPLAY R-IMP LINE L CONTROL "FCOLOR=GREEN,BCOLOR=BLACK".
      ** para indicar cada cuantas va el titulo **
      	       IF L = 24 OR L > 24
      		PERFORM ING-TECLA
              END-IF.
       ING-TECLA.
	    ADD 2 TO L.
	    DISPLAY "PULSE UNA TECLA PARA CONTINUAR", LINE L
	    CONTROL "FCOLOR=RED,BCOLOR=BLACK"
	    ACCEPT TECLA
		  IF FSTM NOT = "10"
		     PERFORM IMPRIMIR-TITULOS
		  END-IF.

       IMPRIMIR-TOTALES.
            MOVE MTSD TO STSD.
            MOVE MTSH TO STSH.
	        MOVE MAYD TO SMAYD.
	        MOVE MAND TO SAND.
	        MOVE MAYD TO SSAD.
	        MOVE MTED TO STED.
	        MOVE MENH TO SMENH.
	        MOVE MANH TO SANH.
	        MOVE MENH TO SSAH.
	        MOVE MTEH TO STEH.
            MOVE SPACES TO R-IMP.
	        MOVE SALIDA-DE-TOTALES1 TO R-IMP.
	           ADD 2 TO L.
	       DISPLAY R-IMP LINE L.
	       MOVE SALIDA-DE-TOTALES2 TO R-IMP.
	           ADD 2 TO L.
	       DISPLAY R-IMP LINE L.
	       MOVE SALIDA-DE-TOTALES3 TO R-IMP.
	           ADD 1 TO L.
	       DISPLAY R-IMP LINE L.
	       MOVE SALIDA-DE-TOTALES4 TO R-IMP.
	           ADD 1 TO L.
	       DISPLAY R-IMP LINE L.
	       MOVE SALIDA-DE-TOTALES5 TO R-IMP.
	           ADD 1 TO L.
	       DISPLAY R-IMP LINE L.
	       MOVE SALIDA-DE-TOTALES6 TO R-IMP.
	           ADD 2 TO L.
	       DISPLAY R-IMP LINE L.
	       MOVE SALIDA-DE-TOTALES7 TO R-IMP.
	           ADD 1 TO L.
	       DISPLAY R-IMP LINE L.
	       MOVE SALIDA-DE-TOTALES8 TO R-IMP.
	           ADD 1 TO L.
	       DISPLAY R-IMP LINE L.
	       MOVE SALIDA-DE-TOTALES9 TO R-IMP.
	           ADD 1 TO L.
	       DISPLAY R-IMP LINE L.
	       PERFORM ING-TECLA.



       FIN.
	    DISPLAY " " ERASE CONTROL "FCOLOR=WHITE,BCOLOR=BLACK".
	    IF	FSTM  > "00" AND FSTM NOT = "10"
	    DISPLAY " ERROR EN LA APERTURA " FSTM LINE 23, POSITION 26
	    END-IF.
	    CLOSE MAESTRO IMPRE.
            EXIT PROGRAM.
      *      STOP RUN.















