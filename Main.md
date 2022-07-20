# TestCobol
Test cobol CGM

       PROCESS APOST
       IDENTIFICATION DIVISION.
       PROGRAM-ID.         PSORT001.
      *
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        IBM-AS400.
       OBJECT-COMPUTER.        IBM-AS400.
       SPECIAL-NAMES. REQUESTOR     IS CONSOLE
                      DECIMAL-POINT IS COMMA
                      I-O-FEEDBACK  IS FEEDBACK-AREA.
      *
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *
       01  I1             PIC 9(02).
      *
       01  WKA-TABLE-N.
           05  WKA-REC    PIC 9(09) OCCURS 5.
      *
       01  WKB-TABLE-A.
           05  WKB-REC    PIC X(02) OCCURS 5.
      *
       01  WKC-TABLE.
           05  WKC-REC              OCCURS 5.
               10  WKC-CAMPO-A      PIC X(02).
               10  WKC-CAMPO-B      PIC 9(09).
               10  WKC-CAMPO-C      PIC X(05).
      *
       01  AREA-SORT.
           03   SORT-RC         PIC 9(02).
           03   SORT-TIPO-KEY   PIC X(01).
                88  SORT-TIPO-KEY-N       VALUE 'N'.
                88  SORT-TIPO-KEY-A       VALUE 'A'.
           03   SORT-KEY-INIZIO PIC 9(02).
           03   SORT-KEY-LEN    PIC 9(02).
           03   SORT-TAB.
                10   SORT-REC   PIC X(50) OCCURS 5.
      *
      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAIN-PROGRAM.
           PERFORM INIZIO
           PERFORM ELABORAZIONE
           PERFORM FINE-ELAB
           GOBACK.
      *
      *----------------------------------------------------------------*
      *    S O T T O P R O G R A M M I                                 *
      *----------------------------------------------------------------*
       INIZIO.
      *
            MOVE 10                  TO WKA-REC(1).
            MOVE 6                   TO WKA-REC(2).
            MOVE 1                   TO WKA-REC(3).
            MOVE 2                   TO WKA-REC(4).
            MOVE 7                   TO WKA-REC(5).
      *
            MOVE '10'                TO WKB-REC(1).
            MOVE '6'                 TO WKB-REC(2).
            MOVE '1'                 TO WKB-REC(3).
            MOVE '2'                 TO WKB-REC(4).
            MOVE '7'                 TO WKB-REC(5).
      *
            MOVE '10'                TO WKC-CAMPO-A(1)
            MOVE '6'                 TO WKC-CAMPO-A(2)
            MOVE '1'                 TO WKC-CAMPO-A(3)
            MOVE '2'                 TO WKC-CAMPO-A(4)
            MOVE '7'                 TO WKC-CAMPO-A(5)
            MOVE 10                  TO WKC-CAMPO-B(1)
            MOVE 6                   TO WKC-CAMPO-B(2)
            MOVE 1                   TO WKC-CAMPO-B(3)
            MOVE 2                   TO WKC-CAMPO-B(4)
            MOVE 7                   TO WKC-CAMPO-B(5)
            MOVE '10'                TO WKC-CAMPO-C(1)
            MOVE '6'                 TO WKC-CAMPO-C(2)
            MOVE '1'                 TO WKC-CAMPO-C(3)
            MOVE '2'                 TO WKC-CAMPO-C(4)
            MOVE '7'                 TO WKC-CAMPO-C(5).
      *
      *----------------------------------------------------------------*
      *    Routine principale di elaborazione                          *
      *----------------------------------------------------------------*
       ELABORAZIONE.
      *
           PERFORM SORT-NUM.
           PERFORM SORT-ALFA.
           PERFORM SORT-TABELLA.
      *
      *----------------------------------------------------------------*
       SORT-NUM.
      *
           INITIALIZE AREA-SORT.
           SET     SORT-TIPO-KEY-N         TO TRUE
           MOVE    1                       TO SORT-KEY-INIZIO
           MOVE    9                       TO SORT-KEY-LEN
           PERFORM VARYING I1 FROM 1 BY 1
             UNTIL I1 > 5
                   MOVE WKA-REC(I1)        TO SORT-REC(I1)
           END-PERFORM.
      *
           CALL 'SORT001' USING AREA-SORT.
      *
           PERFORM VARYING I1 FROM 1 BY 1
             UNTIL I1 > 5
                   DISPLAY 'POS:' I1 '-VALORE:' SORT-REC(I1)
           END-PERFORM.
      *
      *----------------------------------------------------------------*
       SORT-ALFA.
      *
           INITIALIZE AREA-SORT.
           SET     SORT-TIPO-KEY-A         TO TRUE
           MOVE    1                       TO SORT-KEY-INIZIO
           MOVE    2                       TO SORT-KEY-LEN
           PERFORM VARYING I1 FROM 1 BY 1
             UNTIL I1 > 5
                   MOVE WKB-REC(I1)        TO SORT-REC(I1)
           END-PERFORM.
      *
           CALL 'SORT001' USING AREA-SORT.
      *
           PERFORM VARYING I1 FROM 1 BY 1
             UNTIL I1 > 5
                   DISPLAY 'POS:' I1 '-VALORE:' SORT-REC(I1)
           END-PERFORM.
      *
      *----------------------------------------------------------------*
       SORT-TABELLA.
      *
           INITIALIZE AREA-SORT.
           SET     SORT-TIPO-KEY-N         TO TRUE
           MOVE    3                       TO SORT-KEY-INIZIO
           MOVE    9                       TO SORT-KEY-LEN
           PERFORM VARYING I1 FROM 1 BY 1
             UNTIL I1 > 5
                   MOVE WKC-REC(I1)        TO SORT-REC(I1)
           END-PERFORM.
      *
           CALL 'SORT001' USING AREA-SORT.
      *
           PERFORM VARYING I1 FROM 1 BY 1
             UNTIL I1 > 5
                   DISPLAY 'POS:' I1 '-VALORE:' SORT-REC(I1)
           END-PERFORM.
      *
      *---------------------------------------------------------------*
      *    ROUTINE DI FINE LAVORO                                     *
      *---------------------------------------------------------------*
       FINE-ELAB.
      *
