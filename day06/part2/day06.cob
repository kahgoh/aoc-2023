      *Day06 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY06.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT FD-INPUTS ASSIGN TO 'input.data'
         ORGANIZATION IS LINE SEQUENTIAL. 
       DATA DIVISION.
       FILE SECTION.
       FD FD-INPUTS.
       01 BOAT-RECORD.
           05 COL-TIME             PIC 9(08).
           05 COL-DIST             PIC 9(17).
       WORKING-STORAGE SECTION.
       01 WS-EOF                   PIC X VALUE 'N'.
       01 WS-BOAT-RES              PIC 9(17).
       01 WS-TIME                  PIC 9(08).
       01 WS-DIST                  PIC 9(17).
       PROCEDURE DIVISION.
         OPEN INPUT FD-INPUTS.
         
         PERFORM UNTIL WS-EOF = 'Y'
           READ FD-INPUTS
             AT END
               MOVE 'Y' TO WS-EOF
           NOT AT END
               PERFORM PROCESS-BOAT
           END-READ
         END-PERFORM.
         
         CLOSE FD-INPUTS.
         STOP RUN.
       
       PROCESS-BOAT.
         SET WS-TIME TO 0.
         SET WS-DIST TO 0.

      * Convert/Parse the numbers in the DIST and TIME columns   
         MOVE FUNCTION NUMVAL(COL-DIST) TO COL-DIST.
         MOVE FUNCTION NUMVAL(COL-TIME) TO COL-TIME.

         DISPLAY "Time: " COL-TIME
         DISPLAY "Dist: " COL-DIST

         PERFORM UNTIL COL-DIST < WS-DIST
           COMPUTE WS-TIME = WS-TIME + 1
           COMPUTE WS-DIST = (COL-TIME - WS-TIME) * WS-TIME
         END-PERFORM.
         
         COMPUTE WS-BOAT-RES = COL-TIME - (2*WS-TIME) + 1
         DISPLAY "Result: " WS-BOAT-RES.

