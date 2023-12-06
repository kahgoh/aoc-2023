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
           05 COL-TIME             PIC 9(07).
           05 COL-DIST             PIC 9(07).
       WORKING-STORAGE SECTION.
       01 WS-EOF                   PIC X VALUE 'N'.
       01 WS-RES                   PIC 9(15).
       01 WS-BOAT-RES              PIC 9(15).
       01 WS-TIME                  PIC 9(07).
       01 WS-DIST                  PIC 9(07).
       PROCEDURE DIVISION.
         SET WS-RES TO 1.
         OPEN INPUT FD-INPUTS.
         
         PERFORM UNTIL WS-EOF = 'Y'
           READ FD-INPUTS
             AT END
               MOVE 'Y' TO WS-EOF
           NOT AT END
               PERFORM PROCESS-BOAT
               COMPUTE WS-RES = WS-RES * WS-BOAT-RES
           END-READ
         END-PERFORM.
         
         CLOSE FD-INPUTS.
         DISPLAY "Result: " WS-RES.
         STOP RUN.
       
       PROCESS-BOAT.
         SET WS-TIME TO 0.
         SET WS-DIST TO 0.

      * Convert/Parse the numbers in the DIST and TIME columns   
         MOVE FUNCTION NUMVAL(COL-DIST) TO COL-DIST.
         MOVE FUNCTION NUMVAL(COL-TIME) TO COL-TIME.

         PERFORM UNTIL COL-DIST < WS-DIST
           COMPUTE WS-TIME = WS-TIME + 1
           COMPUTE WS-DIST = (COL-TIME - WS-TIME) * WS-TIME
         END-PERFORM.
         
         COMPUTE WS-BOAT-RES = COL-TIME - (2*WS-TIME) + 1.

