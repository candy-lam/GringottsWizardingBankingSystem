      ******************************************************************
      * CSCI3180 Principles of Programming Languages
      *
      * --- Declaration ---
      *
      * I declare that the assignment here submitted is original except for source
      * material explicitly acknowledged. I also acknowledge that I am aware of
      * University policy and regulations on honesty in academic work, and of the
      * disciplinary guidelines and procedures applicable to breaches of such policy
      * and regulations, as contained in the website
      * http://www.cuhk.edu.hk/policy/academichonesty/
      *
      * Assignment 1
      * Name : Lam Hiu Ching
      * Student ID : 1155129247
      * Email Addr : 1155129247@link.cuhk.edu.hk
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CENTRAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MASTER-FILE ASSIGN TO 'master.txt'
                ORGANISATION IS LINE SEQUENTIAL
                STATUS IS FS.

       SELECT TRANS711-FILE ASSIGN TO 'trans711.txt'
                ORGANISATION IS LINE SEQUENTIAL.

       SELECT TRANS713-FILE ASSIGN TO 'trans713.txt'
                ORGANISATION IS LINE SEQUENTIAL.

       SELECT UPDATEDM-FILE ASSIGN TO 'updatedMaster.txt'
                ORGANISATION IS LINE SEQUENTIAL.

       SELECT TRANSSORT711-FILE ASSIGN TO 'transSorted711.txt'
                ORGANISATION IS LINE SEQUENTIAL.

       SELECT TRANSSORT713-FILE ASSIGN TO 'transSorted713.txt'
                ORGANISATION IS LINE SEQUENTIAL.

       SELECT TRANSSORT-FILE ASSIGN TO 'transSorted.txt'
                ORGANISATION IS LINE SEQUENTIAL.

       SELECT NEGREPORT-FILE ASSIGN TO 'negReport.txt'
                ORGANISATION IS LINE SEQUENTIAL.

       SELECT TRANSACTION-TEMP-FILE ASSIGN TO 'temp.txt'
                ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MASTER-FILE.
       01 ACCOUNT.
           05 ACC-NAME PIC A(20).
           05 ACC-NUM PIC 9(16).
           05 ACC-PASSWORD PIC 9(6).
           05 BALANCE-SIGN PIC X.
           05 BALANCE PIC 9(13)V9(2).

       FD TRANS711-FILE.
       01 TRANSACTION711.
           05 ACC-NUM711 PIC 9(16).
           05 OPERA711 PIC X.
           05 AMOUNT711 PIC 9(5)V9(2).
           05 TS711 PIC 9(5).

       FD TRANS713-FILE.
       01 TRANSACTION713.
           05 ACC-NUM713 PIC 9(16).
           05 OPERA713 PIC X.
           05 AMOUNT713 PIC 9(5)V9(2).
           05 TS713 PIC 9(5).

       FD UPDATEDM-FILE.
       01 ACCOUNT-U.
           05 ACC-NAME-U PIC A(20).
           05 ACC-NUM-U PIC 9(16).
           05 ACC-PASSWORD-U PIC 9(6).
           05 BALANCE-SIGN-U PIC X.
           05 BALANCE-U PIC 9(13)V9(2).

       FD TRANSSORT711-FILE.
       01 TRANSACTION-S711.
           05 ACC-NUM-S711 PIC 9(16).
           05 OPERA-S711 PIC X.
           05 AMOUNT-S711 PIC 9(5)V9(2).
           05 TS-S711 PIC 9(5).

       FD TRANSSORT713-FILE.
       01 TRANSACTION-S713.
           05 ACC-NUM-S713 PIC 9(16).
           05 OPERA-S713 PIC X.
           05 AMOUNT-S713 PIC 9(5)V9(2).
           05 TS-S713 PIC 9(5).

       FD TRANSSORT-FILE.
       01 TRANSACTION-S.
           05 ACC-NUM-S PIC 9(16).
           05 OPERA-S PIC X.
           05 AMOUNT-S PIC 9(5)V9(2).
           05 TS-S PIC 9(5).

       FD NEGREPORT-FILE.
       01 NEGACC.
           05 STR1 PIC X(6).
           05 ACC-NAME-N PIC A(20).
           05 STR2 PIC X(16).
           05 ACC-NUM-N PIC 9(16).
           05 STR3 PIC X(11).
           05 BALANCE-N PIC 9(13)V9(2).

       SD TRANSACTION-TEMP-FILE.
       01 TRANSACTION-TEMP.
           05 ACC-TEMP PIC A(20).
           05 OPERA-TEMP PIC X.
           05 AMOUNT-TEMP PIC 9(5)V9(2).
           05 TS-TEMP PIC 9(5).

       WORKING-STORAGE SECTION.
      * variables for user account
       01 WS-ACCOUNT.
           05 WS-ACC-NAME PIC A(20).
           05 WS-ACC-NUM PIC 9(16).
           05 WS-ACC-PASSWORD PIC 9(6).
           05 WS-BALANCE-SIGN PIC X.
           05 WS-BALANCE PIC 9(13)V9(2).
       01 SORTING-TRANS.
           05 SO-ACC-NUM PIC 9(16).
           05 SO-OPERA PIC X.
           05 SO-AMOUNT PIC 9(5)V9(2).
           05 SO-TS PIC 9(5).
       01  TEMP-BALANCE PIC S9(13)V9(2).
       01  WS-EOF PIC 9 VALUE 0.
       *>bool variable for determining eof, 0 = false, 1 = true, initialize 0
       01  AN-EOF PIC 9 VALUE 0.
       *>bool variable for determining eof, 0 = false, 1 = true, initialize 0
       01  NULL-FILE-711 PIC 9 VALUE 1.    *> bool variable check whether file is null
       01  NULL-FILE-713 PIC 9 VALUE 1.    *> bool variable check whether file is null
       01  FS PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PARA.
      *    clean all output file/create empty output file
           OPEN OUTPUT TRANSSORT711-FILE.
           CLOSE TRANSSORT711-FILE.
           OPEN OUTPUT TRANSSORT713-FILE.
           CLOSE TRANSSORT713-FILE.
           OPEN OUTPUT TRANSSORT-FILE.
           CLOSE TRANSSORT-FILE.
           OPEN OUTPUT UPDATEDM-FILE.
           CLOSE UPDATEDM-FILE.
           OPEN OUTPUT NEGREPORT-FILE.
           CLOSE NEGREPORT-FILE.

       CENTRAL-MACHINE-PARA.
      * sorting
        *> create transSorted711.txt
           SORT TRANSACTION-TEMP-FILE ON ASCENDING KEY ACC-NUM-S711
                                      ON ASCENDING KEY TS-S711
           USING TRANS711-FILE GIVING TRANSSORT711-FILE.


       *> create transSorted713.txt
           SORT TRANSACTION-TEMP-FILE ON ASCENDING KEY ACC-NUM-S713
                                      ON ASCENDING KEY TS-S713
           USING TRANS713-FILE GIVING TRANSSORT713-FILE.


      * merging
           OPEN OUTPUT TRANSSORT-FILE.
           OPEN INPUT TRANSSORT711-FILE.
           MOVE 0 TO WS-EOF.

        TRANS-MERGE-711-PARA.
           READ TRANSSORT711-FILE NEXT RECORD INTO SORTING-TRANS
              AT END MOVE 1 TO WS-EOF
              NOT AT END  MOVE SO-ACC-NUM TO ACC-NUM-S
                          MOVE SO-OPERA TO OPERA-S
                          MOVE SO-AMOUNT TO AMOUNT-S
                          MOVE SO-TS TO TS-S
                          WRITE TRANSACTION-S
                          END-WRITE
           END-READ

           IF WS-EOF = 0 THEN      *> read next line if not eof
               GO TO TRANS-MERGE-711-PARA
           END-IF.

           CLOSE TRANSSORT711-FILE.

           *> check existence of file
           OPEN INPUT TRANSSORT713-FILE.
           MOVE 0 TO WS-EOF.
           GO TO TRANS-MERGE-713-PARA.

       TRANS-MERGE-713-PARA.
           READ TRANSSORT713-FILE NEXT RECORD INTO SORTING-TRANS
              AT END MOVE 1 TO WS-EOF
              NOT AT END  MOVE SO-ACC-NUM TO ACC-NUM-S
                          MOVE SO-OPERA TO OPERA-S
                          MOVE SO-AMOUNT TO AMOUNT-S
                          MOVE SO-TS TO TS-S
                          WRITE TRANSACTION-S
                          END-WRITE
           END-READ

           IF WS-EOF = 0 THEN      *> read next line if not eof
                GO TO TRANS-MERGE-713-PARA
           END-IF.

           CLOSE TRANSSORT713-FILE.
           CLOSE TRANSSORT-FILE.

       TRANSSORT-FILE-SORTING-PARA.                                *> sort file after merging
           SORT TRANSACTION-TEMP-FILE ON ASCENDING KEY ACC-NUM-S  *> primary key
                                      ON ASCENDING KEY TS-S       *> secondary key
           USING TRANSSORT-FILE GIVING TRANSSORT-FILE.

           IF FS = 41 THEN     *> if MASTER-FILE already open
               CLOSE MASTER-FILE
           END-IF.

           OPEN INPUT MASTER-FILE.

           IF FS = 35 THEN     *> if master.txt not exist
               DISPLAY "non-existing file!(master.txt)"
               STOP RUN
           END-IF.

           *> rest variable
           MOVE 1 TO NULL-FILE-711.
           MOVE 1 TO NULL-FILE-713.
           MOVE 0 TO WS-EOF.

       *> Read an acc info in master.txt
       *> for each acc, compare to all transaction record
       *> if found matched acc and transaction, update the balance
       UPDATE-PARA.
           *> if end of master.txt file then exit
           IF WS-EOF = 1 THEN
               GO TO EXIT-PARA
           END-IF.

           READ MASTER-FILE NEXT RECORD INTO WS-ACCOUNT
               AT END MOVE 1 TO WS-EOF
           END-READ.

           OPEN INPUT TRANSSORT-FILE.
           *> rest variable
           MOVE 0 TO AN-EOF.
           MOVE 0 TO TEMP-BALANCE.
           *> store the balance into temp variable with sign
           IF WS-BALANCE-SIGN = '-' THEN
               COMPUTE TEMP-BALANCE = 0 - WS-BALANCE
           END-IF.
           IF WS-BALANCE-SIGN = '+' THEN
               MOVE WS-BALANCE TO TEMP-BALANCE
           END-IF.

       COMPARE-RECORD-PARA.    *> compare each record in transaction
           READ TRANSSORT-FILE NEXT RECORD INTO SORTING-TRANS
               AT END MOVE 1 TO AN-EOF
           END-READ.

           *> if end of tranSorted.txt then write UPDATEDM-FILE and NEGREPORT-FILE
           IF AN-EOF = 1 THEN
               CLOSE TRANSSORT-FILE
               *> if end of master.txt (all accounts are updated, (avoid duplicate loop for last acc
               IF WS-EOF = 1 THEN
                   GO TO EXIT-PARA
               END-IF

               IF NULL-FILE-711 = 0 THEN
                   OPEN EXTEND UPDATEDM-FILE
               END-IF
               IF NULL-FILE-711 = 1 THEN
                   OPEN OUTPUT UPDATEDM-FILE
                   MOVE 0 TO NULL-FILE-711
               END-IF

               *> write the update acc info
               MOVE WS-ACCOUNT TO ACCOUNT-U
               WRITE ACCOUNT-U
               END-WRITE
               *> DISPLAY ACCOUNT-U
               CLOSE UPDATEDM-FILE

               *> find negative balance account and write it into negreport
               IF WS-BALANCE-SIGN = '-' THEN
                   IF NULL-FILE-713 = 0 THEN
                       OPEN EXTEND NEGREPORT-FILE
                   END-IF
                   IF NULL-FILE-713 = 1 THEN
                       OPEN OUTPUT NEGREPORT-FILE
                       MOVE 0 TO NULL-FILE-713
                   END-IF

                   MOVE "Name: " TO STR1
                   MOVE WS-ACC-NAME TO ACC-NAME-N
                   MOVE "Account Number: " TO STR2
                   MOVE WS-ACC-NUM TO ACC-NUM-N
                   MOVE " Balance: -" TO STR3
                   MOVE WS-BALANCE TO BALANCE-N
                   WRITE NEGACC
                   END-WRITE
                   CLOSE NEGREPORT-FILE
               END-IF

               GO TO UPDATE-PARA
           END-IF.

           *> update the balance according to transaction record
           IF WS-ACC-NUM = SO-ACC-NUM THEN
               *> add deposit amount to current balance
               IF SO-OPERA = 'D' THEN
                   COMPUTE TEMP-BALANCE = TEMP-BALANCE + SO-AMOUNT
               END-IF
               *> subtract withdrawal amount to current balance
               IF SO-OPERA = 'W' THEN
                   COMPUTE TEMP-BALANCE = TEMP-BALANCE - SO-AMOUNT
               END-IF

               *> assign corresponding sign according current balance
               IF TEMP-BALANCE < 0 THEN
                   MOVE '-' TO WS-BALANCE-SIGN
               END-IF
               IF TEMP-BALANCE >= 0 THEN
                   MOVE '+' TO WS-BALANCE-SIGN
               END-IF

               *> assign current balance without sign
               MOVE TEMP-BALANCE TO WS-BALANCE
           END-IF.

           *> read mext line if not end of transSort
           IF AN-EOF = 0 THEN
               GO TO COMPARE-RECORD-PARA
           END-IF.

        EXIT-PARA.     *> if MASTER-FILE already open
           CLOSE MASTER-FILE.

       END PROGRAM CENTRAL.
