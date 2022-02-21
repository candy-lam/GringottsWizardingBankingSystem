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
       PROGRAM-ID. ATM.

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
      * variables for checking file existence
       01 FILE-NAME PIC X(30).
       01 FILE-INFO.
           05 FILE-SIZE PIC 9(18) COMP-X.
           05 MOD-DD PIC 9(2) COMP-X.      *> Modification Time
           05 MOD-MO PIC 9(2) COMP-X.
           05 MOD-YYYY PIC 9(4) COMP-X.    *> Modification Date
           05 MOD-HH PIC 9(2) COMP-X.
           05 MOD-MM PIC 9(2) COMP-X.
           05 MOD-SS PIC 9(2) COMP-X.
           05 FILLER PIC 9(2) COMP-X.      *> This will always be 00
      * variables for user account
       01 WS-ACCOUNT.
           05 WS-ACC-NAME PIC A(20).
           05 WS-ACC-NUM PIC 9(16).
           05 WS-ACC-PASSWORD PIC 9(6).
           05 WS-BALANCE-SIGN PIC X.
           05 WS-BALANCE PIC 9(13)V9(2).
      * variables for receiver account
       01 RE-ACCOUNT.
           05 RE-ACC-NAME PIC A(20).
           05 RE-ACC-NUM PIC 9(16).
           05 RE-ACC-PASSWORD PIC 9(6).
           05 RE-BALANCE-SIGN PIC X.
           05 RE-BALANCE PIC 9(13)V9(2).
      * variables for sorting usage
       01 SORTING-TRANS.
           05 SO-ACC-NUM PIC 9(16).
           05 SO-OPERA PIC X.
           05 SO-AMOUNT PIC 9(5)V9(2).
           05 SO-TS PIC 9(5).
       01  ATM-NUM PIC 99.                 *> stores chosen atm
       01  INPUT-ACC PIC 9(16).            *> stores input account number
       01  INPUT-PASSWORD PIC 9(6).        *> stores input account password
       01  INPUT-SERVICE PIC A.            *> stores service choice
       01  INPUT-AMOUNT PIC S9(5)V9(2).    *> stores input amount
       01  TIMESTAMP PIC 9(5) VALUE 00000. *> stores timestamp, initialize 00000
       01  TEMP-BALANCE PIC S9(13)V9(2).
       01  WS-EOF PIC 9 VALUE 0.
       *>bool variable for determining eof, 0 = false, 1 = true, initialize 0
       01  AN-EOF PIC 9 VALUE 0.
       *>bool variable for determining eof, 0 = false, 1 = true, initialize 0
       01  MATCH PIC 9 VALUE 0.
       *>bool variable for checking valid pair of acc and pw, 0 = false, 1 = true, initialize 0
       01  REGISTERED PIC 9 VALUE 0.
       *>bool variable for checking valid transfer, 0 = false, 1 = true, initialize 0
       01  NULL-FILE-711 PIC 9 VALUE 1.    *> bool variable check whether file is null
       01  NULL-FILE-713 PIC 9 VALUE 1.    *> bool variable check whether file is null
       01  FS PIC 9(2).                    *> variable storing file status of MASTER-FILE

       PROCEDURE DIVISION.
       MAIN-PARA.
      *    clean all output file/create empty output file
           OPEN OUTPUT TRANS711-FILE.
           CLOSE TRANS711-FILE.
           OPEN OUTPUT TRANS713-FILE.
           CLOSE TRANS713-FILE.
           OPEN OUTPUT TRANSSORT711-FILE.
           CLOSE TRANSSORT711-FILE.
           OPEN OUTPUT TRANSSORT713-FILE.
           CLOSE TRANSSORT713-FILE.
           OPEN OUTPUT TRANSSORT-FILE.
           CLOSE TRANSSORT-FILE.
           OPEN OUTPUT NEGREPORT-FILE.
           CLOSE NEGREPORT-FILE.

           DISPLAY "##############################################".
           DISPLAY "##         Gringotts Wizarding Bank         ##".
           DISPLAY "##                 Welcome                  ##".
           DISPLAY "##############################################".

       CHOOSING-PARA.          *> choosing atm
           DISPLAY "=> PLEASE CHOOSE THE ATM".
           DISPLAY "=> PRESS 1 FOR ATM 711".
           DISPLAY "=> PRESS 2 FOR ATM 713".
           ACCEPT ATM-NUM FROM SYSIN.
           IF NOT(ATM-NUM = 1 OR ATM-NUM = 2) THEN *> if invalid input
               DISPLAY "=> INVALID INPUT"
               GO TO CHOOSING-PARA
           END-IF.

           MOVE 0 TO MATCH.    *> reset variable before next step

      * check if master.txt exists
           MOVE 'master.txt' TO FILE-NAME.
           CALL "CBL_CHECK_FILE_EXIST" USING FILE-NAME FILE-INFO.
           IF RETURN-CODE NOT = 0 THEN
               DISPLAY "non-existing file! (master.txt)"
               STOP RUN
           END-IF.
           IF FS = 41 THEN     *> if MASTER-FILE already open
               CLOSE MASTER-FILE
           END-IF.
           OPEN INPUT MASTER-FILE.

       LOGIN-PARA.         *> input account and password
           IF MATCH = 1 THEN
               GO TO TRANSACTION-PARA
           END-IF.
           DISPLAY "=> ACCOUNT".
           ACCEPT INPUT-ACC FROM SYSIN.
           DISPLAY "=> PASSWORD".
           ACCEPT INPUT-PASSWORD FROM SYSIN.
           GO TO CHECKACCPW-PARA.

       CHECKACCPW-PARA.    *> check if input account and password are valid
           READ MASTER-FILE NEXT RECORD INTO WS-ACCOUNT
               AT END MOVE 1 TO WS-EOF
               NOT AT END IF WS-ACC-NUM = INPUT-ACC THEN   *> if input account number is registered
                               IF WS-ACC-PASSWORD = INPUT-PASSWORD THEN
                                   *> if correct password
                                   MOVE 1 TO MATCH
                               END-IF
                          END-IF
           END-READ.

           IF MATCH = 1 THEN       *> if correct acc and pw
               IF FS = 41 THEN     *> if MASTER-FILE already open
                   CLOSE MASTER-FILE
               END-IF
               OPEN INPUT MASTER-FILE
               MOVE 0 TO WS-EOF   *> reset bool variable for eof
               GO TO TRANSACTION-PARA
           END-IF.

           IF WS-EOF = 0 THEN      *> if not found in current line, read next line
               GO TO CHECKACCPW-PARA
           END-IF.

           IF MATCH = 0 THEN       *> if not found in file, loop
               DISPLAY "=> INCORRECT ACCOUNT/PASSWORD"
               *> reset file pointer to the begining by reopen the file
               IF FS = 41 THEN     *> if MASTER-FILE already open
                   CLOSE MASTER-FILE
               END-IF
               OPEN INPUT MASTER-FILE
               MOVE 0 TO WS-EOF    *> reset bool variable for eof
               GO TO LOGIN-PARA
           END-IF.

       TRANSACTION-PARA.           *> choosing which service
           *> check if negative balance. if yes, end transaction
           IF WS-BALANCE-SIGN = '-' THEN
               IF FS = 41 THEN     *> if MASTER-FILE already open
                   CLOSE MASTER-FILE
               END-IF
               OPEN INPUT MASTER-FILE
               DISPLAY "=> NEGATIVE REMAINS TRANSACTION ABORT"
               MOVE 0 TO MATCH
               GO TO LOGIN-PARA
           END-IF.

           DISPLAY "=> PLEASE CHOOSE YOUR SERVICE".
           DISPLAY "=> PRESS D FOR DEPOSIT".
           DISPLAY "=> PRESS W FOR WITHDRAWAL".
           DISPLAY "=> PRESS T FOR TRANSFER".
           ACCEPT INPUT-SERVICE FROM SYSIN.

           IF INPUT-SERVICE = 'D' THEN
               GO TO DEPOSIT-PARA
           END-IF.

           IF INPUT-SERVICE = 'W' THEN
               MOVE 0 TO REGISTERED        *> reset variable
               GO TO WITHDRAWAL-PARA
           END-IF.

           IF INPUT-SERVICE = 'T' THEN
               GO TO PRETRANSFER-PARA
           END-IF.

      * input other than 'D', 'W' and 'T' treat as invalid input, then loop
           DISPLAY "=> INVALID INPUT".
           GO TO TRANSACTION-PARA.

       DEPOSIT-PARA.  *> deposit service
           DISPLAY "=> AMOUNT".
           ACCEPT INPUT-AMOUNT FROM SYSIN.

           IF INPUT-AMOUNT < 0 THEN    *> negative input amount, then loop
               DISPLAY "=> INVALID INPUT"
               GO TO DEPOSIT-PARA
           END-IF.

           *> generate transaction record to according atm file.
           IF ATM-NUM = 1 THEN
               IF NULL-FILE-711 = 0 THEN
                   OPEN EXTEND TRANS711-FILE
               END-IF
               IF NULL-FILE-711 = 1 THEN
                   OPEN OUTPUT TRANS711-FILE
                   MOVE 0 TO NULL-FILE-711
               END-IF

               MOVE WS-ACC-NUM TO ACC-NUM711
               MOVE 'D' TO OPERA711
               MOVE INPUT-AMOUNT TO AMOUNT711
               MOVE TIMESTAMP TO TS711
               COMPUTE TIMESTAMP = TIMESTAMP + 1
               WRITE TRANSACTION711
               END-WRITE
               CLOSE TRANS711-FILE
           END-IF.

           IF ATM-NUM = 2 THEN
               IF NULL-FILE-713 = 0 THEN
                   OPEN EXTEND TRANS713-FILE
               END-IF
               IF NULL-FILE-713 = 1 THEN
                   OPEN OUTPUT TRANS713-FILE
                   MOVE 0 TO NULL-FILE-713
               END-IF

               MOVE WS-ACC-NUM TO ACC-NUM713
               MOVE 'D' TO OPERA713
               MOVE INPUT-AMOUNT TO AMOUNT713
               MOVE TIMESTAMP TO TS713
               COMPUTE TIMESTAMP = TIMESTAMP + 1
               WRITE TRANSACTION713
               END-WRITE
               CLOSE TRANS713-FILE
           END-IF.


           GO TO END-PARA.

       PRETRANSFER-PARA.   *> input receiver acc num for transfer service
           IF REGISTERED = 0 THEN
               IF FS = 41 THEN     *> if MASTER-FILE already open
                   CLOSE MASTER-FILE
               END-IF
               OPEN INPUT MASTER-FILE
               DISPLAY "=> TARGET ACCOUNT"
               ACCEPT INPUT-ACC FROM SYSIN

               IF INPUT-ACC = WS-ACC-NUM THEN      *> receiver acc == user acc
                   DISPLAY "=> YOU CANNOT TRANSFER TO YOURSELF"
                   CLOSE MASTER-FILE
                   GO TO PRETRANSFER-PARA
               END-IF
               GO TO CHECKREGISTERED-PARA
           END-IF.

       TRANSFER-PARA.  *> transfer service
           DISPLAY "=> AMOUNT".
           ACCEPT INPUT-AMOUNT FROM SYSIN.

           IF INPUT-AMOUNT < 0 THEN    *> negative input amount, then loop
               DISPLAY "=> INVALID INPUT"
               GO TO TRANSFER-PARA
           END-IF.

           IF INPUT-AMOUNT > WS-BALANCE THEN   *> insufficient balance, then loop
               DISPLAY "=> INSUFFICIENT BALANCE"
               GO TO TRANSFER-PARA
           END-IF.

           *> generate transaction record to according atm file.
           IF ATM-NUM = 1 THEN
               IF NULL-FILE-711 = 0 THEN
                   OPEN EXTEND TRANS711-FILE
               END-IF
               IF NULL-FILE-711 = 1 THEN
                   OPEN OUTPUT TRANS711-FILE
                   MOVE 0 TO NULL-FILE-711
               END-IF

               MOVE WS-ACC-NUM TO ACC-NUM711
               MOVE 'W' TO OPERA711
               MOVE INPUT-AMOUNT TO AMOUNT711
               MOVE TIMESTAMP TO TS711
               COMPUTE TIMESTAMP = TIMESTAMP + 1
               WRITE TRANSACTION711
               END-WRITE

               MOVE RE-ACC-NUM TO ACC-NUM711
               MOVE 'D' TO OPERA711
               MOVE INPUT-AMOUNT TO AMOUNT711
               MOVE TIMESTAMP TO TS711
               COMPUTE TIMESTAMP = TIMESTAMP + 1
               WRITE TRANSACTION711
               END-WRITE

               CLOSE TRANS711-FILE
           END-IF.

           IF ATM-NUM = 2 THEN
               IF NULL-FILE-713 = 0 THEN
                   OPEN EXTEND TRANS713-FILE
               END-IF
               IF NULL-FILE-713 = 1 THEN
                   OPEN OUTPUT TRANS713-FILE
                   MOVE 0 TO NULL-FILE-713
               END-IF

               MOVE WS-ACC-NUM TO ACC-NUM713
               MOVE 'W' TO OPERA713
               MOVE INPUT-AMOUNT TO AMOUNT713
               MOVE TIMESTAMP TO TS713
               COMPUTE TIMESTAMP = TIMESTAMP + 1
               WRITE TRANSACTION713
               END-WRITE

               MOVE RE-ACC-NUM TO ACC-NUM713
               MOVE 'D' TO OPERA713
               MOVE INPUT-AMOUNT TO AMOUNT713
               MOVE TIMESTAMP TO TS713
               COMPUTE TIMESTAMP = TIMESTAMP + 1
               WRITE TRANSACTION713
               END-WRITE

               CLOSE TRANS713-FILE
           END-IF.

           GO TO END-PARA.

       CHECKREGISTERED-PARA.   *> check if receiver acc resgistered
           READ MASTER-FILE NEXT RECORD INTO RE-ACCOUNT
               AT END MOVE 1 TO WS-EOF
               NOT AT END IF RE-ACC-NUM = INPUT-ACC THEN
                               MOVE 1 TO REGISTERED
                          END-IF
           END-READ.

           IF REGISTERED = 1 THEN       *> if registered acc found
               *> DISPLAY "=> TARGET ACCOUNT FOUND"
               CLOSE MASTER-FILE
               MOVE 0 TO WS-EOF         *> reset bool variable for eof
               GO TO TRANSFER-PARA
           END-IF.

           IF WS-EOF = 0 THEN           *> if not found in current line, read next line
               GO TO CHECKREGISTERED-PARA
           END-IF.

           IF REGISTERED = 0 THEN       *> if not registered
               DISPLAY "=> TARGET ACCOUNT DOES NOT EXIST"
               CLOSE MASTER-FILE
               MOVE 0 TO WS-EOF    *> reset bool variable for eof
               GO TO PRETRANSFER-PARA
           END-IF.


       WITHDRAWAL-PARA.    *> withdrawal service
           DISPLAY "=> AMOUNT".
           ACCEPT INPUT-AMOUNT FROM SYSIN.

           IF INPUT-AMOUNT < 0 THEN    *> negative input amount, then loop
               DISPLAY "=> INVALID INPUT"
               GO TO WITHDRAWAL-PARA
           END-IF.

           IF INPUT-AMOUNT > WS-BALANCE THEN   *> insufficient balance, then loop
               DISPLAY "=> INSUFFICIENT BALANCE"
               GO TO WITHDRAWAL-PARA
           END-IF.

           *> generate transaction record to according atm file.
           IF ATM-NUM = 1 THEN
               IF NULL-FILE-711 = 0 THEN
                   OPEN EXTEND TRANS711-FILE
               END-IF
               IF NULL-FILE-711 = 1 THEN
                   OPEN OUTPUT TRANS711-FILE
                   MOVE 0 TO NULL-FILE-711
               END-IF

               MOVE WS-ACC-NUM TO ACC-NUM711
               MOVE 'W' TO OPERA711
               MOVE INPUT-AMOUNT TO AMOUNT711
               MOVE TIMESTAMP TO TS711
               COMPUTE TIMESTAMP = TIMESTAMP + 1
               WRITE TRANSACTION711
               END-WRITE
               CLOSE TRANS711-FILE
           END-IF.

           IF ATM-NUM = 2 THEN
               IF NULL-FILE-713 = 0 THEN
                   OPEN EXTEND TRANS713-FILE
               END-IF
               IF NULL-FILE-713 = 1 THEN
                   OPEN OUTPUT TRANS713-FILE
                   MOVE 0 TO NULL-FILE-713
               END-IF

               MOVE WS-ACC-NUM TO ACC-NUM713
               MOVE 'W' TO OPERA713
               MOVE INPUT-AMOUNT TO AMOUNT713
               MOVE TIMESTAMP TO TS713
               COMPUTE TIMESTAMP = TIMESTAMP + 1
               WRITE TRANSACTION713
               END-WRITE
               CLOSE TRANS713-FILE
           END-IF.

           GO TO END-PARA.

       END-PARA.       *> end of transaction
           DISPLAY "=> CONTINUE?"
           DISPLAY "=>  N FOR NO"
           DISPLAY "=>  Y FOR YES"
           ACCEPT INPUT-SERVICE FROM SYSIN.

           IF INPUT-SERVICE = 'Y' THEN
               GO TO CHOOSING-PARA     *> go back to step 1
           END-IF.

           IF INPUT-SERVICE = 'N' THEN
               GO TO CENTRAL-MACHINE-PARA
           END-IF.

      * input other than 'Y' and 'N' treat as invalid input, then loop
           DISPLAY "=> INVALID INPUT".
           GO TO END-PARA.

       CENTRAL-MACHINE-PARA.
      * sorting
      * check if trans711.txt exists
           MOVE 'trans711.txt' TO FILE-NAME.
           CALL "CBL_CHECK_FILE_EXIST" USING FILE-NAME FILE-INFO.
           *> create transSorted711.txt
           IF RETURN-CODE = 0 THEN
               SORT TRANSACTION-TEMP-FILE ON ASCENDING KEY ACC-NUM-S711
                                          ON ASCENDING KEY TS-S711
               USING TRANS711-FILE GIVING TRANSSORT711-FILE
           END-IF.

      * check if trans713.txt exists
           MOVE 'trans713.txt' TO FILE-NAME.
           CALL "CBL_CHECK_FILE_EXIST" USING FILE-NAME FILE-INFO.
           *> create transSorted713.txt
           IF RETURN-CODE = 0 THEN
               SORT TRANSACTION-TEMP-FILE ON ASCENDING KEY ACC-NUM-S713
                                          ON ASCENDING KEY TS-S713
               USING TRANS713-FILE GIVING TRANSSORT713-FILE
           END-IF.

      * merging
           OPEN OUTPUT TRANSSORT-FILE
           *> check existence of file
           MOVE 'trans711.txt' TO FILE-NAME.
           CALL "CBL_CHECK_FILE_EXIST" USING FILE-NAME FILE-INFO.
           IF RETURN-CODE = 0 THEN
               OPEN INPUT TRANSSORT711-FILE
               MOVE 0 TO WS-EOF
               GO TO TRANS-MERGE-711-PARA
           END-IF.

           *> check existence of file
           MOVE 'trans713.txt' TO FILE-NAME.
           CALL "CBL_CHECK_FILE_EXIST" USING FILE-NAME FILE-INFO.
           IF RETURN-CODE = 0 THEN
               OPEN INPUT TRANSSORT713-FILE
               MOVE 0 TO WS-EOF
               GO TO TRANS-MERGE-713-PARA
           END-IF.

           STOP RUN.

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
           MOVE 'trans713.txt' TO FILE-NAME.
           CALL "CBL_CHECK_FILE_EXIST" USING FILE-NAME FILE-INFO.
           IF RETURN-CODE = 0 THEN
              OPEN INPUT TRANSSORT713-FILE
              MOVE 0 TO WS-EOF
              GO TO TRANS-MERGE-713-PARA
           END-IF.

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

       TRANSSORT-FILE-SORTING-PARA.
           *> check existence of file
           MOVE 'transSorted.txt' TO FILE-NAME.
           CALL "CBL_CHECK_FILE_EXIST" USING FILE-NAME FILE-INFO.
           IF RETURN-CODE = 0 THEN                                    *> sort file after merging
               SORT TRANSACTION-TEMP-FILE ON ASCENDING KEY ACC-NUM-S  *> primary key
                                          ON ASCENDING KEY TS-S       *> secondary key
               USING TRANSSORT-FILE GIVING TRANSSORT-FILE
           END-IF.
           IF FS = 41 THEN     *> if MASTER-FILE already open
                   CLOSE MASTER-FILE
           END-IF.
           OPEN INPUT MASTER-FILE.
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

           *> update the current for next loop
           MOVE WS-BALANCE TO TEMP-BALANCE.

           *> read mext line if not end of transSort
           IF AN-EOF = 0 THEN
               GO TO COMPARE-RECORD-PARA
           END-IF.

        EXIT-PARA.    *> if MASTER-FILE already open
            CLOSE MASTER-FILE.

       END PROGRAM ATM.
