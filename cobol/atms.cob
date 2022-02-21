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

       WORKING-STORAGE SECTION.
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

           IF FS = 41 THEN     *> if MASTER-FILE already open
               CLOSE MASTER-FILE
           END-IF.

           OPEN INPUT MASTER-FILE.

           IF FS = 35 THEN     *> if master.txt not exist
               DISPLAY "non-existing file!"
               STOP RUN
           END-IF.


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

           DISPLAY WS-ACCOUNT.

           IF MATCH = 1 THEN       *> if correct acc and INPUT-PASSWORD
               CLOSE MASTER-FILE
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
               CLOSE MASTER-FILE
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
               IF FS = 41 THEN     *> if MASTER-FILE already open
                   CLOSE MASTER-FILE
               END-IF
               STOP RUN
           END-IF.

      * input other than 'Y' and 'N' treat as invalid input, then loop
           DISPLAY "=> INVALID INPUT".
           GO TO END-PARA.

       END PROGRAM ATM.
