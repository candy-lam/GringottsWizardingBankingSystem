#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sort.h"

/*
    CSCI3180 Principles of Programming Languages

    --- Declaration ---

    I declare that the assignment here submitted is original except for source
    material explicitly acknowledged. I also acknowledge that I am aware of
    University policy and regulations on honesty in academic work, and of the
    disciplinary guidelines and procedures applicable to breaches of such policy
    and regulations, as contained in the website
    http://www.cuhk.edu.hk/policy/academichonesty/

    Assignment 1
    Name : Lam Hiu Ching
    Student ID : 1155129247
    Email Addr : 1155129247@link.cuhk.edu.hk
*/

/*
    function for merging files
    source     : file pointer of file being merged
    destination: file pointer of merged file
*/
void merge(FILE *source, FILE *destination){
    char *line;
    line = malloc(60*sizeof(char));
    while (fgets(line, 60, source) != NULL){    // loop if not the end of source file
        line[strlen(line)] = '\0';
        printf("%s", line);
        fprintf(destination, line);             // write line in merged file
    }
}

/*
    function for generating updatedMaster.txt
    performing balance update account by account according to the order of master.txt

    original     : file pointer of original master.txt
    modification : file pointer of modification going to make (aka. transaction file (transSorted.txt))
    destination  : file pointer of updated master.txt
*/
void update(FILE *original, FILE *modification, FILE *destination){
    char *originalLine = malloc(60*sizeof(char));       // storing a line of master.txt
    char *output = malloc(60*sizeof(char));             // storing a line of updatedMaster.txt
    char *accNum = malloc(17*sizeof(char));             // storing the account number currently updating
    char *balanceInS = malloc(16*sizeof(char));         // temporary string variable for storing balance from master.txt
    int balance;                                        // storing balance of current account

    while (fgets(originalLine, 60, original) != NULL){  // read a line from master.txt until end of file
        //printf("fgets Line: %s", originalLine);
        strncpy(balanceInS, originalLine+42, 16);
        balanceInS[strlen(balanceInS)] = '\0';
        balance = atoi(balanceInS);
        //printf("Current balance: %+016d /100 \n", balance);

        strncpy(accNum, originalLine+20, 16);
        accNum[16] = '\0';
        //printf("accNum: %s\n", accNum);

        searchAcc(modification, accNum, &balance);      // update balance according to the transaction file (transSorted.txt)

        strncpy(output, originalLine, 42);
        output[42] = '\0';
        /* write updatedMaster.txt in format of
           20 characters for account holder name | 16 digits for account number | 6 digits for password | 16 digits for balance*/
        sprintf(output, "%s%+016d\n", output, balance);
        fprintf(destination, output);
        printf("%s", output);
    }
}

/*
    function for updating account balance
    check whether the account have transaction and then update the balance

    source       : file pointer of transaction file (transSorted.txt)
    target       : account number of account currently updating
    balance      : account balance, pass by reference
*/
int searchAcc(FILE *source, char *target, int *balance){
    rewind(source);                                  // reset the buffer of transaction file
    char *line = malloc(30*sizeof(char));           // storing a line of transSorted.txt
    char *accNum = malloc(17*sizeof(char));         // storing the account number of the transaction
    char *transAmountInS = malloc(7*sizeof(char));  // temporary string variable for storing transaction amount from transSorted.txt
    int transAmount;                                 // storing transaction amount
    char action;                                     // storing action (either D(Deposit)/W(Withrawal)

    while (fgets(line, 30, source) != NULL){        // read a line from transSorted.txt until end of file
        //printf("%s\n", line);
        strncpy(accNum, line, 16);
        //printf("%s\n", accNum);

        strncpy(transAmountInS, line+18, 6);
        transAmountInS[6] = '\0';
        transAmount = atoi(transAmountInS);
        //printf("transAmount: %d\n", transAmount);

        if (strcmp(accNum, target) == 0){           // if find transaction record of current account
            action = line[16];
            //printf("%c\n", action);
            if (action == 'D')                      // if deposit is performed, add the deposit amount to current balance
                *balance += transAmount;
            else if (action == 'W')
                *balance -= transAmount;            // if withdrawal is performed, subtract the withdrawal amount to current balance
        }
        fgets(line, 30, source);
    }

    //printf("Current balance: %d\n", *balance);
    return 0;
}

/*
    function for generating negative balance account balance
    check whether the account have negative balance line by line

    source       : file pointer of master.txt
    report       : file pointer of updateMaster.txt
*/
void findNeg(FILE *source, FILE *report){
    rewind(source);                             // reset the buffer
    char *line = malloc(60*sizeof(char));      // storing a line from master.txt
    char *name = malloc(20*sizeof(char));      // storing account holder name
    char *accNum = malloc(16*sizeof(char));    // storing account number
    char *balance = malloc(16*sizeof(char));   // storing account balance
    char *output = malloc(85*sizeof(char));    // storing the whole line with format for output

    while (fgets(line, 60, source) != NULL){
        //printf("%s\n", line);
        //printf("%c\n", line[42]);
        if (line[42] == '-'){                   // check the sign of balance whether negative
            strncpy(name, line, 20);
            strncpy(accNum, line+20, 16);
            strncpy(balance, line+42, 16);
            /* write updatedMaster.txt in format of
               Name: [20 characters for account holder name] Account Number: [16 digits for account number] Balance: [16 digits for balance]*/
            sprintf(output, "Name: %sAccount Number: %s Balance: %s\n", name, accNum, balance);
            fprintf(report, output);
            printf("%s", output);
        }
    }
}

int main(){
    printf("=================Central=================\n\n");
    FILE *masterfile, *trans711, *trans713, *transSort711, *transSort713, *transSort, *updatedFile, *negRep;

    masterfile = fopen("master.txt", "r");
    if (masterfile == NULL){                //if master.txt not exist, end the program
        printf("non-existing file!\n");
        return 0;
    }

    // create empty file if file not exist.
    trans711 = fopen("trans711.txt", "a+");
    trans713 = fopen("trans713.txt", "a+");
    fclose(trans711);
    fclose(trans713);

    // sort each transaction record and store into corresponding file
    sort_transaction("trans711.txt", "transSorted711.txt");
    sort_transaction("trans713.txt", "transSorted713.txt");

    transSort = fopen("transSorted.txt", "w+");
    transSort711 = fopen("transSorted711.txt", "r");
    transSort713 = fopen("transSorted713.txt", "r");

    // merge two transaction file into one file
    merge(transSort711, transSort);
    merge(transSort713, transSort);
    printf("Transaction File Merged => transSorted.txt\n");

    // sort the merged transaction file
    fclose(transSort);
    sort_transaction("transSorted.txt", "transSorted.txt");
    printf("Transaction File Sorted => transSorted.txt\n\n");

    // generating updated master file
    transSort = fopen("transSorted.txt", "r");
    updatedFile = fopen("updatedMaster.txt", "w+");
    update(masterfile, transSort, updatedFile);
    printf("Information Updated => updatedMaster.txt\n\n");

    // generating negative balance account report
    negRep = fopen("negReport.txt", "w+");
    findNeg(updatedFile, negRep);
    printf("Negative Report Generated => negReport.txt\n\n");

    fclose(transSort);
    fclose(transSort711);
    fclose(transSort713);
    fclose(updatedFile);
    fclose(negRep);
    fclose(masterfile);

    return 0;
}
