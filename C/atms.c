#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

int timestamp = 0;  //global variable of time stamp for transactions, start from 0

/*
    function for checking if the account is registered and correct password input

    accNum    : account number need to be checked
    pw        : password need to be check, -1 if no password checking required
    acc       : char pointer storing account information if the account is registered (and correct password input if required)
    masterfile: file pointer of master.txt (all registered account information)

    return 0 if wrong account or password, 1 if correct account (or password if required), -1 if account balance is negative
*/
int login(char *accInfo, FILE *masterfile){
    int machineNum, correctAcc = 0;            // correct
    do{
        printf("=> PLEASE CHOOSE THE ATM\n");
        printf("=> PRESS 1 FOR ATM 711\n");
        printf("=> PRESS 2 FOR ATM 713\n");
        scanf("%d", &machineNum);

        if (machineNum != 1 && machineNum != 2)     // check if valid input (either 1 or 2)
            printf("=> INVALID INPUT\n");

    }while (machineNum != 1 && machineNum != 2);    // loop if invalid input

    char *accNum, *pw;
    accNum = malloc(16*sizeof(char));
    pw = malloc(8*sizeof(char));

    do{
        printf("=> ACCOUNT\n");
        scanf("%s", accNum);
        printf("=> PASSWORD\n");
        scanf("%s", pw);
        if (strlen(pw) != 6){   // wrong password format (length of 6)
            printf("=> INCORRECT ACCOUNT/PASSWORD\n");
            continue;
        }
        correctAcc = checkAcc(accNum, pw, accInfo, masterfile);     // check if correct account and password, 0 for false, 1 for true, -1 for negative balance
        if (correctAcc == 0)
            printf("=> INCORRECT ACCOUNT/PASSWORD\n");
        else if (correctAcc == -1){
            printf("=> NEGATIVE REMAINS TRANSACTION ABORT\n");
            correctAcc = 0;                                         // negative balance account, ask user to input another account
        }
    }while (correctAcc == 0);

    return machineNum;
}

int checkAcc(char *accNum, char *pw, char *acc, FILE *masterfile){
    rewind(masterfile);
    if (strlen(accNum) != 16) // wrong account number format (length of 16)
        return 0;

    char line[59], fileAccNum[17], filePw[7], neg;
    while(fgets(line, 59, masterfile) != NULL){ // read file until end of file
        //printf("line: %s\n", line);
        strncpy(fileAccNum, line+20, 16);
        fileAccNum[16] = '\0';
        //printf("fileAccNum: %s accNum: %s\n", fileAccNum, accNum);
        if (strcmp(fileAccNum, accNum) == 0){   // check if registered account
            if (strcmp(pw, "-1") == 0){         // check if password check required
                strncpy(acc, line, 59);         // pass the account information to pointer
                return 1;
            }
            strncpy(filePw, line+36, 6);
            filePw[6] = '\0';
            if (strcmp(filePw, pw) == 0){       // check if correct password
                neg = line[42];
                if (neg == '-')                 // check if negative balance
                    return -1;
                strncpy(acc, line, 59);         // pass the account information to pointer
                return 1;
            }
        }
        fgets(line, sizeof(line), masterfile);
    }

    return 0;
}

/*
    login page for choosing machine and inputing account and corresponding password

    accInfo   : char pointer storing account information
    masterfile: file pointer of master.txt (all registered account information)

    return chosen machine's number, 1 for 711, 2 for 713
*/


/*
    page for selecting service, return chosen service
*/
char serviceSelect(){
    char *input;
    char service;
    int valid = 0;    // 0 for invalid, 1 for valid
    input = malloc(20*sizeof(char));
    do{
        printf("=> PLEASE CHOOSE YOUR SERVICE\n");
        printf("=> PRESS D FOR DEPOSIT\n");
        printf("=> PRESS W FOR WITHDRAWAL\n");
        printf("=> PRESS T FOR TRANSFER\n");
        scanf("%s", input);
        if (strlen(input) > 1){     // check if length of input > 1
            printf("INVALID INPUT\n");
            continue;
        }
        service = input[0];
        if (service != 'D' && service != 'W' && service != 'T') // check if valid input (D/W/T)
            printf("INVALID INPUT\n");
        else valid =  1;
    }while(valid == 0);   // loop if invalid input

    return service;
}

/*
    function for deposit action

    accInfo     : char pointer storing account information
    machineNum  : int variable for determine which machine in use, 1 for 711, 2 for 713
    transAmount : float pointer for transfer amount, contain value if call by transfer function, -1 if only deposit action (call by main function)
*/
void deposit(char *accInfo, int machineNum, float *transAmount){
    float value;
    int exit = 0;
    char *filename, *accNum, *output;
    filename = malloc(30*sizeof(char));
    accNum = malloc(16*sizeof(char));
    output = malloc(30*sizeof(char));

    // check which file should be output according to machine in use, 1 for 711, 2 for 713
    if (machineNum == 1)
        filename = "trans711.txt";
    else if (machineNum == 2)
        filename = "trans713.txt";


    while(exit == 0 && *transAmount == -1){     // if call by main function, loop if invalid input
        printf("=> AMOUNT\n");
        scanf("%f", &value);
        printf("%.2f\n", value);
        if (value < 0 || value >= 100000)       // if input amount out of bound [0 - 100000)
            printf("=> INVALID INPUT\n");
        else
            exit = 1;
    }

    if (*transAmount != -1)                     // if call by transfer function, assign the transfer amount for output
        value = *transAmount;

    int amount = value*100;     // formating the transaction amount for decimal number to 7 digits

    strncpy(accNum, accInfo+20, 16);
    accNum[16] = '\0';

    /*  formating the output for file
        16 digits for account | 1 character indicate action | 7 digits for transaction amount | 5 digits for time stamp*/
    sprintf(output, "%s%c%07d%05d\n", accNum, 'D', amount, timestamp);
    timestamp += 1;     // +1 time stamp for next action

    // write corresponding transaction file
    FILE *record = fopen(filename, "a+");
    fprintf(record, output);
    fclose(record);
    printf("%s => %s\n", output, filename);

}


/*
    function for withdrawal action

    accInfo     : char pointer storing account information
    machineNum  : int variable for determine which machine in use, 1 for 711, 2 for 713
    transAmount : float pointer for transfer amount, contain value if call by transfer function, -1 if only withdrawal action (call by main function)
*/
void withdrawal(char *accInfo, int machineNum, float *transAmount){
    int exit = 0;
    float value, accBalance;
    char *filename, *accNum, *accBalanceInS, *output;

    filename = malloc(30*sizeof(char));
    output = malloc(30*sizeof(char));
    accNum = malloc(15*sizeof(char));
    accBalanceInS = malloc(16*sizeof(char));

    strncpy(accBalanceInS, accInfo+43, 15);     // read account balance
    accBalanceInS[15] = '\0';
    accBalance = atof(accBalanceInS) /100;      // turn it into decimal number
    printf("Current Balance: %.2f\n", accBalance);

    // check which file should be output according to machine in use, 1 for 711, 2 for 713
    if (machineNum == 1)
        filename = "trans711.txt";
    else if (machineNum == 2)
        filename = "trans713.txt";

    while(exit == 0){           // loop if invalid input
        printf("=> AMOUNT\n");
        scanf("%f", &value);
        //printf("%.2f\n", value);
        if (value < 0 || value >= 100000){      // check if input amount out of bound [0 - 100000)
            printf("=> INVALID INPUT\n");
        }else if (value > accBalance){          // check if sufficient balance
            printf("=> INSUFFICIENT BALANCE\n");
        }else
            exit = 1;
    }

    *transAmount = value;

    int amount = value*100; // formating the transaction amount for decimal number to 7 digits

    strncpy(accNum, accInfo+20, 16);
    accNum[16] = '\0';

    /*  formating the output for file
        16 digits for account | 1 character indicate action | 7 digits for transaction amount | 5 digits for time stamp */
    sprintf(output, "%s%c%07d%05d\n", accNum, 'W', amount, timestamp);
    timestamp += 1;     // +1 time stamp for next action

    // write corresponding transaction file
    FILE *record = fopen(filename, "a+");
    fprintf(record, output);
    fclose(record);
    printf("%s => %s\n", output, filename);
}


/*
    function for withdrawal action

    accInfo     : char pointer storing account information
    machineNum  : int variable for determine which machine in use, 1 for 711, 2 for 713
    masterfile: file pointer of master.txt (all registered account information)
*/
void transfer(char *accInfo, int machineNum, FILE *masterfile){
    char *target, *accNum, *targetInfo;
    float value;
    int exit = 0;
    target = malloc(16*sizeof(char));
    accNum = malloc(16*sizeof(char));
    targetInfo = malloc(58*sizeof(char));       // char pointer storing target account information

    strncpy(accNum, accInfo+20, 16);
    accNum[16] = '\0';
    printf("accNum: %s\n", accNum);

    int exist = 0;
    target = malloc(16*sizeof(char));
    do{
        printf("=> TARGET ACCOUNT\n");
        scanf("%s", target);

        if (strcmp(target, accNum) == 0){       // check if target account and user account are the same
            printf("=> YOU CANNOT TRANSFER TO YOURSELF\n");
        }
        exist = checkAcc(target, "-1", targetInfo, masterfile);  // check if target account registered, 0 for false, 1 for true
        if (exist == 0){
            printf("=> TARGET ACCOUNT DOSE NOT EXIST\n");
        }
    }while (exist == 0);    // loop until valid target account input

    printf("accInfo: %s\n", accInfo);
    printf("targetInfo: %s\n", targetInfo);

    float transAmount;     // float pointer for transfer amount
    withdrawal(accInfo, machineNum, &transAmount);   // withdrawal action for user account
    deposit(targetInfo, machineNum, &transAmount);   // deposit action for target account
    printf("transAmount: %.2f\n", transAmount);
}

/*
    page for user to choose whether continue the program
    return user choice (either N(No) or Y(Yes))
*/
char contin(){
    char *input;
    int validQuit = 0;
    input = malloc(10*sizeof(char));

    while(validQuit == 0){      // loop if invalid input
        printf("=> CONTINUE?\n");
        printf("=> N FOR NO\n");
        printf("=> Y FOR YES\n");
        scanf("%s", input);
        input[strlen(input)] = '\0';
        if (strcmp(input, "N") == 0 || strcmp(input, "Y") == 0)     // check if valid input (either N or Y)
            validQuit = 1;
        else
            printf("=> INVALID INPUT\n");
    }
    return input[0];
}


int main(){
    printf("##############################################\n");
    printf("##         Gringotts Wizarding Bank         ##\n");
    printf("##                 Welcome                  ##\n");
    printf("##############################################\n");
    FILE *masterfile, *trans711, *trans713;

    masterfile = fopen("master.txt", "r");
    if (masterfile == NULL){                //if master.txt not exist, end the program
        printf("non-existing file!\n");
        return 0;
    }

    char *accInfo;
    accInfo = malloc(58*sizeof(char));
    int machineNum;
    float fromMain;         // variable for withdrawal() and deposit() to recognize the request from main func
    do{
        // reset variables
        fromMain = -1;

        machineNum = login(accInfo, masterfile);    //determine which machine in use
        //printf("accInfo: %s\n", accInfo);

        char s = serviceSelect();                   //select service
        if (s == 'D')
            deposit(accInfo, machineNum, &fromMain);
        else if (s == 'W')
            withdrawal(accInfo, machineNum, &fromMain);
        else if (s == 'T')
            transfer(accInfo, machineNum, masterfile);

    }while(contin() == 'Y');    // loop if chosen continue

    fclose(masterfile);
    return 0;
}
