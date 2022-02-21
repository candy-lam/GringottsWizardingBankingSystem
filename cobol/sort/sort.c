#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include"sort.h"

////////////////////////////////////////////////
// SORT FUNCTION
// global variable
int MAX_TRAN = 10;

void read_str(char input_line[], char output_line[], int start_index, int length) {
    strncpy(output_line, input_line + start_index, length);
    output_line[length] = '\0';
}

struct transaction {
    char transac_account[20];
    char op_transac[5];
    double amounts;
    int timestamp;
};

struct transaction * process_one_transaction(char line[]) {
    struct transaction * result_transaction = (struct transaction *) malloc(sizeof(struct transaction));

    char temp_str[30];

    // transaction account
    read_str(line, temp_str, 0, 16);
    strcpy(result_transaction->transac_account , temp_str);

    // operation
    read_str(line, temp_str, 16, 1);
    strcpy(result_transaction->op_transac , temp_str);

    // integer
    read_str(line, temp_str, 17, 5);
    int integer = atoi(temp_str);

    //float
    read_str(line, temp_str, 22, 2);
    int floating = atoi(temp_str);
    result_transaction->amounts = (double) integer + (double) floating/(double)100;

    // timestamp
    read_str(line, temp_str, 24, 5);
    result_transaction->timestamp = atoi(temp_str);

    return result_transaction;
}

struct transaction ** get_transactions(char stat_path[]){
    struct transaction ** all_transactions = (struct transaction **) malloc(sizeof(struct transaction *) * MAX_TRAN);
    FILE * fp = fopen(stat_path, "r");
    for (int i = 0; i < MAX_TRAN; i++) all_transactions[i] = NULL;
    char line[60];
    int cnt = 0;
    while (fgets(line, 60, fp) != NULL) {
        all_transactions[cnt] = process_one_transaction(line);
        cnt += 1;

        if (cnt == MAX_TRAN) {
            // allocate more space
            MAX_TRAN *= 2;
            struct transaction ** all_transactions_temp;
            all_transactions_temp = realloc(all_transactions, sizeof(struct transaction *) * MAX_TRAN);
            all_transactions = all_transactions_temp;
        }
    }
    return all_transactions;

}

void swap(struct transaction *a , struct transaction *b)
{
    struct transaction temp ;
    temp = *a ;
    *a = *b ;
    *b = temp ;
    return ;
}

struct transaction ** sort_transactions(struct transaction ** transaction_i){
    //struct transaction ** all_transactions = (struct transaction **) malloc(sizeof(struct transaction *) * MAX_TRAN);
    //for (int i = 0; i < MAX_TRAN * 2; i++) all_transactions[i] = NULL;
    int transaction_index = 0, temp_index = 0;
    while (transaction_i[transaction_index] != NULL) {
        temp_index = transaction_index;
        while (transaction_i[temp_index] != NULL){
            if (strcmp(transaction_i[temp_index]->transac_account,transaction_i[transaction_index]->transac_account)<0){
                swap(transaction_i[temp_index], transaction_i[transaction_index]);
            }
            else{
                if (strcmp(transaction_i[temp_index]->transac_account,transaction_i[transaction_index]->transac_account)==0){
                    if (transaction_i[temp_index]->timestamp < transaction_i[transaction_index]->timestamp){
                        swap(transaction_i[temp_index], transaction_i[transaction_index]);
                    }
                }
            }
            temp_index += 1;

        }
        transaction_index += 1;
    }
    return transaction_i;
}

void save_transactions(struct transaction ** transactions, char save_path[]){
    FILE * fp = fopen(save_path, "w");
    int transaction_index = 0;
    int integer, floating, positive;
    double amounts;
    char timestamp_str[10];
    char integer_str[10];
    char floating_str[10];
    int timestamp;
    while(transactions[transaction_index] != NULL){

        fprintf(fp, "%.16s%.1s", transactions[transaction_index]->transac_account, transactions[transaction_index]->op_transac);
        if(transactions[transaction_index]->amounts < 0){
            positive = 0;
        }
        amounts = fabs(transactions[transaction_index]->amounts);

        integer = (int) amounts;
        itoa(integer, integer_str, 10);

        if (!positive){
            for (int i=0; i<5-strlen(integer_str)-1;i++){
                fprintf(fp, "%s", " ");
            }
            fprintf(fp, "%s", "-");
        }
        else{
            for (int i=0; i<5-strlen(integer_str);i++){
                fprintf(fp, "%s", "0");
            }
        }
        fprintf(fp, "%s", integer_str);

        floating = (int) (amounts - integer)*100;
        itoa(floating, floating_str, 10);
        fprintf(fp, "%s", floating_str);
        for (int i=0; i<2-strlen(floating_str);i++){
            fprintf(fp, "%s", "0");
        }
        timestamp = transactions[transaction_index]->timestamp;
        itoa(timestamp, timestamp_str, 10);
        for (int i=0; i<5-strlen(timestamp_str);i++){
            fprintf(fp, "%s", "0");
        }
        fprintf(fp, "%s\n", timestamp_str);
        transaction_index += 1;
    }
    fclose(fp);
}

void sort_transaction(char path[], char sort_path[]){
    struct transaction ** transactions = get_transactions(path);
    struct transaction ** transactions_sort = sort_transactions(transactions);
    save_transactions(transactions_sort, sort_path);
}
///////////////////////////////////////////////
