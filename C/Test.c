#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main(){
    printf("Input: ");
    char *input = malloc(60*sizeof(char));
    char *copy1 = malloc(60*sizeof(char));
    char *copy2 = malloc(60*sizeof(char));
    scanf("%s", input);
    input[strlen(input)] = '\0';


    strncpy(copy1, input, 3);
    strncpy(copy2, input+1, 3);

    printf("input: %s\n", input);
    printf("copy1: %s   copy2: %s\n", copy1, copy2);
    return 0;
}
