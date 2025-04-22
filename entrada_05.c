#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 5
#define MAX 100

// Struct, Union, Enum, Typedef
struct Data {
    int id;
    char name[50];
};

union Value {
    int i;
    float f;
};

enum Color { RED, GREEN, BLUE };
typedef unsigned long ULong;

// Protótipo de função
void printArray(int *arr, int size);

int main() {
    // Variáveis de diferentes tipos
    int array[SIZE] = {1, 2, 3, 4, 5};
    int matrix[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};
    int even_count = 0, odd_count = 0, sum = 0, i, j, k;
    int flag = 0;
    float average;
    char grade;
    int *dynamic_array;
    int n = 10;

    // Struct, union, enum
    struct Data data1 = {1, "John"};
    union Value val;
    val.i = 42;
    enum Color color = GREEN;
    ULong bigNum = 123456789;

    // Alocação dinâmica
    dynamic_array = (int *)malloc(n * sizeof(int));
    if (dynamic_array == NULL) {
        printf("Erro ao alocar memória\n");
        return 1;
    }

    // Inicialização do array dinâmico
    for (i = 0; i < n; i++) {
        dynamic_array[i] = i * 10;
    }

    // Laços e condicionais aninhados
    for (i = 0; i < SIZE; i++) {
        sum += array[i];

        if (array[i] % 2 == 0) {
            if (sum > 10) {
                even_count++;
                flag = 1;
            } else {
                if (array[i] > 0)
                    flag = 7;
            }
        } else {
            odd_count++;
            flag = 2;
        }

        for (j = 0; j < array[i]; j++) {
            flag++;
            while (j > 1) {
                do {
                    int temp = j;
                    if (temp % 2 == 0)
                        break;
                    else
                        j--;
                } while (j > 0);
            }
        }
    }

    // Switch com if e while dentro
    switch (color) {
        case RED:
            if (sum > 50)
                printf("Cor vermelha e soma alta.\n");
            break;
        case GREEN:
            while (flag < 3) {
                printf("Cor verde com flag %d\n", flag);
                flag++;
            }
            break;
        case BLUE:
            printf("Cor azul\n");
            break;
        default:
            printf("Cor desconhecida\n");
    }

    // Média e nota
    average = (float)sum / SIZE;
    switch ((int)average / 10) {
        case 10:
        case 9:
        case 8:
            grade = 'A';
            break;
        case 7:
        case 6:
            grade = 'B';
            break;
        case 5:
            grade = 'C';
            break;
        default:
            grade = 'D';
    }

    printf("Média: %.2f, Nota: %c\n", average, grade);

    // while
    while (sum > 0) {
        sum--;
        flag = 3;
    }

    // do-while
    do {
        sum--;
        flag = 4;
    } while (sum > -5 * SIZE);

    // if-else
    if (sum == -5 * SIZE) {
        flag = 5;
    } else {
        flag = 6;
    }

    printArray(dynamic_array, n);
    free(dynamic_array);
    return 0;
}

void printArray(int *arr, int size) {
    printf("Array: ");
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}
