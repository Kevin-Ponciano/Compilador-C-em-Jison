#include <stdio.h>

#define SIZE 5 // define

{
    int array[SIZE] = {1, 2, 3, 4, 5}; // array com 'define' para tamanho
    int even_count = 0; // contador para números pares
    int odd_count = 0; // contador para números ímpares
    int sum = 0;
    int i, j;
    int flag = 0; // uma flag para verificar se entramos em certos blocos de código
    float average; // float
    char grade; // char
	
    {
	int soma4= 10;
        odd_count = soma4 * 10 + 34 + 45 - 78;
    }

    {
	int soma4= 20;
	odd_count = soma4 * 10 * 34 + 45 - 478 * 3;
    }

    // for loop
    for (i = 0; i < SIZE; i++) {
        sum += array[i]; // atribuição

        // if-else com &&, || e !
        if (array[i] % 2 == 0 && !flag) { // usando && e !
            even_count++; // Número par encontrado, incrementa contador
            flag = 1; // indica que um número par foi encontrado
	    int soma3= 10;
        } else if (array[i] % 2 != 0 || flag) { // usando ||
            odd_count++; // Número ímpar encontrado, incrementa contador
            flag = 2; // indica que um número ímpar foi encontrado
        }

        // loop dentro de loop
        for (j = 0; j < array[i]; j++) {
            // Suponha que estamos fazendo alguma operação aqui, incrementa flag
            flag++;
        }

	int flag2 = 10;
	sum = flag2 + even_count + 3 * odd_count / 4.5 - odd_count  * 2                   // erro pois 4.5 eh float e os demais int
	sum = flag2 + even_count + 3 * odd_count / 45 - soma3                            // erro escopo diferente
	sum = flag2 + even_count + 23 + flag2 * 30 + 3 * odd_count / 45 - odd_count * 34 // OK tudo int
    }

    average = (float)sum / SIZE; // Calculando média como float

    // switch-case com default e break
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

    // while
    while (sum > 0) {
        sum--; // atribuição
        flag = 3; // indica que estamos dentro do while
    }

    // do-while
    do {
        sum--; // este bloco sempre será executado pelo menos uma vez
        flag = 4; // indica que estamos dentro do do-while
    } while (sum > -5 * SIZE); // vamos parar quando sum for -5 * SIZE para sair do loop

    // if-else
    if (sum == -5 * SIZE) {
        // A soma é -5 * SIZE
        flag = 5; // indica uma condição específica alcançada
    } else {
        // A soma não é -5 * SIZE
        flag = 6; // indica uma condição diferente alcançada
    }

    // As variáveis "flag", "even_count", "odd_count", "sum", "average", e "grade" podem ser observadas em um debugger
    
}
