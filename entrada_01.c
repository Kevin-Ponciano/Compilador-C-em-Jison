{
    int array[5] = {1, 2, 3, 4, 5}; // array
    int even_count = 0; // contador para números pares
    int odd_count = 0; // contador para números ímpares
    int sum = 0;
    int i, j;
    int flag = 0; // uma flag para verificar se entramos em certos blocos de código

    // for loop
    for (i = 0; i < 5; i++) {
        sum += array[i]; // atribuição

        // if dentro de loop
        if ( (array[i] % 2 == 0) && (sum > 10 || (sum <= 300) ) ) {
            even_count++; // Número par encontrado, incrementa contador
            flag = 1; // indica que um número par foi encontrado
        } else {
            odd_count++; // Número ímpar encontrado, incrementa contador
            flag = 2; // indica que um número ímpar foi encontrado
        }

        // loop dentro de loop
        for (j = 0; j < array[i]; j++) {
            // Suponha que estamos fazendo alguma operação aqui, incrementa flag
            flag++;
	    while(j > 1)
		float nada = 12.34;
        }
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
    } while (sum > -5); // vamos parar quando sum for -5 para sair do loop

    // if-else
    if (sum == -5) {
        // A soma é -5
        flag = 5; // indica uma condição específica alcançada
    } else {
        // A soma não é -5
        flag = 6; // indica uma condição diferente alcançada
    }

    // As variáveis "flag", "even_count", "odd_count" e "sum" podem ser observadas em um debugger
    
}
