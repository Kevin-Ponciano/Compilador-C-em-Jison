#include <stdio.h>
#include <stdlib.h>

int main() {
    int *arr;   // Ponteiro para armazenar o endereço do array
    int n, i;

    // Solicita ao usuário o tamanho do array
    printf("Digite o tamanho do array: ");
    scanf("%d", &n);

    // Aloca dinamicamente o array usando malloc
    arr = (int *)malloc(n * sizeof(int));

    // Verifica se a alocação foi bem-sucedida
    if (arr == NULL) {
        printf("Erro: Falha na alocação de memória!\n");
        return 1;  // Encerra o programa em caso de erro
    }

    // Preenche o array com valores e imprime os valores armazenados
    printf("Preenchendo o array com valores:\n");
    for (i = 0; i < n; i++) {
        arr[i] = i * 10;  // Exemplo de valores
        printf("arr[%d] = %d\n", i, arr[i]);
    }

    // Desaloca a memória alocada dinamicamente
    free(arr);
    printf("Memória desalocada com sucesso.\n");

    return 0;
}
