#include <stdlib.h>
#include <stdio.h>

void fail(){
	printf("Execution failed\n");
	exit(1);
}

void print_mu(int* mu, int len) {
	printf("memory: [");

	for(int i = 0; i < len; i++) {
		printf("%d, ", mu[i]);
	}

	printf("]\n");
}