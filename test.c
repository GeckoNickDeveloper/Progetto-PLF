#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define PI 3.14159265359

void dct(double c[], int N) {
	double* C = (double*) malloc(sizeof(double) * N);

	int n, k;

	for (k = 0; k < N; k++)
	{
		for (n = 0, C[k] = 0; n < N; n++)
		{
			C[k] += c[n] * cos((PI * (2 * n + 1) * k) / (2 * N));
		}

		C[k] *= 2;
	}

	for (k = 0; k < N; k++)
	{
		c[k] = C[k];
	}
	
	
	free(C);
}


int main() {
	double c[] = {1.0, 3.7, 10.9, -2.6};

	int i;

	dct(c, 4);

	for (i = 0; i < 4; i++)
	{
		printf("%f\n", c[i]);
	}
	
}