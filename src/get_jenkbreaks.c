// C code to calculate Jenks natural breaks
// Ported from R package seqInt by Pascal Title, January 2015
// inputs:
// - d1 = vector of numeric values
// - k1 = number of categories
// length_d1 = length of the d vector
// brks = empty vector to receive results

#include <float.h>
#include <R.h>

// Function declarations
void rcpp_get_jenkbreaks(double *d, int *k1, int *length_d1, double *brks);

void rcpp_get_jenkbreaks(double *d, int *k1, int *length_d1, double *brks)
{

    int i3;
    int i4;
    double v;
    double val;
    double id;

    int k = k1[0];
    int nCat = k;
    int length_d = length_d1[0];

    double** mat1 = Calloc(length_d, double*);
    double** mat2 = Calloc(length_d, double*);

    for (int i = 0; i < length_d; i++) {
        double* row = Calloc(k, double);
        for (int j = 0; j < k; j++) {
            row[j] = 1.0;
        }
        mat1[i] = row;
    }

    for (int i = 0; i < length_d; i++) {
        double* row = Calloc(k, double);
        for (int j = 0; j < k; j++) {
            row[j] = 0.0;
        }
        mat2[i] = row;
    }

    v = 0;

    //Fill mat2 with initial values
    for (int i = 1; i < length_d; i++) {
        for (int j = 0; j < k; j++) {
            mat2[i][j] = FLT_MAX;
        }
    }

    //Begin
    for (int l = 2; l <= length_d; l++) {
        double s1 = 0;
        double s2 = 0;
        double w = 0;

        for (int m = 1; m <= l; m++) {
            i3 = l - m + 1;
            val = d[(i3-1)];
            s2 = s2 + val * val;
            s1 = s1 + val;
            w = w + 1;
            v = s2 - (s1 * s1) / w;
            i4 = (int)(i3 - 1);

            if (i4 != 0) {
                for (int j = 2; j <= k; j++) {
                    if (mat2[l-1][j-1] >= (v + mat2[i4-1][j-2])) {
                        mat1[l-1][j-1] = i3;
                        mat2[l-1][j-1] = v + mat2[i4-1][j-2];
                    }
                }
            }
        }
        mat1[l-1][0] = 1;
        mat2[l-1][0] = v;
    }

    double kclass[k];
    for (int i = 1; i <= k; i++) {
        kclass[i-1] = i;
    }

    kclass[k - 1] = length_d;
    k = length_d;

    for (int j = nCat; j > 1; j--) {
        id = (int) (mat1[k-1][j-1]) - 1;
        //printf("j = %d\n", j);
        kclass[j - 2] = id;
        k = id;
    }

    brks[0] = d[0];
    for (int i = 1; i < (nCat + 1); i++) {
        brks[i] = d[(int)(kclass[i - 1]) - 1];
    }

    //delete objects
    for (int i = 0; i < length_d; i++) {
        Free(mat1[i]);
        Free(mat2[i]);
    }

    Free(mat1);
    Free(mat2);
}
