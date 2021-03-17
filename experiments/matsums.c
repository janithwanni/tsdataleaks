#include <stdlib.h>
#include <Rinternals.h>
static inline void rowSums_template_sxt (size_t s, size_t t,
                                         double *A, size_t LDA,
                                         size_t nr, size_t nc,
                                         double *sum) {

  size_t ncr = nc % t, nrr = nr % s, i;
  double *A_end = A + LDA * nc, *B;
  double a0, a1;

  for (i = 0; i < nr; i++) sum[i] = 0.0;  // necessary initialization

  if (ncr > 0) {  // is there a "fractional loop" for the outer loop?
    if (nrr > 0) sum[0] += A[0];  // is there a "fractional loop" for the inner loop?
    for (i = nrr; i < nr; i += s) {  // main inner loop with depth-s
      sum[i] += A[i];
      if (s > 1) sum[i + 1] += A[i + 1];
      }
    A += LDA;
    }

  for (; A < A_end; A += t * LDA) {  // main outer loop with depth-t
    if (t > 1) B = A + LDA;
    if (nrr > 0) {  // is there a "fractional loop" for the inner loop?
      a0 = A[0]; if (t > 1) a0 += A[LDA];
      sum[0] += a0;
      }
    for(i = nrr; i < nr; i += s) {  // main inner loop with depth-s
      a0 = A[i]; if (t > 1) a0 += B[i];
      sum[i] += a0;
      if (s > 1) {
        a1 = A[i + 1]; if (t > 1) a1 += B[i + 1];
        sum[i + 1] += a1;
        }
      }
    }

  }

#define macro_define_rowSums(s, t, rowSums_sxt) \
SEXP rowSums_sxt (SEXP matA, SEXP chunk_size) { \
  double *A = REAL(matA); \
  size_t nrow_A = (size_t)nrows(matA); \
  size_t ncol_A = (size_t)ncols(matA); \
  SEXP result = PROTECT(allocVector(REALSXP, nrows(matA))); \
  double *sum = REAL(result); \
  size_t block_size = (size_t)asInteger(chunk_size); \
  size_t i, block_size_i; \
  if (block_size > nrow_A) block_size = nrow_A; \
  for (i = 0; i < nrow_A; i += block_size_i) { \
    block_size_i = nrow_A - i; if (block_size_i > block_size) block_size_i = block_size; \
    rowSums_template_sxt(s, t, A, nrow_A, block_size_i, ncol_A, sum); \
    A += block_size_i; sum += block_size_i; \
    } \
  UNPROTECT(1); \
  return result; \
  }

macro_define_rowSums(1, 1, rowSums_1x1)
macro_define_rowSums(1, 2, rowSums_1x2)
macro_define_rowSums(2, 1, rowSums_2x1)
macro_define_rowSums(2, 2, rowSums_2x2)
