# Function to create a symmetric matrix usable in the CAR model, given...
#   mat: a square (preferably symmetric) matrix
jitter_n_sym <- function(mat){
  # adding rnorm to jitter values
  mat_jit <- mat + matrix(rnorm(nrow(mat) * ncol(mat), sd = 0.1), ncol = ncol(mat))
  # Make symmetric
  mat_jit_sym <- mat_jit
  mat_jit_sym[upper.tri(mat_jit_sym)] <- t(mat_jit_sym)[upper.tri(mat_jit_sym)]
  return(mat_jit_sym)
}