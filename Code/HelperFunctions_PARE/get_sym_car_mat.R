# Shortcut function to call other functions to get to the symmetrical distance matrix 
# (still make sure to take reciprocal of distance matrix for weight matrix - a.k.a. inverse of each entry, NOT inverse of matrix)
get_sym_car_mat <- function(data, hMat, c=1){
  test_num <- summary(data$Reg_fac) # taking summary of factors gives the number of stations in each factor
  test_D <- create_mat(test_num, hMat, c)
  test_D_sym <- jitter_n_sym(test_D)
  return(test_D_sym)
}
