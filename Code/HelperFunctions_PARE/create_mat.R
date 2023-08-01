
# Function to calculate a vector of return levels by region, given...
#   n_vec: a vector of the number of stations in each region
#   hMat: the extended Hausdorff distance matrix you want to use (make sure to convert to miles first)
#   c: the constant to define the distance between points within the same region (defaults to 1)
create_mat <- function(n_vec, hMat, c = 1){
  n1 = n_vec[1]
  n2 = n_vec[2]
  n3 = n_vec[3]
  # Creating blocks for block matrix
  one_to_one <- matrix(data = rep(c, n1^2), nrow = n1, ncol = n1)
  two_to_two <- matrix(data = rep(c, n2^2), nrow = n2, ncol = n2)
  three_to_three <- matrix(data = rep(c, n3^2), nrow = n3, ncol = n3)
  one_to_two <- matrix(data = rep(hMat[1,2], n1*n2), nrow = n1, ncol = n2)
  one_to_three <- matrix(data = rep(hMat[1,3], n1*n3), nrow = n1, ncol = n3)
  two_to_one <- matrix(data = rep(hMat[1,2], n1*n2), nrow = n2, ncol = n1)
  three_to_one <- matrix(data = rep(hMat[1,3], n1*n3), nrow = n3, ncol = n1)
  two_to_three <- matrix(data = rep(hMat[2,3], n2*n3), nrow = n2, ncol = n3)
  three_to_two <- matrix(data = rep(hMat[2,3], n2*n3), nrow = n3, ncol = n2)
  # putting blocks together
  one <- cbind(one_to_one, one_to_two, one_to_three)
  two <- cbind(two_to_one, two_to_two, two_to_three)
  three <- cbind(three_to_one, three_to_two, three_to_three)
  D_all <- rbind(one, two, three) # n x n matrix, where n = n1 + n2 + n3
  return(D_all)
}