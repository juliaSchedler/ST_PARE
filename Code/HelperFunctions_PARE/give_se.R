# Function to get SE's from a list of varcov matrices (useful for kriging model)
give_se <- function(varcov_list){ 
  se_vec <- NULL
  for (i in 1:length(varcov_list)){
    se_vec <- cbind(se_vec, sqrt(diag(varcov_list[[i]])))
  }
  rownames(se_vec) <- c("scale", "shape")
  colnames(se_vec) <- c("Reg1", "Reg2", "Reg3")
  return(se_vec)
}