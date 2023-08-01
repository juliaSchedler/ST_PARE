# Function to convert return level CI matrices into vectors for easy plotting

# separate by region! For each region, will have matrix made of diff RL values (25, 100, 500 and bounds)
rl_ci_to_plot_vec <- function(rl_mat_25, rl_mat_100, rl_mat_500){
  reg <- list()
  for ( i in 1:dim(rl_mat_25)[1]){ #i.e. 1:3
    vec_25  <- rl_mat_25[i, ]
    vec_100 <- rl_mat_100[i, ]
    vec_500 <- rl_mat_500[i, ]
    mat <- rbind(vec_25, vec_100, vec_500)
    rownames(mat) <- c("25-yr", "100-yr", "500-yr")
    colnames(mat)[1] <- "RL"
    reg[[i]] <- mat
  }
  return(reg)
}
