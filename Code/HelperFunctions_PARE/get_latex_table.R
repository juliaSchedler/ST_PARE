get_latex_table <- function(est, se){
  for(i in 1:nrow(est)){
  cat("&")
  cat(rownames(est)[i],paste(round(est[i,],2), "(", 
                             round(se[i,],4), ")", sep =""), sep = "&")
    cat("\\\\")
      cat("\n")
    }
}

get_latex_table_RL <- function(RL_list){
  for(i in 1:length(RL_list)){
    cat("&")
    cat(colnames(RL_list[[i]])[1],paste(round(RL_list[[i]][,1],3), "(", 
              round(RL_list[[i]][,2],4), ")", sep =""), sep = "&")
    cat("\\\\")
    cat("\n")
  }
}