
# Function to create dataset for plotting, given...
#   win_1, win_2, win_3: the list of RLs and CIs by region for each window
# currently only for 3 windows
win_rl_to_plot_dat <- function(win_1, win_2, win_3){
  reg <- list()
  for(i in 1:3){ # by region
    # win_1[[i]] # is region i values from window 1
    dat_25 <- rbind(win_1[[i]][1, ], win_2[[i]][1, ], win_3[[i]][1, ])
    dat_100 <- rbind(win_1[[i]][2, ], win_2[[i]][2, ], win_3[[i]][2, ])
    dat_500 <- rbind(win_1[[i]][3, ], win_2[[i]][3, ], win_3[[i]][3, ])
    dat <- cbind(dat_25, dat_100, dat_500)
    colnames(dat) <- c("RL_25", "LB_25", "UB_25", "RL_100", "LB_100", "UB_100","RL_500", "LB_500", "UB_500")
    reg[[i]] <- as.data.frame(dat)
  }
  return(reg)
}
