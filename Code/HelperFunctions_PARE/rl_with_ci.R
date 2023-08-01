# Function to calculate return level CIs given parameter estimates and varcov matrix...
# using code pulled from functions in extRemes package
rl_with_ci <- function(par_mat, varcov_list, return_period, type = "ci", alpha = 0.05){
  out_ci <- out_se <- NULL
  for(i in 1:length(varcov_list)){  # for Region i
    cov.theta <- varcov_list[[i]]
    scale <- par_mat[1,i]
    shape <- par_mat[2,i]
    rate  <- par_mat[3,i]
    p <- extRemes::rlevd(period=return_period, type="GP", scale=scale, shape=shape, rate=rate, threshold=thresh)
    # p <- rlevd(period = return.period, loc = loc, scale = scale, 
    #            shape = shape, threshold = x$threshold, type = mod, 
    #            npy = x$npy, rate = lam)
    # p gives the basic return level!
    
    z.alpha <- qnorm(alpha/2, lower.tail = FALSE)
    
    # grads <- rlgrad.fevd(x, period = return.period)
    lam <- rate
    n = 14610 # while working with 40-year windows
    npy = 365.25
    m <- return_period * npy
    mlam <- m * lam
    grads <- cbind(scale * m^(-shape) * lam^(-shape - 1), 
                   (shape)^(-1) * ((mlam)^(shape) - 1), 
                   -scale * (shape)^(-2) * ((mlam)^(shape) - 1) + (scale/shape) * (mlam)^(shape) * log(mlam))
    grads <- t(grads)
    cov.theta <- rbind(c(lam * (1 - lam)/n, 0, 0), 
                       cbind(0, cov.theta))
    var.theta <- t(grads) %*% cov.theta %*% grads
    
    # return(var.theta)
    # which.par = 1
    
    # now make it work for all 3 regions!
    
    if(type=="se"){  # if type="se", give return level and standard error of RL
      rl_se <- sqrt(var.theta)
      out_se <- rbind(out_se, c(p, rl_se))
    }else{          # if type="ci" (default), give return level with CI
      out <- c(p, p - z.alpha * sqrt(var.theta),
               p + z.alpha * sqrt(var.theta))
      out_ci <- rbind(out_ci, out)
    }
  }
  if(type=="se"){  # if type="se", give return level and standard error of RL
    rownames(out_se) <- c("Reg1", "Reg2", "Reg3")
    colnames(out_se) <- c(paste0(return_period, "-yr RL"), "SE")
    return(out_se)
  }else{          # if type="ci" (default), give return level with CI
    rownames(out_ci) <- c("Reg1", "Reg2", "Reg3")
    colnames(out_ci) <- c(paste0(return_period, "-yr RL"), "LB", "UB")
    return(out_ci)
  }
}