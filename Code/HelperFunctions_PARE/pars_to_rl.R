# extRemes::return.level - calculates return levels given an fevd object
# extRemes::rlevd() - calculates return levels given a distribution and parameter estimates

# Function to calculate a vector of return levels by region, given...
#   parmat: the parameter matrix for the regions
#   return_period: a return period
pars_to_rl <- function(par_mat, return_period){
  RL_vec <- NULL
  for(reg in 1:3){
    scale <- par_mat[1, reg]
    shape <- par_mat[2, reg]
    rate  <- par_mat[3, reg]
    RL_vec[reg] <- extRemes::rlevd(period=return_period, type="GP", scale=scale, shape=shape, rate=rate, threshold=thresh)
  }
  return(RL_vec)
}