library(tidyverse)
library(reshape2)

S0 = 100      # initial stock price
mu = 0.05     # drift
sigma = 0.15  # volatility
r = 0.03      # risk-free rate
M = 1         # maturity
T_ = 24        # number of time steps
N_MC = 10000  # number of paths
delta_t = M / T_ # time interval

# BS-Simulation
set.seed(42) # set seed

#stock prices 1000x24
S = matrix(data = rep(NA,(N_MC) * T_), nrow = N_MC, ncol = T_)
S[, 1] = S0

#standard normal random numbers 1000x24
RN = matrix(data = rnorm(N_MC * T_, 0, 1), nrow = N_MC, ncol = T_)

for (t in 2:T_){
  S[,t] = S[,t-1] * exp((mu - (1/2) * sigma^2) * delta_t + sigma * sqrt(delta_t) * RN[,t])
}


delta_S = S[,2:T_] - exp(r * delta_t) * S[,1:(T_-1)]
delta_S_hat = delta_S - apply(delta_S, 1, mean)

#plotting
df_S = data.frame(t(S[1:5,]))
df_S = cbind(1:T_, df_S)
colnames(df_S)[1] = "time"

df_S <- melt(df_S ,  id.vars = 'time', variable.name = 'series')
ggplot(df_S, aes(time,value)) + geom_line(aes(colour = series))
