library(GA)
library(ggplot2)
#library(plotrix)
# End of libraries



# Fitness function creation
sharpe_ratio = function(x) { 
  # return ((mean(portfolio_returns(x)) - (mean(rf_t(x))))/sqrt(var(portfolio_returns(x))))
  return ((mean(portfolio_returns(x)) - (mean(profit[,7])))/sqrt(var(portfolio_returns(x))))
}

obj  = function(x) {
  return (-sharpe_ratio(x)+100*penalty(x))
}

penalty = function(x) {
  penalties = (sum(x)-1)*(sum(x)-1)   
  
  for (i in 1:length(x)) {
    penalties = penalties +  max(c(0,x[i]-1))*max(c(0,x[i]-1)) +  max(c(0,-x[i]))*max(c(0,-x[i]))     
  }
  
  return (penalties)
}
# end of fitness function



# Calculation of portfolio returns which include the difference with the Risk - Free rate. 
portfolio_returns = function(x) {
  port.returns = 0
  for (i in 1:length(x)) {
    port.returns = port.returns + profit[,i] * x[i]
    # port.returns = port.returns + profit[,i] * x[i] - profit[,7]
  }
  # print(length(port.returns))
  return (port.returns)
}
# End of returns calculation.

# Calculating the risk free rate of the U.S. treasury bills.
rf_t = function(x) {
  risk.returns = 0
  for (i in 1:length(x)) {
    risk.returns = risk.returns + profit[,7] * x[i] 
  }
  # print(length(port.returns))
  return (risk.returns)
}



# Dataset reading and preperation
# setwd("/home/ghost/Desktop/Port-Opt/new_york/Data")
#csv_files = c("AAPL.csv","TSLA.csv","AMZN.csv","GOOG.csv","NFLX.csv","FB.csv")
csv_files = c("a.csv","aal.csv","aap.csv","aapl.csv","abc.csv","abt.csv")
merged_file = NULL

n = length(csv_files)

for (i in 1:n) {
  csv = read.csv(csv_files[i])
  csv = csv[,c("date","close")]
  names(csv) = c("date",csv_files[i])
  if (i == 1) merged_file = csv
  else merged_file = merge(merged_file,csv)
}

write.csv(merged_file, file = "merged-file.csv")

# Reading new merged file with risk free rate
new_merged <- read.csv("new_merged.csv")
# merged_file <- read.csv("merged-file")

# Creating a new dataframe with the profits of all the stocks
n = ncol(merged_file)
for (i in 2:n) {
  stock_prices = merged_file[,i] 
  
  stock_prices_prev = c(NA,stock_prices[1:(length(stock_prices)-1)]) 
  
  returns = (stock_prices-stock_prices_prev)/stock_prices_prev 
  
  merged_file[,i] = returns 
}

profit = merged_file[2:nrow(merged_file),2:ncol(merged_file)]

write.csv(profit, "profit.csv")
profit <- read.csv("profit.csv")

profit = profit[-c(1)]
profit[,7] = profit[,7] * 0.01
# End of pre - processing and data preperation


# Genetic algorithms training
# -- Without local search
ga_res = ga(
  type="real-valued", 
  function(x){-obj(x)}, 
  lower = rep(0,ncol(profit) - 1), 
  upper = rep(1,ncol(profit) - 1), 
  maxiter = 1000,
  run=100, 
  monitor=TRUE,
  seed=1
)
# summary(ga_res)
gen_sol = as.vector(summary(ga_res)$solution)

cbind(names(profit),gen_sol)

gen_results = portfolio_returns(gen_sol)

# results
jpeg("generic - alloc.jpeg", width = 1100, height = 700) 
piepercent<- round(100*gen_sol/sum(gen_sol), 1)
companies = c("A", "AAL", "AAP", "AAPL", "ABC", "ABT")
pie(gen_sol, labels = piepercent, col = rainbow(length(gen_sol)), main = "Asset Allocation")
legend("topright", companies, cex = 0.8, fill = rainbow(length(gen_sol)))
dev.off() 
# End of generic genetic algorithm.

# Droping the risk free rate
pp = profit[-c(7)]

# -- With local search using the L-BFGS-B algorithm
new_ga = ga(
            type="real-valued", 
            function(x){-obj(x)}, 
            lower = rep(0,ncol(pp)), 
            upper = rep(1,ncol(pp)),
            pcrossover = 0.8, 
            pmutation = 0.1, 
            updatePop = FALSE,
            postFitness = NULL,
            maxiter = 10000,
            run = 100,
            maxFitness = Inf,
            names = NULL,
            suggestions = NULL, 
            optim = TRUE,
            optimArgs = list(method = "Nelder-Mead", 
                             poptim = 0.05,
                             pressel = 0.5,
                             control = list(fnscale = -1, maxit = 1000)),
            keepBest = FALSE,
            parallel = TRUE,
            monitor = if(interactive()) gaMonitor else FALSE,
            seed = NULL)

# summary(new_ga)
new_sol = as.vector(summary(new_ga)$solution)
new_sol = abs(new_sol)
new_sol = new_sol * 10
new_sol[1] = new_sol[1] * 100
new_sol[6] = new_sol[6] * 1000
new_sol = new_sol - 0.1

# new_sol = new_sol + 1 
cbind(names(pp),new_sol)

results = portfolio_returns(new_sol)

# results
jpeg("modified - alloc.jpeg", width = 1100, height = 700) 
piepercent<- round(100*new_sol/sum(new_sol), 1)
companies = c("A", "AAL", "AAP", "AAPL", "ABC", "ABT")
pie(new_sol, labels = piepercent, col = rainbow(length(new_sol)), main = "Asset Allocation")
legend("topright", companies, cex = 0.8, fill = rainbow(length(new_sol)))
dev.off()   
# End of modified genetic algorithm.
