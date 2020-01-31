library(GA)
library(ggplot2)
#library(plotrix)


old_sr = function(x) { 
  return (mean(old_portfolio_returns(x))/sqrt(var(old_portfolio_returns(x))))
  
}

old_obj  = function(x) {
  return (-old_sr(x)+100*penalty(x))
}

old_penalty = function(x) {
  penalties = (sum(x)-1)*(sum(x)-1)   
  
  for (i in 1:length(x)) {
    penalties = penalties +  max(c(0,x[i]-1))*max(c(0,x[i]-1)) +  max(c(0,-x[i]))*max(c(0,-x[i]))     
  }
  
  return (penalties)
}


old_portfolio_returns = function(x) {
  port.returns = 0
  
  for (i in 1:length(x)) {
    port.returns = port.returns + profit_old[,i] * x[i]
  }
  
  return (port.returns)
}


#setwd("/home/ghost/Desktop/Port-Opt/new_york")
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

write.csv(merged_file, file = "merged-file-old.csv")

n = ncol(merged_file)
for (i in 2:n) {
  stock_prices = merged_file[,i] 
  
  stock_prices_prev = c(NA,stock_prices[1:(length(stock_prices)-1)]) 
  
  returns = (stock_prices-stock_prices_prev)/stock_prices_prev 
  
  merged_file[,i] = returns 
}

profit_old = merged_file[2:nrow(merged_file),2:ncol(merged_file)]


# Without local search
old_ga = ga(
  type="real-valued", 
  function(x){-old_obj(x)}, 
  lower = rep(0,ncol(profit_old)), 
  upper = rep(1,ncol(profit_old)), 
  maxiter = 1000,
  run=100, 
  monitor=TRUE,
  seed=1
)


# With local search
# new_ga = ga(
#   type="real-valued", 
#   function(x){-old_obj(x)}, 
#   lower = rep(0,ncol(profit_old)), 
#   upper = rep(1,ncol(profit_old)),
#   pcrossover = 0.8, 
#   pmutation = 0.1, 
#   updatePop = FALSE,
#   postFitness = NULL,
#   maxiter = 10000,
#   run = 100,
#   maxFitness = Inf,
#   names = NULL,
#   suggestions = NULL, 
#   optim = TRUE,
#   optimArgs = list(method = "L-BFGS-B", 
#                    poptim = 0.05,
#                    pressel = 0.5,
#                    control = list(fnscale = -1, maxit = 100)),
#   keepBest = FALSE,
#   parallel = TRUE,
#   monitor = if(interactive()) gaMonitor else FALSE,
#   seed = NULL)

summary(old_ga)
old_sol = as.vector(summary(old_ga)$old_solution)

cbind(names(profit_old),old_sol)

results = old_portfolio_returns(old_sol)

results
jpeg("asset-alloc - old.jpeg", width = 1100, height = 700) 
piepercent<- round(100*old_sol/sum(old_sol), 1)
companies = c("A", "AAL", "AAP", "AAPL", "ABC", "ABT")
pie(old_sol, labels = piepercent, col = rainbow(length(old_sol)), main = "Asset Allocation")
legend("topright", companies, cex = 0.8, fill = rainbow(length(old_sol)))
dev.off() 