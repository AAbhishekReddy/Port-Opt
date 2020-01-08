library(GA)
library(ggplot2)
#library(plotrix)


sharpe_ratio = function(x) { 
  return (mean(portfolio_returns(x))/sqrt(var(portfolio_returns(x))))
  
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


portfolio_returns = function(x) {
  port.returns = 0
  
  for (i in 1:length(x)) {
    port.returns = port.returns + profit[,i] * x[i]
  }
  
  return (port.returns)
}


#setwd("/home/ghost/Desktop/Port-Opt/data/")
csv_files = c("AAPL.csv","TSLA.csv","AMZN.csv","GOOG.csv","NFLX.csv","FB.csv")
merged_file = NULL

n = length(csv_files)

for (i in 1:n) {
  csv = read.csv(csv_files[i])
  csv = csv[,c("Date","Close")]
  names(csv) = c("Date",csv_files[i])
  if (i == 1) merged_file = csv
  else merged_file = merge(merged_file,csv)
}

write.csv(merged_file, file = "merged-file.csv")

n = ncol(merged_file)
for (i in 2:n) {
  stock_prices = merged_file[,i] 
  
  stock_prices_prev = c(NA,stock_prices[1:(length(stock_prices)-1)]) 
  
  returns = (stock_prices-stock_prices_prev)/stock_prices_prev 
  
  merged_file[,i] = returns 
}

profit = merged_file[2:nrow(merged_file),2:ncol(merged_file)]



ga_res = ga(
  type="real-valued", 
  function(x){-obj(x)}, 
  lower = rep(0,ncol(profit)), 
  upper = rep(1,ncol(profit)), 
  maxiter = 1000,
  run=100, 
  monitor=TRUE,
  seed=1
)

new_ga = ga(
            type="real-valued", 
            function(x){-obj(x)}, 
            lower = rep(0,ncol(profit)), 
            upper = rep(1,ncol(profit)),
            #population = gaControl(gareal)$population,
            #selection = gaControl(type)$selection,
            #crossover = gaControl(type)$crossover, 
            #mutation = gaControl(type)$mutation,
            #popSize = 50, 
            pcrossover = 0.8, 
            pmutation = 0.1, 
            #elitism = base::max(1, round(popSize*0.05)), 
            updatePop = FALSE,
            postFitness = NULL,
            maxiter = 10000,
            run = 100,
            maxFitness = Inf,
            names = NULL,
            suggestions = NULL, 
            optim = TRUE,
            optimArgs = list(method = "L-BFGS-B", 
                             poptim = 0.05,
                             pressel = 0.5,
                             control = list(fnscale = -1, maxit = 100)),
            keepBest = FALSE,
            parallel = TRUE,
            monitor = if(interactive()) gaMonitor else FALSE,
            seed = NULL)

summary(ga_res)
sol = as.vector(summary(ga_res)$solution)

cbind(names(profit),sol)

results = portfolio_returns(sol)

results
jpeg("asset-alloc.jpeg", width = 1100, height = 700) 
piepercent<- round(100*sol/sum(sol), 1)
companies = c("AAPL", "TSLA", "AMZN", "GOOG", "NFLX", "FB")
pie(sol, labels = piepercent, col = rainbow(length(sol)), main = "Asset Allocation")
legend("topright", companies, cex = 0.8, fill = rainbow(length(sol)))
dev.off() 

