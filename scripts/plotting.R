# Over lay graphs
jpeg("overlay.jpeg", width = 1100, height = 700) 

plot(cumsum(results),type="l",lwd=5, col = "black", main = "Optimized Portfolio")

lines(cumsum(profit[,1]), col = "red")
lines(cumsum(profit[,2]), col = "green")
lines(cumsum(profit[,3]), col = "blue")
lines(cumsum(profit[,4]), col = "yellow")
lines(cumsum(profit[,5]), col = "orange")
lines(cumsum(profit[,6]), col = "purple")
legend("topleft", legend=c("AAPL", "TSLA", "AMZN", "GOOG", "NFLX", "FB"),
       col=c("red", "green", "blue", "yellow", "orange", "purple"), lty=1, cex=0.8)
dev.off() 

# profit induvidual distribution
jpeg("opt-profit.jpeg", width = 1100, height = 700) 
plot(results, type = 'l', lwd = 8, col = "black", main = "Optimised Portfolio", ylab = "Profit")
dev.off() 

jpeg("aapl-profit.jpeg", width = 1100, height = 700) 
plot(profit[,1], type = 'l', lwd = 8, col = "red", main = "AAPL", ylab = "Profit")
dev.off() 

jpeg("tsla-profit.jpg", width = 1100, height = 700) 
plot(profit[,2], type = 'l', lwd = 8, col = "green", main = "TSLA", ylab = "Profit")
dev.off() 

jpeg("amzn-profit.jpeg", width = 1100, height = 700) 
plot(profit[,3], type = 'l', lwd = 8, col = "blue", main = "AMZN", ylab = "Profit")
dev.off() 

jpeg("goog-profit.jpeg", width = 1100, height = 700) 
plot(profit[,4], type = 'l', lwd = 8, col = "yellow", main = "GOOG", ylab = "Profit")
dev.off() 

jpeg("nflx-profit.jpeg", width = 1100, height = 700) 
plot(profit[,5], type = 'l', lwd = 8, col = "orange", main = "NFLX", ylab = "Profit")
dev.off() 

jpeg("fb-profit.jpeg", width = 1100, height = 700) 
plot(profit[,6], type = 'l', lwd = 3, col = "purple", main = "FB", ylab = "Profit")
dev.off() 


# Cumilative sum plots

jpeg("opt-cumsum.jpeg", width = 1100, height = 700) 
plot(cumsum(results), type = "l", lwd = 8, col = "black", main = "Optimized portfolio")
dev.off() 

jpeg("aapl-cumsum.jpeg", width = 1100, height = 700) 
plot(cumsum(profit[,1]), type = "l", lwd = 8, col = "red", main = "AAPL")
dev.off() 

jpeg("tsla-cumsum.jpeg", width = 1100, height = 700) 
plot(cumsum(profit[,2]), type = "l", lwd = 8, col = "green", main = "TSLA")
dev.off() 

jpeg("amzn-cumsum.jpeg", width = 1100, height = 700) 
plot(cumsum(profit[,3]), type = "l", lwd = 8, col = "blue", main = "AMZN")
dev.off() 

jpeg("goog-cumsum.jpeg", width = 1100, height = 700) 
plot(cumsum(profit[,3]), type = "l", lwd = 8, col = "yellow", main = "GOOG")
dev.off() 

jpeg("nflx-cumsum.jpeg", width = 1100, height = 700) 
plot(cumsum(profit[,5]), type = "l", lwd = 8, col = "orange", main = "NFLX")
dev.off() 

jpeg("fb-cumsum.jpeg", width = 1100, height = 700) 
plot(cumsum(profit[,6]), type = "l", lwd = 8, col = "purple", main = "FB")
dev.off() 

# Asset Allocation Plot
jpeg("asset-alloc.jpeg", width = 1100, height = 700) 
piepercent<- round(100*sol/sum(sol), 1)
companies = c("AAPL", "TSLA", "AMZN", "GOOG", "NFLX", "FB")
pie(sol, labels = piepercent, col = rainbow(length(sol)), main = "Asset Allocation")
legend("topright", companies, cex = 0.8, fill = rainbow(length(sol)))
dev.off() 