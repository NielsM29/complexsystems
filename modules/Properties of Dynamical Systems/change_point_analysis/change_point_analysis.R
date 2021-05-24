# Change Point Analysis (CPA) ---------------------------------------------

#Creation variables for change point analysis (CPA)
list <- c('sheep', 'wolves', 'grass')
e.out <- list()
df.e <- df_2[1,]
df.e[1,] <- rep(NA, 3)

#Running CPA on 3 timeseries
for (k in 1:3){
  ts <- matrix(na.exclude(df_2[,k]))
  e.out[[k]] <- e.divisive(ts, R=500, sig.lvl = 0.05)
  df.e[,k] <- length(which(e.out[[k]]$p.values < 0.05))
}

#Plots
for(cl in (1:length(e.out))){
  plot(as.ts(df_2[,cl]), main=(colnames(df_2)[cl]), ylab = 'Value')
  abline(v=e.out[[cl]]$estimates,col="blue")  
}

#Change Point Estimates
for (i in 1:3){
  print(colnames(df_2)[i])
  print(e.out[[i]]$estimates)
}