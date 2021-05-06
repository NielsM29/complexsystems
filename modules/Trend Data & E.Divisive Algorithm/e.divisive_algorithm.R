set.seed(29)

# Loading packages --------------------------------------------------------

library(ggplot2)
library(tseries)
library(ecp)

# Loading dataset ---------------------------------------------------------

#Loading dataset produced by NetLogo and selecting right timeframe
df_1 <- read.csv('wolf_sheep_grass_unstable.csv', skip = 23, col.names = c('', 'time', 'sheep', 'wolves', 'grass'))
df_1[,1] <- NULL
df_1 <- df_1[23000:28000,]

#Plotting the selected timeframe in the dataset
ggplot(data = df_1, aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = sheep, color = 'Sheep')) +
  geom_line(aes(y = wolves, color = 'Wolves')) +
  geom_line(aes(y = grass, color = 'Grass')) +
  xlab('Time') +
  ylab('Number') +
  ggtitle('Regime shift in predator-prey dataset')


# First Trial -------------------------------------------------------------

#Creation variables for change point analysis (CPA)
list <- c('sheep', 'wolves', 'grass')
e.out <- list()
df.e <- df_1[1,]
df.e[,1] <- NULL
df.e[1,] <- rep(NA, 3)

#Running CPA on 3 timeseries
for (k in 2:4){
  ts <- matrix(na.exclude(df_1[,k]))
  e.out[[k-1]] <- e.divisive(ts, R=500, sig.lvl = 0.05)
  df.e[,k-1] <- length(which(e.out[[k-1]]$p.values < 0.05))
}


#Plots
for(cl in (2:4)){
  plot(as.ts(df_1[,cl]), main=(colnames(df_1)[cl]), ylab = 'Value')
  abline(v=e.out[[cl-1]]$estimates,col="blue")  
}

#Change Point Estimates
for (i in 1:3){
  print(colnames(df_1)[i+1])
  print(length(e.out[[i]]$estimates))
}


# Second Trial ------------------------------------------------------------

#Creation variables for change point analysis (CPA)
list <- c('sheep', 'wolves', 'grass')
e.out <- list()
df.e <- df_1[1,]
df.e[,1] <- NULL
df.e[1,] <- rep(NA, 3)

#Running CPA on 3 timeseries
for (k in 2:4){
  ts <- matrix(na.exclude(df_1[,k]))
  e.out[[k-1]] <- e.divisive(ts, R=500, sig.lvl = 0.05, min.size = 180)
  df.e[,k-1] <- length(which(e.out[[k-1]]$p.values < 0.05))
}

#Plots
for(cl in (2:4)){
  plot(as.ts(df_1[,cl]), main=(colnames(df_1)[cl]), ylab = 'Value')
  abline(v=e.out[[cl - 1]]$estimates,col="blue")  
}

#Change Point Estimates
for (i in 1:3){
  print(colnames(df_1)[i + 1])
  print(e.out[[i]]$estimates)
}

# Detrending timeseries ---------------------------------------------------

# Trial Three -------------------------------------------------------------

df_2 <- data.frame(rep(NA, 4999))
df_2$detrend_sheep <- NA
df_2$detrend_wolves <- NA
df_2$detrend_grass <- NA
df_2[,1] <- NULL
colnames(df_2) <- c('Detrended Sheep Values', 'Detrended Wolves Values', 'Detrended Grass Values')

df_2$detrend_sheep <- diff(x = df_1$sheep, lag = 1, differences = 2)
df_2$detrend_wolves <- diff(x = df_1$wolves, lag = 1, differences = 2)
df_2$detrend_grass <- diff(x = df_1$grass, lag = 1, differences = 2)

ggplot(data = df_2, aes(x = 1:4999, y = `Detrended Sheep Values`)) +
  geom_line() +
  xlab('Time') +
  ylab('Value') +
  ggtitle('Detrended Sheep Plot')

ggplot(data = df_2, aes(x = 1:4999, y = `Detrended Wolves Values`)) +
  geom_line() +
  xlab('Time') +
  ylab('Value') +
  ggtitle('Detrended Wolves Plot')

ggplot(data = df_2, aes(x = 1:4999, y = `Detrended Grass Values`)) +
  geom_line() +
  xlab('Time') +
  ylab('Value') +
  ggtitle('Detrended Grass Plot')

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
