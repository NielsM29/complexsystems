set.seed(29)

# Loading packages --------------------------------------------------------

library(ggplot2)
library(randtests)
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


# MEMORY ------------------------------------------------------------------


# Bartels_Rank Tests ------------------------------------------------------

#Performing Bartels-Rank tests against non-randomness
sheep_bartels_rank <- bartels.rank.test(df_1$sheep, alternative = "two.sided")
wolves_bartels_rank <- bartels.rank.test(df_1$wolves, alternative = "two.sided")
grass_bartels_rank <- bartels.rank.test(df_1$grass, alternative = "two.sided")

#Show outcome of tests
sheep_bartels_rank
wolves_bartels_rank
grass_bartels_rank

# Autocorrelation Function ------------------------------------------------

# Calculate Partial Autocorrelation & length of timeseries
sheep_pacf <- pacf(df_1$sheep, lag.max = 900, main = 'Sheep Partial Autocorrelation Plot')
N <-length(df_1$sheep)  
# Number of values passing two-tailed Z-test treshold
sheep_memory_values <- length(which(abs(sheep_pacf$acf) > (2 / sqrt(N))))
# Max lag for long-term memory
sheep_max_lag <- max(which(abs(sheep_pacf$acf) > (2 / sqrt(N))))

# Show number of lags & maximum lag
sheep_memory_values
sheep_max_lag

#Repeat process

wolves_pacf <- pacf(df_1$wolves, lag.max = 900, main = 'Wolves Partial Autocorrelation Plot')
N <-length(df_1$wolves)  
wolves_memory_values <- length(which(abs(wolves_pacf$acf) > (2 / sqrt(N))))
wolves_max_lag <- max(which(abs(wolves_pacf$acf) > (2 / sqrt(N))))

wolves_memory_values
wolves_max_lag

grass_pacf <- pacf(df_1$grass, lag.max = 900, main = 'Grass Partial Autocorrelation Plot')
N <-length(df_1$grass)  
grass_memory_values <- length(which(abs(grass_pacf$acf) > (2 / sqrt(N))))
grass_max_lag <- max(which(abs(grass_pacf$acf) > (2 / sqrt(N))))

grass_memory_values
grass_max_lag


# REGIME SHIFTS -----------------------------------------------------------



# Performing KPSS ---------------------------------------------------------

#KPSS test for trend stationarity -> shows non-stationarity
sheep_kpss <- kpss.test(df_1$sheep, null = 'Trend')
wolves_kpss <- kpss.test(df_1$wolves, null = 'Trend')
grass_kpss <- kpss.test(df_1$grass, null = 'Trend')

sheep_kpss
wolves_kpss
grass_kpss


# Detrending timeseries ---------------------------------------------------

df_2 <- data.frame(rep(NA, 4999))
df_2$detrend_sheep <- NA
df_2$detrend_wolves <- NA
df_2$detrend_grass <- NA
df_2[,1] <- NULL

df_2$detrend_sheep <- diff(x = df_1$sheep, lag = 1, differences = 2)
df_2$detrend_wolves <- diff(x = df_1$wolves, lag = 1, differences = 2)
df_2$detrend_grass <- diff(x = df_1$grass, lag = 1, differences = 2)
colnames(df_2) <- c('Detrended Sheep Values', 'Detrended Wolves Values', 'Detrended Grass Values')

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
