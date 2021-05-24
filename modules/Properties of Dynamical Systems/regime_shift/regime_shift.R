# Regime shift ------------------------------------------------------------

#KPSS test for trend stationarity -> shows non-stationarity
sheep_kpss <- kpss.test(df$sheep, null = 'Trend')
wolves_kpss <- kpss.test(df$wolves, null = 'Trend')
grass_kpss <- kpss.test(df$grass, null = 'Trend')

#Detrending time-series
df_2 <- data.frame(rep(NA, 4999))
df_2$detrend_sheep <- NA
df_2$detrend_wolves <- NA
df_2$detrend_grass <- NA
df_2[,1] <- NULL

df_2$detrend_sheep <- diff(x = df$sheep, lag = 1, differences = 2)
df_2$detrend_wolves <- diff(x = df$wolves, lag = 1, differences = 2)
df_2$detrend_grass <- diff(x = df$grass, lag = 1, differences = 2)
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