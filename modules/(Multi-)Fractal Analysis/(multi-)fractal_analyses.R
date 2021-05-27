set.seed(29)

# Loading packages --------------------------------------------------------

#Required packages
library(nonlinearTseries)
library(ifultools)
library(zoo)
library(MFDFA)
library(ggplot2)
library(ggpubr)


# Loading datasets --------------------------------------------------------

#Import wolf_sheep_grasspredation.csv
df_1 <- read.csv("wolf_sheep_grasspredation.csv", header = 1, skip = 23, col.names = c('', 'time', 'sheep', 'wolves', 'grass'))
df_1[,1] <- NULL

ggplot(data = df_1, aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = sheep, color = 'Sheep')) +
  geom_line(aes(y = wolves, color = 'Wolves')) +
  geom_line(aes(y = grass, color = 'Grass')) +
  ggtitle('Sheep/Wolf/Grass Predator-prey Dataset') +
  xlab('Time') +
  ylab('Count')

#Import Binance_BTCUSDT_1h
df_2 <- read.csv("Binance_BTCUSDT_1h.csv", skip = 2, col.names = c("unix", "time", "symbol","open", "high", "low", "close", "Volume BTC", "Volume USDT", "tradecount"))
df_2 <- df_2[0:3000,]
df_2[,c(1,3:6,8:10)] <- NULL
df_2$time <- seq(1, 3000)

ggplot(data = df_2, aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = close, color = 'Closing price')) +
  ggtitle('Bitcoin hourly closing price') +
  xlab('Time') +
  ylab('Closing price')


# Mean centering ----------------------------------------------------------

#Mean center
df_1$wolves_centered <- scale(df_1$wolves, center = TRUE, scale = FALSE)
df_1$grass_centered <- scale(df_1$grass, center = TRUE, scale = FALSE)
df_1$sheep_centered <- scale(df_1$sheep, center = TRUE, scale = FALSE)

#Interpolate missing values
df_1$wolves_centered <- na.approx(df_1$wolves_centered)
df_1$grass_centered <- na.approx(df_1$grass_centered)
df_1$sheep_centered <- na.approx(df_1$sheep_centered)

#Plot full time-series
A <- ggplot(data = df_1[0:2000,], aes(x = time)) + 
            scale_colour_hue() +
            geom_line(aes(y = wolves_centered, color = 'Wolves')) +
            ggtitle('Mean-centered "Wolves" variable (full-scale)') +
            xlab('Time') +
            ylab('Value')

#Plot full time-series on a scale of 200 time-steps
B <- ggplot(data = df_1[300:500,], aes(x = time)) + 
            scale_colour_hue() +
            geom_line(aes(y = wolves_centered, color = 'Wolves')) +
            ggtitle('Mean-centered "Wolves" variable (200 time-steps scale)') +
            xlab('Time') +
            ylab('Value')

#Plot full on second scale of 20 time-steps
C <- ggplot(data = df_1[20:40,], aes(x = time)) + 
            scale_colour_hue() +
            geom_line(aes(y = wolves_centered, color = 'Wolves')) +
            ggtitle('Mean-centered "Wolves" variable (20 time-steps scale)') +
            xlab('Time') +
            ylab('Value')

#Show self-similarity at different scales
ggarrange(A, B, C,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)


# DFA analysis ------------------------------------------------------------

#Set parameters for DFA analysis
scale_min = 16
scale_max = length(df_1$wolves_centered) / 10 #Hu et al (2001)
scale_num = length(logScale(scale.min = scale_min, scale.max = scale_max, scale.ratio = 1.25))

#DFA wolves
wolves_dfa = dfa(time.series = df_1$wolves_centered, npoints = scale_num, 
                 window.size.range = c(scale_min,scale_max), do.plot = FALSE)
wolves_estimation = estimate(wolves_dfa, do.plot = TRUE, main = 'DFA - Wolves time-series')

#DFA sheep
sheep_dfa = dfa(time.series = df_1$sheep_centered, npoints = scale_num, 
                window.size.range = c(scale_min,scale_max), do.plot = FALSE)
sheep_estimation = estimate(sheep_dfa, do.plot = TRUE, main = 'DFA - Sheep time-series')

#DFA Grass
grass_dfa = dfa(time.series = df_1$grass_centered, npoints = scale_num, 
                window.size.range = c(scale_min,scale_max), do.plot = FALSE)
grass_estimation = estimate(grass_dfa, do.plot = TRUE, main = 'DFA - Grass time-series')


# Surrogate check ---------------------------------------------------------

#Generate a randomized version of 'wolves' time-series
df_1$wolves_surrogate <- sample(df_1$wolves_centered, replace=FALSE)

ggplot(data = df_1, aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = wolves_surrogate, color = 'Wolves')) +
  ggtitle('Randomised mean-centered wolves time-series') +
  xlab('Time') +
  ylab('Value')


#Run DFA on randomised time-series
wolves_surrogate_dfa = dfa(time.series = df_1$wolves_surrogate, npoints = scale_num, 
                           window.size.range = c(scale_min, scale_max), do.plot = FALSE)
wolves_surrogaate_estimation = estimate(wolves_surrogate_dfa, do.plot = TRUE, main = 'DFA - Randomised wolves time-series')


# MF-DFA analysis ---------------------------------------------------------

#Mean Center
df_2$close_centered <- scale(df_2$close, center = TRUE, scale = FALSE)

#Interpolate missing values
df_2$close_centered <- na.approx(df_2$close_centered)

#Plot full time-series
A.2 <- ggplot(data = df_2[0:3000,], aes(x = time)) + 
              scale_colour_hue() +
              geom_line(aes(y = close_centered, color = 'Close')) +
              ggtitle('Mean-centered "Close" time-series (full-scale)') +
              xlab('Time') +
              ylab('Value')

#Plot full time-series on a scale of 300 time-steps
B.2 <- ggplot(data = df_2[300:600,], aes(x = time)) + 
              scale_colour_hue() +
              geom_line(aes(y = close_centered, color = 'Close')) +
              ggtitle('Mean-centered "Close" variable (300 time-steps scale)') +
              xlab('Time') +
              ylab('Value')

#Plot full on second scale of 30 time-steps
C.2 <- ggplot(data = df_2[370:400,], aes(x = time)) + 
              scale_colour_hue() +
              geom_line(aes(y = close_centered, color = 'Close')) +
              ggtitle('Mean-centered "Close" variable (30 time-steps scale)') +
              xlab('Time') +
              ylab('Value')

#Show self-similarity at different scales
ggarrange(A.2, B.2, C.2,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)

#Setting parameters for MF-DFA
scale_min_2 <- 16
scale_max_2 <- length(df_2$close_centered) / 10
scale_2 <- logScale(scale.min = scale_min_2, scale.max = scale_max_2, scale.ratio = 1.25)

#Range of q order exponents
q <- -10:10

#Run the MF-DFA
mfdfa_btc <- MFDFA(df_2$close_centered, scale = as.integer(scale_2), q = q, m = 1)

#Create a surrogate version and run MFDFA
mfdfa_btc_surrogate <- MFDFA(sample(df_2$close_centered, rep=FALSE), scale = as.integer(scale_2), q = q, m = 1)

#Compare surr data vs bitcoin data
matplot(mfdfa_btc$line, type = 'l', pch = 19, add = FALSE,
        ylim = c(8,18),
        xlab = "log Time Scale", 
        ylab="log F(q)", 
        main = "MF - DFA BTC hourly closing")

matplot(mfdfa_btc_surrogate$line, type = 'l', pch = 19, add = FALSE, 
        ylim = c(8,18), 
        xlab="log Time Scale", 
        ylab="log F(q)", 
        main = "BTC hourly closing surrogate")

#Calculate the width of the spectrum and compare it the previous estimates 
normal_width = max(mfdfa_btc$spec$hq) - min(mfdfa_btc$spec$hq)
surrogate_width = max(mfdfa_btc_surrogate$spec$hq) - min(mfdfa_btc_surrogate$spec$hq)
normal_width
surrogate_width
