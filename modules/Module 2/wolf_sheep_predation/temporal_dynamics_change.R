library(tidyverse)
library(nonlinearTseries)
library(zoo)
library(lattice)
library(tseries)

##Sheep dataframe from netlogo
df_sheep <- read.csv(file = 'wolf_sheep_grasspredation.csv', skip = 22, col.names = c('', 'time', 'sheep', 'wolves', 'grass'))
df_sheep[1] <- NULL

#Plot normal timeseries 
ggplot(data = df_sheep, aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = sheep, color = 'Sheep')) +
  geom_line(aes(y = wolves, color = 'Wolves')) +
  geom_line(aes(y = grass, color = 'Grass'))

#Stationarity tests
sheep_dickey <- adf.test(x = df_sheep$sheep, alternative = 'stationary')
wolves_dickey <- adf.test(x = df_sheep$wolves, alternative = 'stationary')
grass_dickey <- adf.test(x = df_sheep$grass, alternative = 'stationary')

sheep_kpss <- kpss.test(df_sheep$sheep, null = 'Trend')
wolves_kpss <- kpss.test(df_sheep$wolves, null = 'Trend')
grass_kpss <- kpss.test(df_sheep$grass, null = 'Trend')

#Random walk plot
x <- data.frame('x' = ts(cumsum(rnorm(100, mean = 1, sd = 2)), start = 1, end = 100, frequency = 1), 'Time' = c(1:100))
ggplot(data = x, aes(x = Time, y = x)) + geom_line()

#Stability and measurement error
densityplot(df_sheep$sheep, main = 'Sheep stability', xlab = 'Number of sheep', ylab = 'Density')
densityplot(df_sheep$wolves, main = 'Wolves stability', xlab = 'Number of wolves', ylab = 'Density')
densityplot(df_sheep$grass, main = 'Grass stability', xlab = 'Amount of grass', ylab = 'Density')

#Measurement error Sheep
set.seed(2929)
a <- rnorm(2501, mean = 0, sd = 1)
b <- rnorm(2501, mean = 0, sd = 10)
c <- rnorm(2501, mean = 0, sd = 30)
d <- rnorm(2501, mean = 0, sd = 50)

plot(density(df_sheep$sheep + a), main = 'Measurement Error Sheep', xlab = 'Number of Sheep', col = '#E69F00')
lines(density(df_sheep$sheep + b), col = '#56B4E9')
lines(density(df_sheep$sheep + c), col = '#CC79A7')
lines(density(df_sheep$sheep + d), col = '#009E73')
legend(450, 0.0055, legend = c('SD = 1', 'SD = 10', 'SD = 30', 'SD = 50'), fill = c('#E69F00', '#56B4E9', '#CC79A7', '#009E73'))

#Measurement error Wolves
set.seed(2929)
a <- rnorm(2501, mean = 0, sd = 1)
b <- rnorm(2501, mean = 0, sd = 10)
c <- rnorm(2501, mean = 0, sd = 30)
d <- rnorm(2501, mean = 0, sd = 50)

plot(density(df_sheep$wolves + a), main = 'Measurement Error Wolves', xlab = 'Number of Wolves', col = '#E69F00')
lines(density(df_sheep$wolves + b), col = '#56B4E9')
lines(density(df_sheep$wolves + c), col = '#CC79A7')
lines(density(df_sheep$wolves + d), col = '#009E73')
legend(230, 0.0095, legend = c('SD = 1', 'SD = 10', 'SD = 30', 'SD = 50'), fill = c('#E69F00', '#56B4E9', '#CC79A7', '#009E73'))

#Measurement error Grass
set.seed(2929)
a <- rnorm(2501, mean = 0, sd = 1)
b <- rnorm(2501, mean = 0, sd = 10)
c <- rnorm(2501, mean = 0, sd = 30)
d <- rnorm(2501, mean = 0, sd = 50)

plot(density(df_sheep$grass + a), main = 'Measurement Error Grass', xlab = 'Amount of Grass', col = '#E69F00')
lines(density(df_sheep$grass + b), col = '#56B4E9')
lines(density(df_sheep$grass + c), col = '#CC79A7')
lines(density(df_sheep$grass + d), col = '#009E73')
legend(500, 0.0042, legend = c('SD = 1', 'SD = 10', 'SD = 30', 'SD = 50'), fill = c('#E69F00', '#56B4E9', '#CC79A7', '#009E73'))

#Create smoothed dataset
df_sheep$deltasheep_1[25:2501] <- rollmean(df_sheep$sheep, 25)
df_sheep$deltawolves_1[25:2501] <- rollmean(df_sheep$wolves, 25)
df_sheep$deltagrass_1[25:2501] <- rollmean(df_sheep$grass, 25)

ggplot(data = df_sheep[25:2501,], aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = deltasheep_1, color = 'Sheep')) +
  geom_line(aes(y = deltawolves_1, color = 'Wolves')) +
  geom_line(aes(y = deltagrass_1, color = 'Grass')) +
  xlab('Time') +
  ylab('Average Predator Prey') +
  ggtitle('Rolling Mean Window = 25')

#Create overly smoothed dataset
df_sheep$deltasheep_2[200:2501] <- rollmean(df_sheep$sheep, 200)
df_sheep$deltawolves_2[200:2501] <- rollmean(df_sheep$wolves, 200)
df_sheep$deltagrass_2[200:2501] <- rollmean(df_sheep$grass, 200)

ggplot(data = df_sheep[200:2501,], aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = deltasheep_2, color = 'Sheep')) +
  geom_line(aes(y = deltawolves_2, color = 'Wolves')) +
  geom_line(aes(y = deltagrass_2, color = 'Grass')) +
  xlab('Time') +
  ylab('Average Predator Prey') +
  ggtitle('Rolling Mean Window = 200')
