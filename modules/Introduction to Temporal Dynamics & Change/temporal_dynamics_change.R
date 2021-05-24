library(tidyverse)
library(nonlinearTseries)
library(zoo)
library(lattice)
library(tseries)

# Import wolf_sheep_grasspredation.csv
df <- read.csv("wolf_sheep_grasspredation.csv", header = 1, skip = 23, col.names = c('', 'time', 'sheep', 'wolves', 'grass'))
df[,1] <- NULL

ggplot(data = df, aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = sheep, color = 'Sheep')) +
  geom_line(aes(y = wolves, color = 'Wolves')) +
  geom_line(aes(y = grass, color = 'Grass')) +
  ggtitle('Sheep/Wolf/Grass Predator-prey Dataset') +
  xlab('Time') +
  ylab('Count')


# Types of change ---------------------------------------------------------

#First-order change
first_order_change <- data.frame('time' = seq(1, 100, 1), 
                                 'linear' = rep(5, 100),
                                 'constant' = seq(0.1, 10, 0.1),
                                 'exponential' = rep(NA, 100))

for (i in 1:100) {
  first_order_change$exponential[i] <- (0.001 * 1.1 ** i)
}

ggplot(data = first_order_change, aes(x = time)) + 
  geom_line(aes(y = linear, color = 'Linear Value')) + 
  geom_line(aes(y = constant, color = 'Constant Change')) +
  geom_line(aes(y = exponential, color = 'Exponential Change')) +
  xlab('Time') +
  ylab('Value') +
  ggtitle('First-order change')

#Second-order change
second_order_change <- data.frame('time' = seq(1, 100, 1),
                                  'constant' = rep(NA, 100), 
                                  'non-constant' = rep(NA, 100), 
                                  'oscillating' = rep(NA, 100))

for (i in 1:100) {
  second_order_change$constant[i] = sin(i/5) + 6
}


for (i in 1:100) {
  second_order_change$non_constant[i] = sin(i * (i/400)) + 3
}

temp <- c(seq(1, 50, 1), seq(50, 1, -1))

for (i in 1:100) {
  second_order_change$oscillating[i] = cos(temp[i] * temp[i] / 200)
}

ggplot(data = second_order_change, aes(x = time)) + 
  geom_line(aes(y = constant, color = 'Constant Oscillation')) +
  geom_line(aes(y = non_constant, color = 'Non-constant Oscillation')) +
  geom_line(aes(y = oscillating, color = 'Oscillating Oscillation')) +
  xlab('Time') +
  ylab('Value') +
  ggtitle('Second-order change')

#Third-order change
lorenz <- lorenz(sigma = 10, beta = 8/3, rho = 28, start = c(-13, -14, 47), time = seq(0, 50, by = 0.01))

ggplot(data = data.frame(lorenz), aes(x = time, y = x)) + 
  geom_line() +
  xlab('Time') +
  ylab('X value') +
  ggtitle('Third-order change')


# Stationarity tests ------------------------------------------------------

#Stationarity tests
sheep_dickey <- adf.test(x = df$sheep, alternative = 'stationary')
wolves_dickey <- adf.test(x = df$wolves, alternative = 'stationary')
grass_dickey <- adf.test(x = df$grass, alternative = 'stationary')

sheep_kpss <- kpss.test(df$sheep, null = 'Trend')
wolves_kpss <- kpss.test(df$wolves, null = 'Trend')
grass_kpss <- kpss.test(df$grass, null = 'Trend')


# Stability and measurement error -----------------------------------------

#Random walk plot
x <- data.frame('x' = ts(cumsum(rnorm(100, mean = 1, sd = 2)), start = 1, end = 100, frequency = 1), 'Time' = c(1:100))
ggplot(data = x, aes(x = Time, y = x)) + geom_line() +
  xlab('Time') +
  ylab('Value') + 
  ggtitle('Example random walk plot')

#Stability and measurement error
densityplot(df$sheep, main = 'Sheep stability', xlab = 'Number of sheep', ylab = 'Density')
densityplot(df$wolves, main = 'Wolves stability', xlab = 'Number of wolves', ylab = 'Density')
densityplot(df$grass, main = 'Grass stability', xlab = 'Amount of grass', ylab = 'Density')

#Measurement error Sheep
set.seed(2929)
a <- rnorm(2501, mean = 0, sd = 1)
b <- rnorm(2501, mean = 0, sd = 10)
c <- rnorm(2501, mean = 0, sd = 30)
d <- rnorm(2501, mean = 0, sd = 50)

plot(density(df$sheep + a), main = 'Measurement Error Sheep', xlab = 'Number of Sheep', col = '#E69F00')
lines(density(df$sheep + b), col = '#56B4E9')
lines(density(df$sheep + c), col = '#CC79A7')
lines(density(df$sheep + d), col = '#009E73')
legend(450, 0.0055, legend = c('SD = 1', 'SD = 10', 'SD = 30', 'SD = 50'), fill = c('#E69F00', '#56B4E9', '#CC79A7', '#009E73'))

#Measurement error Wolves
set.seed(2929)
a <- rnorm(2501, mean = 0, sd = 1)
b <- rnorm(2501, mean = 0, sd = 10)
c <- rnorm(2501, mean = 0, sd = 30)
d <- rnorm(2501, mean = 0, sd = 50)

plot(density(df$wolves + a), main = 'Measurement Error Wolves', xlab = 'Number of Wolves', col = '#E69F00')
lines(density(df$wolves + b), col = '#56B4E9')
lines(density(df$wolves + c), col = '#CC79A7')
lines(density(df$wolves + d), col = '#009E73')
legend(230, 0.0095, legend = c('SD = 1', 'SD = 10', 'SD = 30', 'SD = 50'), fill = c('#E69F00', '#56B4E9', '#CC79A7', '#009E73'))

#Measurement error Grass
set.seed(2929)
a <- rnorm(2501, mean = 0, sd = 1)
b <- rnorm(2501, mean = 0, sd = 10)
c <- rnorm(2501, mean = 0, sd = 30)
d <- rnorm(2501, mean = 0, sd = 50)

plot(density(df$grass + a), main = 'Measurement Error Grass', xlab = 'Amount of Grass', col = '#E69F00')
lines(density(df$grass + b), col = '#56B4E9')
lines(density(df$grass + c), col = '#CC79A7')
lines(density(df$grass + d), col = '#009E73')
legend(500, 0.0042, legend = c('SD = 1', 'SD = 10', 'SD = 30', 'SD = 50'), fill = c('#E69F00', '#56B4E9', '#CC79A7', '#009E73'))


# Smoothing/sampling ------------------------------------------------------

#Create smoothed dataset
df$deltasheep_1[25:2501] <- rollmean(df$sheep, 25)
df$deltawolves_1[25:2501] <- rollmean(df$wolves, 25)
df$deltagrass_1[25:2501] <- rollmean(df$grass, 25)

ggplot(data = df[25:2501,], aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = deltasheep_1, color = 'Sheep')) +
  geom_line(aes(y = deltawolves_1, color = 'Wolves')) +
  geom_line(aes(y = deltagrass_1, color = 'Grass')) +
  xlab('Time') +
  ylab('Count') +
  ggtitle('Rolling Mean Window = 25')

#Create overly smoothed dataset
df$deltasheep_2[200:2501] <- rollmean(df$sheep, 200)
df$deltawolves_2[200:2501] <- rollmean(df$wolves, 200)
df$deltagrass_2[200:2501] <- rollmean(df$grass, 200)

ggplot(data = df[200:2501,], aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = deltasheep_2, color = 'Sheep')) +
  geom_line(aes(y = deltawolves_2, color = 'Wolves')) +
  geom_line(aes(y = deltagrass_2, color = 'Grass')) +
  xlab('Time') +
  ylab('Count') +
  ggtitle('Rolling Mean Window = 200')

