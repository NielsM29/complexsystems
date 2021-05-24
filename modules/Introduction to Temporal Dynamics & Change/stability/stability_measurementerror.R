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