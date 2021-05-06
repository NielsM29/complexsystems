library(ggplot2)
library(nonlinearTseries)

lorenz <- lorenz(sigma = 10, beta = 8/3, rho = 28, start = c(-13, -14, 47), time = seq(0, 50, by = 0.01))

ggplot(data = data.frame(lorenz), aes(x = time, y = x)) + 
  geom_line() +
  xlab('Time') +
  ylab('X value')
