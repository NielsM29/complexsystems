library(ggplot2)

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
  ylab('Value')
                                                      