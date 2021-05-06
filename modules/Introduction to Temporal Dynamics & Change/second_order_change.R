library(ggplot2)

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
  print(temp[i])
}


ggplot(data = second_order_change, aes(x = time)) + 
  geom_line(aes(y = constant, color = 'Constant Oscillation')) +
  geom_line(aes(y = non_constant, color = 'Non-constant Oscillation')) +
  geom_line(aes(y = oscillating, color = 'Oscillating Oscillation')) +
  xlab('Time') +
  ylab('Value')

