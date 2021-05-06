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
