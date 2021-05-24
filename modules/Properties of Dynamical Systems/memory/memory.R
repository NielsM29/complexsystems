# Memory ------------------------------------------------------------------

#Performing Bartels-Rank tests against non-randomness
sheep_bartels_rank <- bartels.rank.test(df$sheep, alternative = "two.sided")
wolves_bartels_rank <- bartels.rank.test(df$wolves, alternative = "two.sided")
grass_bartels_rank <- bartels.rank.test(df$grass, alternative = "two.sided")

# Calculate Partial Autocorrelation & length of timeseries
sheep_pacf <- pacf(df$sheep, lag.max = 900, main = 'Sheep Partial Autocorrelation Plot')
N <-length(df$sheep)  
# Number of values passing two-tailed Z-test treshold
sheep_memory_values <- length(which(abs(sheep_pacf$acf) > (2 / sqrt(N))))
# Max lag for long-term memory
sheep_max_lag <- max(which(abs(sheep_pacf$acf) > (2 / sqrt(N))))

# Show number of lags & maximum lag
sheep_memory_values
sheep_max_lag

#Repeat process
wolves_pacf <- pacf(df$wolves, lag.max = 900, main = 'Wolves Partial Autocorrelation Plot')
N <-length(df$wolves)  
wolves_memory_values <- length(which(abs(wolves_pacf$acf) > (2 / sqrt(N))))
wolves_max_lag <- max(which(abs(wolves_pacf$acf) > (2 / sqrt(N))))

wolves_memory_values
wolves_max_lag

grass_pacf <- pacf(df$grass, lag.max = 900, main = 'Grass Partial Autocorrelation Plot')
N <-length(df$grass)  
grass_memory_values <- length(which(abs(grass_pacf$acf) > (2 / sqrt(N))))
grass_max_lag <- max(which(abs(grass_pacf$acf) > (2 / sqrt(N))))

grass_memory_values
grass_max_lag