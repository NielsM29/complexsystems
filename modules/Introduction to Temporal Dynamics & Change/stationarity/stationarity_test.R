# Stationarity tests ------------------------------------------------------

#Stationarity tests
sheep_dickey <- adf.test(x = df$sheep, alternative = 'stationary')
wolves_dickey <- adf.test(x = df$wolves, alternative = 'stationary')
grass_dickey <- adf.test(x = df$grass, alternative = 'stationary')

sheep_kpss <- kpss.test(df$sheep, null = 'Trend')
wolves_kpss <- kpss.test(df$wolves, null = 'Trend')
grass_kpss <- kpss.test(df$grass, null = 'Trend')