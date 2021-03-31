#Stationarity tests
sheep_dickey <- adf.test(x = df_sheep$sheep, alternative = 'stationary')
wolves_dickey <- adf.test(x = df_sheep$wolves, alternative = 'stationary')
grass_dickey <- adf.test(x = df_sheep$grass, alternative = 'stationary')

sheep_kpss <- kpss.test(df_sheep$sheep, null = 'Trend')
wolves_kpss <- kpss.test(df_sheep$wolves, null = 'Trend')
grass_kpss <- kpss.test(df_sheep$grass, null = 'Trend')
