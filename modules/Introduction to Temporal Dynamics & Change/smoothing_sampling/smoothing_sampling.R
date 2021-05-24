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