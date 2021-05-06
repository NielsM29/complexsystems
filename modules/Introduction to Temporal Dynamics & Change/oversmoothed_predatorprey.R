#Create overly smoothed dataset
df_sheep$deltasheep_2[200:2501] <- rollmean(df_sheep$sheep, 200)
df_sheep$deltawolves_2[200:2501] <- rollmean(df_sheep$wolves, 200)
df_sheep$deltagrass_2[200:2501] <- rollmean(df_sheep$grass, 200)

ggplot(data = df_sheep[200:2501,], aes(x = time)) +
  scale_colour_hue() +
  geom_line(aes(y = deltasheep_2, color = 'Sheep')) +
  geom_line(aes(y = deltawolves_2, color = 'Wolves')) +
  geom_line(aes(y = deltagrass_2, color = 'Grass')) +
  xlab('Time') +
  ylab('Average Predator Prey') +
  ggtitle('Rolling Mean Window = 200')
