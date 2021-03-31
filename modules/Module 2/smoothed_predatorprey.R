#Create smoothed dataset
df_sheep$deltasheep_1[25:2501] <- rollmean(df_sheep$sheep, 25)
df_sheep$deltawolves_1[25:2501] <- rollmean(df_sheep$wolves, 25)
df_sheep$deltagrass_1[25:2501] <- rollmean(df_sheep$grass, 25)

ggplot(data = df_sheep[25:2501,], aes(x = time)) +
  scale_colour_hue() +
  geom_line(aes(y = deltasheep_1, color = 'Sheep')) +
  geom_line(aes(y = deltawolves_1, color = 'Wolves')) +
  geom_line(aes(y = deltagrass_1, color = 'Grass')) +
  xlab('Time') +
  ylab('Average Predator Prey') +
  ggtitle('Rolling Mean Window = 25')
