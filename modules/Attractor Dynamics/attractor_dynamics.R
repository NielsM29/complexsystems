set.seed(29)


# Loading packages --------------------------------------------------------

library(dplyr)
library(ggplot2)
library(grid)
library(tiger)
library(pracma)
library(scales)
library(lmtest)


# Loading dataset ---------------------------------------------------------

# Import wolf_sheep_grasspredation.csv
df <- read.csv("wolf_sheep_grasspredation.csv", header = 1, skip = 23, col.names = c('', 'time', 'sheep', 'wolves', 'grass'))
df[,1] <- NULL

ggplot(data = df, aes(x = time)) + 
  scale_colour_hue() +
  geom_line(aes(y = sheep, color = 'Sheep')) +
  geom_line(aes(y = wolves, color = 'Wolves')) +
  geom_line(aes(y = grass, color = 'Grass')) +
  ggtitle('Sheep/Wolf/Grass Predator-prey Dataset') +
  xlab('Time') +
  ylab('Count')


# Topology plot - Full dataset --------------------------------------------

sheep_lag_full <- lag(df$sheep)
sheep_lead_full <- lead(df$sheep, 1)
sheep_change_full <- (sheep_lead_full - sheep_lag_full) / 2

wolves_lag_full <- lag(df$wolves)
wolves_lead_full <- lead(df$wolves, 1)
wolves_change_full <- (wolves_lead_full - wolves_lag_full) / 2

ggplot(data = df, aes(x = sheep, y = wolves)) + 
  geom_segment(aes(xend = sheep + sheep_change_full, yend = wolves + wolves_change_full),
               arrow = arrow(length = unit(.2,"cm"))) +
  stat_density2d(aes(colour = ..level..)) +
  scale_color_continuous(name = 'Density of values') +
  ggtitle('2D Topology Plot - Sheep vs. Wolves') +
  xlab('Sheep count') +
  ylab('Wolves count')


# Topology plot - Sampled dataset -----------------------------------------

sheep_sample <- df$sheep[seq(1, 2501, 20)]
wolves_sample <- df$wolves[seq(1, 2501, 20)]

sheep_lag_sample <- lag(sheep_sample)
sheep_lead_sample <- lead(sheep_sample, 1)
sheep_change_sample <- (sheep_lead_sample - sheep_lag_sample) / 6

wolves_lag_sample <- lag(wolves_sample)
wolves_lead_sample <- lead(wolves_sample, 1)
wolves_change_sample <- (wolves_lead_sample - wolves_lag_sample) / 6

df_2 <- as.data.frame(cbind(sheep_sample, sheep_lag_sample, sheep_lead_sample, sheep_change_sample, wolves_sample, wolves_lag_sample, wolves_lead_sample, wolves_change_sample))

ggplot(data = df_2, aes(x = sheep_sample, y = wolves_sample)) + 
  geom_segment(aes(xend = sheep_sample + sheep_change_sample, yend = wolves_sample + wolves_change_sample),
               arrow = arrow(length = unit(.2,"cm"))) +
  stat_density2d(aes(colour=..level..)) +
  scale_color_continuous(name = 'Density of values') +
  ggtitle('2D Topology Plot - Sampled Sheep vs. Wolves') +
  xlab('Sheep count') +
  ylab('Wolves count')


# Change as outcome regression - Limit cycle ------------------------------

#Calculate change variable
df$sheep_diff = df$sheep - lag(df$sheep)
df$wolves_diff = df$wolves - lag(df$wolves)

#Delete first row bc/ NA
df <- df[-1,]

#Run basic models
mod_sheepdiff <- lm(sheep_diff ~ sheep + wolves, data = df)
summary(mod_sheepdiff)

mod_wolvesdiff <- lm(wolves_diff ~ wolves + sheep, data = df)
summary(mod_wolvesdiff)


#Create phase portrait - code based on Perry et al. (2017)
x_range_basic <- seq(30, 500, 20)
y_range_basic <- seq(10, 250, 20)

temp_basic <- meshgrid(x_range_basic,y_range_basic)
j_basic <- mod_sheepdiff$coefficients[1] + mod_sheepdiff$coefficients[2]  * temp_basic$X + mod_sheepdiff$coefficients[3] * temp_basic$Y 
k_basic <- mod_wolvesdiff$coefficients[1] + mod_wolvesdiff$coefficients[2] * temp_basic$Y + mod_wolvesdiff$coefficients[3] * temp_basic$X 

#Plot phase portrait
plot(range(x_range_basic),range(y_range_basic),type="n",main = 'Change as outcome plot - Sheep vs. Wolves',xlab="Count sheep",ylab="Count wolves")
quiver(temp_basic$X,temp_basic$Y,j_basic,k_basic,scale=1, length=0.05,angle=1, col = 'blue')
points(df$sheep, df$wolves, pch = 20, col = alpha('black', alpha = 0.2))

#Improving model specification
plot(mod_sheepdiff, which = 1, main = 'Sheep basic')
resettest(mod_sheepdiff)
plot(mod_wolvesdiff, which = 1, main = 'Wolf basic')
resettest(mod_wolvesdiff)

#SHEEP
mod1_sheepdiff <- lm(sheep_diff ~ sheep + wolves + I(wolves^2), data = df)
summary(mod1_sheepdiff)
plot(mod1_sheepdiff, which = 1, main = 'Sheep opposite polynomial')
resettest(mod1_sheepdiff)

mod2_sheepdiff <- lm(sheep_diff ~ sheep + wolves + I(sheep^2) + I(wolves^2), data = df)
summary(mod2_sheepdiff)
plot(mod2_sheepdiff, which = 1, main = 'Sheep both polynomial')
resettest(mod2_sheepdiff)

mod3_sheepdiff <- lm(sheep_diff ~ sheep + wolves + I(wolves^2) + sheep * wolves, data = df)
summary(mod3_sheepdiff)
plot(mod3_sheepdiff, which = 1, main = 'Sheep interaction term')
resettest(mod3_sheepdiff)

#WOLVES
mod1_wolvesdiff <- lm(wolves_diff ~ wolves + sheep + I(sheep^2), data = df)
summary(mod1_wolvesdiff)
plot(mod1_wolvesdiff, which = 1, main = 'Wolves opposite polynomial')
resettest(mod1_wolvesdiff)

mod2_wolvesdiff <- lm(wolves_diff ~ wolves + sheep + I(sheep^2) + I(wolves^2), data = df)
summary(mod2_wolvesdiff)
plot(mod2_wolvesdiff, which = 1, main = 'Wolves both polynomial')
resettest(mod2_wolvesdiff)

mod3_wolvesdiff <- lm(wolves_diff ~ wolves + sheep + I(sheep^2) + wolves * sheep, data = df)
summary(mod3_wolvesdiff)
plot(mod3_wolvesdiff, which = 1, main = 'Wolves interaction term')
resettest(mod3_wolvesdiff)

#Create vectorfield - code based on Perry et al. (2017)
x_range <- seq(30, 460, 20)
y_range <- seq(10, 250, 20)

temp <- meshgrid(x_range,y_range)
j <- mod1_sheepdiff$coefficients[1] + mod1_sheepdiff$coefficients[2]  * temp$X + mod1_sheepdiff$coefficients[3] * temp$Y + mod1_sheepdiff$coefficients[4] * temp$Y
k <- mod3_wolvesdiff$coefficients[1] + mod3_wolvesdiff$coefficients[2] * temp$Y + mod3_wolvesdiff$coefficients[3] * temp$X + mod3_wolvesdiff$coefficients[4] * temp$X + mod3_wolvesdiff$coefficients[5] * temp$X * temp$Y

#Plot vectorfield
plot(range(x_range),range(y_range),type="n",main = 'Change as outcome plot - Sheep vs. Wolves - Best',xlab="Count sheep",ylab="Count wolves")
quiver(temp$X,temp$Y,j,k,scale=1, length=0.05,angle=1, col = 'blue')
points(df$sheep, df$wolves, pch = 20, col = alpha('black', alpha = 0.2))

