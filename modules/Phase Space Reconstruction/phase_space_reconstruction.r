set.seed(29)

# Loading packages --------------------------------------------------------

#Required packages
library(ggplot2)
library(nonlinearTseries)
library(tseriesChaos)
library(scatterplot3d)
library(plotly)

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


# Estimating timelag ------------------------------------------------------

# Compute the ACF for max 100 lags for all three vars.
tau.acf_sheep <- timeLag(df$sheep, technique = "acf", lag.max = 50)
tau.acf_wolves <- timeLag(df$wolves, technique = "acf", lag.max = 50)
tau.acf_grass <- timeLag(df$grass, technique = "acf", lag.max = 50)

print(tau.acf_sheep) # tau-autocor = 35
print(tau.acf_wolves)  # tau-autocor = 34
print(tau.acf_grass) # tau-autocor = 36

# Compute the Average Mutual Information
tau.ami_sheep <- timeLag(df$sheep, technique = "ami", lag.max = 50)
tau.ami_wolves <- timeLag(df$wolves, technique = "ami", lag.max = 50)
tau.ami_grass <- timeLag(df$grass, technique = "ami", lag.max = 50)

print(tau.ami_sheep) # tau-ami = 14
print(tau.ami_wolves)  # tau-ami = 14
print(tau.ami_grass) # tau-ami = 14


# Estimating embedding dimensions -----------------------------------------

#False Nearest Neighbors (Kennel, et al., 1992)
# Compute and plot False Nearest Neighbors with AMI
fnn.out_sheep <- false.nearest(df$sheep, m = 7, d = tau.ami_sheep, rt = 10, t = 50, eps = sd(df$sheep)/10)
fnn.out_wolves <- false.nearest(df$wolves, m = 7, d = tau.ami_wolves, rt = 10, t = 50, eps = sd(df$wolves)/10)
fnn.out_grass <- false.nearest(df$grass, m = 7, d = tau.ami_grass, rt = 10, t = 50, eps = sd(df$grass)/10)

plot(fnn.out_sheep) # FNN = 3
plot(fnn.out_wolves)  # FNN = 3
plot(fnn.out_grass) # FNN = 3

# Compute and plot Cao's method with AMI
emb.dim_sheep <- estimateEmbeddingDim(df$sheep, time.lag = tau.ami_sheep, max.embedding.dim = 15)
emb.dim_wolves <- estimateEmbeddingDim(df$wolves, time.lag = tau.ami_wolves, max.embedding.dim = 15)
emb.dim_grass <- estimateEmbeddingDim(df$grass, time.lag = tau.ami_grass, max.embedding.dim = 15)

print(emb.dim_sheep)  #emb.dim_sheep = 8
print(emb.dim_wolves)   #emb.dim_wolf = 8
print(emb.dim_grass)  #emb.dim_grass = 6


# Building phase space reconstruction -------------------------------------

#Build reconstructed manifold of M dimensions
sheep.takens <- buildTakens(df$sheep, emb.dim_sheep, tau.ami_sheep)
wolves.takens <- buildTakens(df$wolves, emb.dim_wolves, tau.ami_wolves)
grass.takens <- buildTakens(df$grass, emb.dim_grass, tau.ami_grass)

#How does the manifold look like?
head(sheep.takens, 10)

#Simple 3D plot of the original data
orig_plot <- plot_ly(x = df$sheep, y = df$wolves,  z = df$grass, type = "scatter3d", mode = "lines", opacity = 1, line = list(width = 2))
orig_plot <- orig_plot %>% layout(
  title = "Original Signals in 3D",
  scene = list(
    xaxis = list(title = "Sheep"),
    yaxis = list(title = "Wolf"),
    zaxis = list(title = "Grass")
  )
)
orig_plot

scatterplot3d(df$sheep, df$wolves, df$grass, 
              grid = T, highlight.3d = T, pch = 20, angle = 65, 
              main = "Sheep vs. Wolves vs. Grass in 3D",
              xlab = "Sheep",
              ylab = "Wolves",
              zlab = 'Grass')


# Plots of reconstructed phase spaces -------------------------------------

#3D plots of Sheep vs. Wolf vs. Grass reconstructed
# Plotting Takens for all three variables. 
sheep_phase <- plot_ly(x = sheep.takens[,1], y = sheep.takens[,4],  z = sheep.takens[,8], type = "scatter3d", mode = "lines", opacity = 1, line = list(width = 2))
sheep_phase <- sheep_phase %>% layout(
  title = "Sheep in Phase Space",
  scene = list(
    xaxis = list(title = "Sheep[1]"),
    yaxis = list(title = "Sheep[4]"),
    zaxis = list(title = "Sheep[8]")
  )
)
sheep_phase

wolves_phase <- plot_ly(x = wolves.takens[,1], y = wolves.takens[,4],  z = wolves.takens[,8], type = "scatter3d", mode = "lines", opacity = 1, line = list(width = 2))
wolves_phase <- wolves_phase %>% layout(
  title = "Wolf in Phase Space",
  scene = list(
    xaxis = list(title = "Wolf[1]"),
    yaxis = list(title = "Wolf[4]"),
    zaxis = list(title = "Wolf[8]")
  )
)
wolves_phase 

grass_phase <- plot_ly(x = grass.takens[,1], y = grass.takens[,6],  z = grass.takens[,3], type = "scatter3d", mode = "lines", opacity = 1, line = list(width = 2))
grass_phase <- grass_phase %>% layout(
  title = "Grass in Phase Space",
  scene = list(
    xaxis = list(title = "Grass[1]"),
    yaxis = list(title = "Grass[6]"),
    zaxis = list(title = "Grass[3]")
  )
)
grass_phase 

#Simpler 3D plots
#Sheep
scatterplot3d(sheep.takens[,1], sheep.takens[,4], sheep.takens[,8], 
              grid = T, highlight.3d = T, pch = 20, angle = 65,
              main = "Sheep in 3D",
              xlab = 'Sheep[1]',
              ylab = 'Sheep[4]',
              zlab = 'Sheep[8]')
#Wolves
scatterplot3d(wolves.takens[,1], wolves.takens[,4], wolves.takens[,8], 
              grid = T, highlight.3d = T, pch = 20, angle = 65,
              main = "Wolves in 3D",
              xlab = 'Wolves[1]',
              ylab = 'Wolves[4]',
              zlab = 'Wolves[8]')
#Grass
scatterplot3d(grass.takens[,1], grass.takens[,3], grass.takens[,6], 
              grid = T, highlight.3d = T, pch = 20, angle = 65,
              main = "Grass in 3D",
              xlab = 'Grass[1]',
              ylab = 'Grass[3]',
              zlab = 'Grass[6]')
