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