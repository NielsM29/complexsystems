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