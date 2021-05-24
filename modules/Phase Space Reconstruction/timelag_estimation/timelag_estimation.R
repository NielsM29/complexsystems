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