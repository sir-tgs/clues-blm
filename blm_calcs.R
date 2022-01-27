# Bad Luck Mitigation Calculations

luck = 1

# Master Clues

# Rare table rates with blm
r_m = 100/(1400 - 14*luck - 25*(0:3))

# Markov chain transition matrix
t_m = matrix(0, nrow = 4, ncol = 4)
for(i in 1:4){
  t_m[i, min(i+1, ncol(t_m))] = (1 - r_m[i])^6
  t_m[i, 1] = 1 - (1 - r_m[i])^6
}
  
# t_m
#           [,1]      [,2]      [,3]      [,4]
# [1,] 0.3619328 0.6380672 0.0000000 0.0000000
# [2,] 0.3673817 0.0000000 0.6326183 0.0000000
# [3,] 0.3729935 0.0000000 0.0000000 0.6270065
# [4,] 0.3787753 0.0000000 0.0000000 0.6212247

# Finding the stationary distribution
# Methodology: https://stephens999.github.io/fiveMinuteStats/stationary_distribution.html

# Function to compute the stationary distribution
stationary <- function(transition) {
  p <- diag(nrow(transition)) - transition
  A <- rbind(t(p),
             rep(1, ncol(transition)))
  b <- c(rep(0, nrow(transition)),
         1)
  qr.solve(A, b)
}

stat_m = stationary(t_m)
stat_m
#  0.3690162 0.2354571 0.1489545 0.2465722

# Per-slot rate of rolling on the rare table
perslot_m = sum(stat_m * r_m)
format(perslot_m, digits = 10)
# 0.07388208365

perslot_m/r_m[1]
# 1.024006
# BLM increases rare rolls for Masters by about 2.40%


# Elite Clues

# The difference between masters and elites is in the transition probabilites due to Elites not being a static 6 rolls per casket

r_e = 100/(1400 - 14*luck - 25*(0:3))
t_e = matrix(0, nrow = 4, ncol = 4)
for(i in 1:4){
  t_e[i, min(i+1, ncol(t_e))] = mean((1 - r_e[i])^(4:6))
  t_e[i, 1] = 1 - mean((1 - r_e[i])^(4:6))
}

# t_e
#           [,1]      [,2]      [,3]      [,4]
# [1,] 0.3110303 0.6889697 0.0000000 0.0000000
# [2,] 0.3158875 0.0000000 0.6841125 0.0000000
# [3,] 0.3208967 0.0000000 0.0000000 0.6791033
# [4,] 0.3260648 0.0000000 0.0000000 0.6739352

stat_e = stationary(t_e)
stat_e
# 0.3182728 0.2192803 0.1500124 0.3124345

# Per-slot rate of rolling on the rare table
perslot_e = sum(stat_e * r_e)
format(perslot_e, digits = 10)
# 0.07413535293

perslot_e/r_e[1]
# 1.027516
# BLM increases rare rolls for Elites by about 2.75%


# Hard clues

r_h = 100/(1592 - 16*luck - 0:100)
t_h = matrix(0, nrow = 101, ncol = 101)
for(i in 1:101){
  t_h[i, min(i+1, ncol(t_h))] = mean((1-r_h[i])^(4:6))
  t_h[i, 1] = 1 - mean((1-r_h[i])^(4:6))
}

stat_h = stationary(t_h)
perslot_h = sum(stat_h * r_h)
format(perslot_h, digits = 10)
# 0.06355610704

perslot_h / r_h[1]
# 1.001644

# BLM increases rare rolls for Hards by about 0.16%
