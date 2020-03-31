# This is developers playground

# basic harman code
source("harmans_code.R")
start = Sys.time()
corona_explore(seq(60, 180, by = 10), 1.06, 25:38)
end = Sys.time()
print(end - start) # expensive simulations

# run one simulation
source("covid_slovakia/config.R")
source("harmans_code.R")

start = Sys.time()
corona_sim(c(150, 1.04, 31))
end = Sys.time()
print(end - start) # expensive simulations

# [1] "Matrix preparing"
# Time difference of 0.001328945 secs
# [1] "Simulating individual"
# Time difference of 0.03329396 secs
# [1] "simulate for every day"
# Time difference of 0.0004720688 secs
# [1] "ploting"
# Time difference of 0.04382801 secs

# simply delete plotting

# basic harman code
source("harmans_code.R")
start = Sys.time()
corona_explore(seq(60, 180, by = 10), 1.06, 25:38)
end = Sys.time()
print(end - start) # expensive simulations

# with plotting
# Time difference of 32.4301 secs
# without plotting
# Time difference of 28.99935 secs