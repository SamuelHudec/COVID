# This is developers playground

# basic harman code
source("harmans_code.R")
start = Sys.time()
corona_explore(seq(60, 180, by = 10), 1.06, 25:38)
end = Sys.time()
print(end - start)