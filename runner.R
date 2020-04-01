install.packages(c("plotly", "ggplot2"))
library(plotly)

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

# plotting ####

# run one simulation
source("covid_slovakia/config.R")
source("harmans_code.R")

b0v = seq(60, 180, by = 10)
gammav = 1.06
tmaxv = 25:38
juu = corona_explore(b0v, gammav, tmaxv)

Vch = juu$Vch
Vpp = juu$Vpp
VNi = juu$VNi
Vs = juu$Vs


# old
image(1/(Vch[,1,]), main = paste("1/Err, gamma2 =", gammav[1]), x = b0v, y = tmaxv,
      breaks = c(0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.04),
      col = hcl.colors(6))
contour(1/(Vch[,1,]), main = "1/chyba", x = b0v, y = tmaxv, add = TRUE,
        levels = c(0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.04))

image(Vpp[,1,], main = "pp", x = b0v, y = tmaxv,
      breaks = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5),
      col = topo.colors(7))
contour(Vpp[,1,], main = "pp", x = b0v, y = tmaxv, add = TRUE)

image(VNi[,1,], main = "Ni", x = b0v, y = tmaxv)
contour(VNi[,1,], main = "Ni", x = b0v, y = tmaxv, add = TRUE)

image(Vs[,1,], main = "deaths", x = b0v, y = tmaxv,
      breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 1000),
      col = rev(gray.colors(10)))
contour(Vs[,1,], main = "deaths", x = b0v, y = tmaxv, add = TRUE)

# new

plot_ly(x = tmaxv, y = b0v, z = 1/(Vch[,1,]), type = "contour",
        contours = list(showlabels = TRUE)) %>% 
  colorbar(title = "1/Err") %>% 
  layout(title = paste("1/Err, gamma2 =", gammav[1]), 
         xaxis = list(title = "tmaxv"), 
         yaxis = list(title = "b0v"))

plot_ly(x = tmaxv, y = b0v, z = Vpp[,1,], type = "contour",
        contours = list(showlabels = TRUE),
        colorscale = "Greens") %>% 
  colorbar(title = "Percent") %>% 
  layout(title = paste("Percento pozitivnych testov, gamma2 =", gammav[1]), 
         xaxis = list(title = "tmaxv"), 
         yaxis = list(title = "b0v"))

plot_ly(x = tmaxv, y = b0v, z = VNi[,1,], type = "contour",
        contours = list(showlabels = TRUE),
        colorscale = "Blues") %>% 
  layout(title = paste("Počet infikovaných jedincov, gamma2 =", gammav[1]), 
         xaxis = list(title = "tmaxv"), 
         yaxis = list(title = "b0v"))

plot_ly(x = tmaxv, y = b0v, z = Vs[,1,], type = "contour",
        contours = list(showlabels = TRUE),
        colorscale = "Reds") %>% 
  layout(title = paste("Počet umrti, gamma2 =", gammav[1]), 
         xaxis = list(title = "tmaxv"), 
         yaxis = list(title = "b0v"))
