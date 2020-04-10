install.packages(c("plotly", "ggplot2"))
library(plotly)
library(tidyverse)

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

# plotting best fit plot -> plotly ####

source("covid_slovakia/config.R")
source("harmans_code.R")

b0 = 150
gamma = 1.04
tmax = 31
juu = corona_sim(c(b0, gamma, tmax))

Zt = juu$Zt
Ct = juu$Ct
Ztv = juu$Ztv
Ctv = juu$Ctv
chyba = juu$chyba

# old
mxcum <- max(c(sum(Zt), sum(Ct)))
plot(cumsum(Zt), type = "b", pch = 19, ylim = c(0, mxcum),
     main = paste(b0, "|", gamma, "|", tmax, "|",
                  "|", sum(Zt), "|", round(chyba, 2)))
points(cumsum(Ct), pch = 19, type = "b", col = "red")

mx <- max(c(Zt, Ct))
lines(Zt[tmax:1]/mx*mxcum, type = "b", lty = "dotted")
lines(Ct[tmax:1]/mx*mxcum, type = "b", lty = "dotted", col = "red")

# new
df = data.frame(days = 1:length(Zt), actual = cumsum(Ct), fit = cumsum(Zt),
             Zt = Zt, Ct = Ct)
ggplotly(
df %>% select(days, actual, fit) %>% 
  gather("legend", "value", -days) %>%
  ggplot(aes(x = days, y = value, col = legend)) +
  theme_minimal() +
  labs(y = "Počet infikovaných cumulatívne",
       x = "dni") +
  geom_point() + 
  geom_line()
)

df %>% select(days, Ct, Zt) %>% 
  gather("legend", "value", -days) %>%
  ggplot(aes(x = days, y = value, col = legend)) +
  theme_minimal() +
  labs(y = "Počet infikovaných denne",
       x = "dni") +
  geom_point() + 
  geom_line()





#
# plotting image -> plotly ####

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

cor = which(1/(Vch[,1,]) == max(1/(Vch[,1,])), arr.ind = TRUE)
# b0
cor[1]
# tmax
cor[2]

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

# text output ####

read_lines('covid_slovakia/config.R') -> ju
juu = numeric(length(ju))
for(i in 1:length(ju)){
  juu[i] = paste(ju[i], "\n")
}




# corona 2 ####
source("harmans_code_2.R")
jup = corona_sim2(9, c(080, 1.25, 11), c(080, 0.95, Inf), 5, pic = T)
I1 = jup$I1
I = jup$I 
tmax = jup$tmax
Ct = jup$Ct


## old
# a) Priebeh odhalenych pripadov: skutocnych a pre najlepsi fit
rad1 <- hist(I1[, tmax + 5], 
             breaks = 0:(tmax + 1) - 0.5, 
             plot = FALSE)$counts[2:(tmax + 1)]
rad1c <- cumsum(rad1)
Ctc <- cumsum(Ct)
mx <- max(c(rad1c[tmax], Ctc[tmax]))

plot(Ctc, type = "b", pch = 19, ylim = c(0, mx), col = "red",
     main = "Kum. pozitivne testy", ylab = "pocet", xlab = "cas")
grid(col = "black")
lines(rad1c, pch = 1, type = "b", col = "red")

## new

df = data.frame(days = 1:length(rad1), actual = Ctc, fit = rad1c)

ggplotly(
  df %>% select(days, actual, fit) %>% 
    gather("legend", "value", -days) %>%
    ggplot(aes(x = days, y = value, col = legend)) +
    theme_minimal() +
    labs(y = "Počet pozitivnych testov",
         x = "dni") +
    geom_point() + 
    geom_line()
)

## old
# b) Simulovany kumulatívny priebeh skutocneho poctu nakazenych
I1.boliI <- matrix(0, nrow = nrow(I1), ncol = tmax)
for (i in 1:nrow(I1)) I1.boliI[i, ] <- sign(cumsum(abs(I1[i, 1:tmax])))
rad1 <- apply(I1.boliI, 2, sum)

# Toto je odhad pre kpsi posunuty o den
plot(0:(tmax - 1), rad1, pch = 1, col = "red",
     main = "Kum. symp. infikovani", type = "b",
     ylab = "pocet", xlab = "cas")
grid(col = "black")

## new

df = data.frame(days = 0:(tmax - 1), inf = rad1)

ggplotly(
df %>% ggplot(aes(x = days, y = inf)) + 
  theme_minimal() +
  labs(y = "Počet symp. infikovaných",
       x = "dni") +
  geom_point(alpha = 0.6) + 
  geom_line()
)




## old
# c) Simulovane aktualne pocty ludi s intenzitami nad 3 prahy: 0, 0.25, 0.5
# Nad prahom 0 su vsetci symptomaticky infikovani, nad 0.25 vazni, nad 0.5 mrtvi
I1.su51 <- I1.su26 <- I1.su01 <- matrix(0, nrow = nrow(I1), ncol = tmax)
for (i in 1:nrow(I1)) {
  I1.su01[i, ] <- sign(as.integer(I1[i, 1:tmax] != 0))
  I1.su26[i, ] <- sign(as.integer(abs(I1[i, 1:tmax]) > 0.25))
  I1.su51[i, ] <- sign(as.integer(abs(I1[i, 1:tmax]) > 0.51))
}
rad1 <- apply(I1.su01, 2, sum)
rad2 <- apply(I1.su26, 2, sum)
rad3 <- apply(I1.su51, 2, sum)

plot(rad1, pch = 1, main = "Akt. symp. infikovani", type = "b",
     ylab = "pocet", xlab = "cas", col = "red")
grid(col = "black")
lines(rad2, pch = 1, type = "b", col = "magenta")
lines(rad3, pch = 1, type = "b", col = "black")



## new
df = data.frame(days = 1:tmax, I1.su01 = rad1, I1.su26 = rad2, I1.su51 = rad3)

ggplotly(
  df %>% select(days, I1.su01, I1.su26, I1.su51) %>% 
    gather("legend", "value", -days) %>%
    ggplot(aes(x = days, y = value, col = legend)) +
    theme_minimal() +
    labs(y = "Počet Akt. symp. infikovaných",
         x = "dni") +
    geom_point(alpha = 0.6) + 
    geom_line()
)

## old
# d) Odhad distribucie vekov pozitivne testovanych
rad1 <- sign(I[, tmax + 5])*I[, tmax + 2]
hist(rad1[rad1 != 0], breaks = 1:10 - 0.5, labels = TRUE,
     main = "Odhad veku pozit. test.",
     xlab = "dekada veku", ylab = "pocet")

## new
df = data.frame(hist = rad1[rad1 != 0])
ggplotly(
df %>% ggplot(aes(x = hist)) + 
  geom_histogram(bins = 9) + 
  theme_minimal() + 
  labs(x = "Odhad veku pozit. test (dekada veku)")
)


## togeather
rad1 <- hist(I1[, tmax + 5], 
             breaks = 0:(tmax + 1) - 0.5, 
             plot = FALSE)$counts[2:(tmax + 1)]
fit <- cumsum(rad1)
Ctc <- cumsum(Ct)

I1.boliI <- matrix(0, nrow = nrow(I1), ncol = tmax)
for (i in 1:nrow(I1)) I1.boliI[i, ] <- sign(cumsum(abs(I1[i, 1:tmax])))
I1.boliI <- apply(I1.boliI, 2, sum)

I1.su51 <- I1.su26 <- I1.su01 <- matrix(0, nrow = nrow(I1), ncol = tmax)
for (i in 1:nrow(I1)) {
  I1.su01[i, ] <- sign(as.integer(I1[i, 1:tmax] != 0))
  I1.su26[i, ] <- sign(as.integer(abs(I1[i, 1:tmax]) > 0.25))
  I1.su51[i, ] <- sign(as.integer(abs(I1[i, 1:tmax]) > 0.51))
}
I1.su01 <- apply(I1.su01, 2, sum)
I1.su26 <- apply(I1.su26, 2, sum)
I1.su51 <- apply(I1.su51, 2, sum)



df = data.frame(days = 1:tmax, actual = Ctc, fit = rad1c,
                days0 = 0:(tmax - 1), inf = I1.boliI,
                I1.su01 = I1.su01, I1.su26 = I1.su26, I1.su51 = I1.su51)

ggplotly(
  df %>% select(days, actual, fit) %>% 
    gather("legend", "value", -days) %>%
    ggplot(aes(x = days, y = value, col = legend)) +
    theme_minimal() +
    labs(y = "Počet pozitivnych testov",
         x = "dni") +
    geom_point() + 
    geom_line()
)

ggplotly(
  df %>% 
    ggplot(aes(x = days0, y = inf)) + 
    theme_minimal() +
    labs(y = "Počet symp. infikovaných",
         x = "dni") +
    geom_point(alpha = 0.6) + 
    geom_line()
)

ggplotly(
  df %>% select(days, I1.su01, I1.su26, I1.su51) %>% 
    gather("legend", "value", -days) %>%
    ggplot(aes(x = days, y = value, col = legend)) +
    theme_minimal() +
    labs(y = "Počet Akt. symp. infikovaných",
         x = "dni") +
    geom_point(alpha = 0.6) + 
    geom_line()
)


his <- sign(I[, tmax + 5])*I[, tmax + 2]

df = data.frame(hist = his[his != 0])

ggplotly(
  df %>% ggplot(aes(x = his)) + 
    geom_histogram(bins = 9) + 
    theme_minimal() + 
    labs(x = "Odhad veku pozit. test (dekada veku)")
)



# run grid search
source("harmans_code_2.R")
source("covid_slovakia/model2.R")
start = Sys.time()
ju = corona_explore2(predv = 1:5, 
                     b0v = seq(100, 300, by = 10), 
                     gamma1 = 1.25, 
                     fin1 = 11, 
                     gamma2 = 1.06, 
                     n = 5)
end = Sys.time()
print(end - start) # expensive simulations

b0v = seq(100, 300, by = 10)
predv = 1:5
n = 5
gamma = 1.06

filled.contour(ju, x = b0v, y = predv, levels = 0:n,
               plot.title = title(main = paste("Fit, gamma2 =", gamma),
                                  xlab = "b0", ylab = "pred"))

# new
plot_ly(y = predv, x = b0v, z = t(1/ju), type = "contour",
        contours = list(showlabels = TRUE)) %>% 
  layout(title = paste("Best Fit Map"), 
         xaxis = list(title = "b0v"), 
         yaxis = list(title = "predv"))


Vch = t(ju)
cor = which(1/Vch == max(1/Vch), arr.ind = TRUE)
c(predv[cor[1]], b0v[cor[2]])
