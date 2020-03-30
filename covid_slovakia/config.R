# ui and server static parameters
b0v_step = 10
gammav_step = 0.01
tmaxv_step = 1

# N ... celkovy pocet ludi v SR
N <- 5450000

# a1 az a9 ... pomer ludi v SR v jednotlivych dekadach veku
a <- c(0.11, 0.10, 0.12, 0.16, 0.15, 0.13, 0.13, 0.07, 0.03)

# pravdepodobosti umrtia v jednotlivych dekadach veku
p <- c(0.2, 0.2, 0.2, 0.2, 0.4, 1.3, 3.6, 8.0, 14.8)/100

# pocty pozitivne testovanych ludi v SR od zaciatku epidemie
# pocty vsetkych testovanych ludi v SR od zaciatku epidemie
positive_tested = c(0,0,0,0,1,2,2,2,0,3,11,11,12,17,11,25,8,19,13,41,7,19,12,10,42)
tested = c(37,32,38,50,49,64,72,69,116,99,35,118,197,228,148,293,217,283,354,399,235,432,464,325,912)