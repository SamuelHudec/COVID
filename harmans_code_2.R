# Staci skopirovat obsah tohto suboru do R-ka a prvu simulaciu spustit napr prikazom:
# corona_sim2(6, c(120, 1.25, 11), c(120, 1.00, Inf), 100)
# Potom mozete skusat ine parametre a modifikovat kod podla lubovole 
# Prosba: Obracajte sa prosim na mna len s informaciami a otazkami, ktore su 
# naozaj dolezite, pretoze pracujem paralelne na mnohych veciach (vacsinou v pozadi)
# Tento kod nie je viazany ziadnym copyrightom ani narokom na citacie a podobne hluposti
# Ide mi vylucne o to, ze by tento model a kod mohol pomoct 
# Precitajte si este prosim disclaimer nizsie

errors <- function(x1, x2) {
  # Vektor podvojnych divergencii dvoch neklesajucoch postupnosti nezapornych cisel
  
  n <- length(x1)
  Err <- rep(0, n)
  
  for (i in 1:n) {
    if (x1[i] < 100) {
      lf1 <- log(factorial(x1[i]))
    } else {
      lf1 <- x1[i]*log(x1[i]) - x1[i] + 0.5*log(2*pi*x1[i])
    }
    if (x2[i] < 100) {
      lf2 <- log(factorial(x2[i]))
    } else {
      lf2 <- x2[i]*log(x2[i]) - x2[i] + 0.5*log(2*pi*x2[i])
    }
    m <- (x1[i] + x2[i])/2
    if (m < 100) {
      lfm <- log(gamma(m + 1))
    } else {
      lfm <- m*log(m) - m + 0.5*log(2*pi*m)
    }
    if (m > 0) Err[i] <- lf1 + lf2 - 2*lfm   
  }
  
  Err
}


corona_sim2 <- function(pred, x1, x2, n, pic = TRUE) {
  
  # Verzia 3
  # Simulacia (zatial) dvoch inicialnych faz epidemie COVID-19 na Slovensku
  # Bez predikcii do buducnosti (tiez je mozne doplnit neskor)
  #
  # Disclaimer: Moja specializacia su statisticke optimalizacne a simulacne
  # metody; nie som priamo odbornik na epidemiologicke modely. Tento moj simulacny 
  # program davam k dispozicii preto, lebo zatial vykazuje dobru zhodu s poctami
  # zistenych pripadov nakazy v zavislosti od poctu testov na Slovensku.
  # Okrem ineho umoznuje kalibrovanim na realne data pomoct odhadnut skutocny
  # pocet infikovanych, aktualne tempo rastu nakazy a ine nepozorovatelne udaje.
  # RH, FMFI UK 4.4.2020
  # 
  # Mometalne plauzibilne scenare:
  # corona_sim2(9, c(080, 1.25, 11), c(080, 0.95, Inf), 100)
  # corona_sim2(6, c(120, 1.25, 11), c(120, 1.00, Inf), 100)
  # corona_sim2(2, c(280, 1.25, 11), c(280, 1.05, Inf), 100)
  #
  # Vstupne parametere:
  #
  # pred ... kolko dni pred prvym marcom doslo k efektivnemu zaciatku epidemie
  #
  # Zlozky vstupneho vektora x1 urcujuceho prvu fazu
  # b01 ... parameter beta-rozdelenia miery priznakov pre neinfikovanych
  # gamma1 ... exponencialna miera denneho prirastku realne infikovanych
  # fin1 ... posledny den tejto fazy od 1.3
  #
  # Zlozky vstupneho vektora x2 urcujuceho druhu fazu
  # b02 ... parameter beta-rozdelenia miery priznakov pre neinfikovanych
  # gamma2 ... exponencialna miera denneho prirastku realne infikovanych
  # fin2 ... posledny den tejto fazy od 1.3.; zatial sa prepise na tmax - pred 
  #
  # n ... pocet simulacnych behov pre dany vektorovy parameter, aspon 3
  
  #### Hruba kontrola vstupu ####
  
  if (pred < 1 || pred > 30) stop("Nerealne pred")
  b01 <- x1[1]; if (b01 < 10) stop("Nerealne b01")
  gamma1 <- x1[2]; if (gamma1 < 1 || gamma1 > 2) stop("Nerealne gamma1")
  fin1 <- x1[3]; if (fin1 < 1) stop("Nemozne fin1")
  b02 <- x2[1]; if (b02 < 10) stop("Nerealne b02")
  gamma2 <- x2[2]; if (gamma2 < 0.5 || gamma2 > 1.3) stop("Nerealne gamma2")
  if (n < 3) stop("Prilis malo simulacnych behov")
  
  limNi <- 10000 # kolko simulovanych pripadov je uz nerealne (preskoci vypocet)
  
  #### Zadanie znamych dat zo SR ####
  
  N <- 5450000 # celkovy pocet ludi na Slovensku
  # a[1] az a[9] ... pomer ludi v SR v jednotlivych dekadach veku
  a <- c(0.11, 0.10, 0.12, 0.16, 0.15, 0.13, 0.13, 0.07, 0.03)
  # pravdepodobnosti umrtia v jednotlivych dekadach veku
  # rovnaky zdroj ako pouziva IZP, ale toto sa musi postupne spresnovat
  p <- c(0.005, 0.01, 0.03, 0.08, 0.15, 0.60, 2.20, 5.10, 9.30)/100
  # b[1] az b[9] ... parameter b rozdelenia Beta(1,b) maxima priznakov
  #                  pre symptomaticky infikovanych v danych dekadach veku.
  #                  Vypocitane tak, aby ked prekroci maximum priznakov
  #                  hodnotu 0.5, tak to znamena umrtie.
  b <- -log(p, base = 2)
  
  # Testy
  # Ct ... pocty pozitivne testovanych ludi v SR od zaciatku epidemie
  # Tt ... pocty vsetkych testovanych ludi v SR od zaciatku epidemie
  #        predpoklada sa nenulovy pocet testov kazdy den
  # (zdroj: ezdravie.nczisk.sk/sk?category=COVID)
  # Prvy pripad je 6.3.
  Ct <- c(0,0,0,0,0,1,2,2,2,0,3, # <- 11.3.
          11,11,12,17,11,24,8,19,14,41, # <- 21.3.
          7,19,12,10,43,23,22,22,27,37, # <- 31.3.
          26,24,21,14)
  Tt <- Ct + c(5,37,32,38,50,49,64,72,69,116,99, # <- 11.3.
               35,118,197,228,148,293,217,283,354,399, # <- 21.3.
               235,432,464,325,870,724,698,379,661,840, # <- 31.3.
               1165,1430,1889,1510)
  tmax <- length(Ct) + pred # Celkovy pocet dni epidemie
  Ct <- c(rep(0, pred), Ct)
  Tt <- c(rep(0, pred), Tt)
  
  fin2 <- tmax - pred
  
  # Potvrdene uzdravenia (zatial sa nepouziva)
  Ht <- c(0,0,0,0,0,0,0,0,0,0,0, # <- 11.3.
          0,0,0,0,0,0,0,0,0,0,0, # <- 21.3.
          0,0,0,0,2,0,0,0,0,1,0, # <- 31.3.
          0,2,3)
  
  # Potvrdene umrtia (zatial sa nepouziva)
  St <- c(0,0,0,0,0,0,0,0,0,0,0, # <- 11.3.
          0,0,0,0,0,0,0,0,0,0,0, # <- 21.3.
          0,0,0,0,0,0,0,0,0,0,0, # <- 31.3.
          0,0,0)
  
  # Distibucia dekad vekov odhalenych nakazenych (zatial sa nepouziva)
  h <- c(0,0,0,0,0,0,0,0,0)
  
  I1 <- I2 <- I3 <- NA # Tieto premenne budu matice I pre 3 najlepsie fity
  # I2 a I3 sa zatial nepouziva
  err1 <- err2 <- err3 <- Inf # Toto budu chyby troch najlepsich natic I
  errv <- rep(0, n) # Vektor chyb pre vsetkych n behov
  
  # Hlavny cyklus; najdeme matice I1,I2,I3 najlepsich fitov a vektor errv
  for (irun in 1:n) {
    
    # Vygeneruj maticu I infikovanych ludi od dna 1 az po den tmax epidemie
    # pomocou Poissona s multiplikatorom strednej hodnoty gamma1 do casu fin1
    # a multiplikatorom gamma2 medzi casmi fin1+1 a fin2
    # Je mozne pouzit aj iny model, klasicke power-law, renewal, ...
    
    # Interpretacia stlpcov pre maticu I vygenerovanych infikovanych
    # Stlpce 1 az tmax su miery priznakov v jednotlive dni
    # Stlpec tmax+1 je den nastupu priznakov
    # Stlpec tmax+2 je vek infikovaneho
    # Stlpec tmax+3 je smax infikovaneho; zatial sa nepouziva
    # Stlpec tmax+4 je Tm infikovaneho; zatial sa nepouziva
    # Stlpec tmax+5 je den zachytenia infikovaneho testom
    I <- matrix(0, nrow = 0, ncol = tmax + 5)
    
    # Vytvorime prazdnu maticu I spravneho rozmeru, s dnami "nastupu priznakov"
    I.act <- 1 # Aktualna stredna hodnota denneho poctu ludi ktorym nastupia priznaky
    
    # Prva faza; fin1 je posledny den prvej fazy
    for (t in 1:(fin1 + pred)) {
      novi <- rpois(1, lambda = I.act)
      if (novi > 0) {
        In <- matrix(0, nrow = novi, ncol = tmax + 5); In[, tmax + 1] <- t
        I <- rbind(I, In)
      }
      if (nrow(I) > limNi) return(rep(Inf, n))
      I.act <- I.act * gamma1 # Stredna hodnota odovzdana do dalsieho dna
    }
    
    # Druha faza
    for (t in (fin1 + pred + 1):(fin2 + pred)) {
      novi <- rpois(1, lambda = I.act)
      if (novi > 0) {
        In <- matrix(0, nrow = novi, ncol = tmax + 5); In[, tmax + 1] <- t
        I <- rbind(I, In)
      }
      if (nrow(I) > limNi) return(rep(Inf, n))
      I.act <- I.act * gamma2 # Stredna hodnota odovzdana do dalsieho dna
    }
    
    Ni <- nrow(I) # Pocet vsetkych infikovanych v tomto simulacnom behu
    
    # Pre kazdeho infikovaneho vygeneruj potrebne udaje v riadku matice I
    # okrem nastupu priznakov (uz je v I) a casu zachytenia (urci sa neskor)
    # Priznaky v stlpcoch 1:tmax je cca miera "podozrenia" na pritomnost virusu
    
    for (i in 1:Ni) {
      
      # generuj vek nahodne podla a[1]-a[9]
      vek <- sample(1:9, 1, prob = a); I[i, tmax + 2] <- vek
      
      # generuj mieru priznakov v case az po tmax podla veku a b[1]-b[9]
      # Momentalny sposob generovania je snaha o napodobenie zisteni z clankov
      smax <- rbeta(1, shape1 = 1, shape2 = b[vek]); I[i, tmax + 3] <- smax
      # cas nadobudutia najsilnejsich priznakov od infikovania
      Tm <- ceiling(28*smax + 14*runif(1)); I[i, tmax + 4] <- Tm
      # cas vyliecenia od infikovania (ak nedoslo k umrtiu)
      Tv <- 2*Tm  
      
      # priebeh intenzity priznakov pocas choroby
      x <- rep(0, Tv)
      x[1:Tm] <- seq(0, smax, length = Tm)
      x[Tm:Tv] <- seq(smax, 0, length = Tv - Tm + 1)
      
      # zaradenie priebehu priznakov na spravne miesto v matici I
      tinf <- I[i, tmax + 1]
      tkon <- min(c(tmax, tinf + Tv - 1))
      I[i, tinf:tkon] <- x[1:(tkon - tinf + 1)]
      
      # ak sa vygeneruje miera v niektorom case > 0.5
      # tak od toho casu prirad Inf, co znamena umrtie
      if (sum(I[i, 1:tmax] > 0.5) > 0) {
        ts <- min((1:tmax)[I[i, 1:tmax] > 0.5])
        I[i, ts:tmax] <- Inf
      }
    }
    
    # Prva faza 
    for (t in 1:fin1) {
      # kriticka medza priznakov, ktora zivemu cloveku zabezpeci otestovanie
      # Predp., ze mrtvi, ktori neboli identifikovani skor, uz testovani nie su
      krit <- qbeta(1 - Tt[t]/N, shape1 = 1, shape2 = b01)
      najdeni <- (I[ ,t] >= krit) & (I[ ,t] < Inf)
      # pozitivny test formalne zaznacime preklopenim priznakov okolo nuly
      I[najdeni, t:tmax] <- -I[najdeni, t:tmax]
      I[najdeni, tmax + 5] <- t
    }
    
    # Druha faza 
    for (t in (fin1 + 1):(fin2 + pred)) {
      # kriticka medza priznakov, ktora zivemu cloveku zabezpeci otestovanie
      # Predp., ze mrtvi, ktori neboli identifikovani skor, uz testovani nie su
      krit <- qbeta(1 - Tt[t]/N, shape1 = 1, shape2 = b02)
      najdeni <- (I[ ,t] >= krit) & (I[ ,t] < Inf)
      # pozitivny test formalne zaznacime preklopenim priznakov okolo nuly
      I[najdeni, t:tmax] <- -I[najdeni, t:tmax]
      I[najdeni, tmax + 5] <- t
    }
    
    # Vypocitame chybu simulacie v danom behu z realnych pozitivnych testov
    # Neskor moze byt chyba zalozena aj na inych odlisnostiach s realitou
    Zt <- hist(I[, tmax + 5], breaks = 0:(tmax + 1) - 0.5, 
               plot = FALSE)$counts[2:(tmax + 1)]
    errI <- mean(errors(cumsum(Zt), cumsum(Ct)))
    errv[irun] <- errI
    
    # Zistime, ci zaradit aktualne I medzi 3 najlepsie fity
    if (errI < err1) {
      I3 <- I2; I2 <- I1; I1 <- I
      err3 <- err2; err2 <- err1; err1 <- errI
    } else if (errI < err2) {
      I3 <- I2; I2 <- I
      err3 <- err2; err2 <- errI
    } else if (errI < err3) {
      I3 <- I
      err3 <- errI
    }
    
  }
  
  # Logika grafov:
  # - cervena sa tyka pozitivnych
  # - magenta sa tyka kritickych
  # - zelena sa tyka uzdravenych (v grafoch v buducnosti)
  # - cierna sa type mrtvych
  # - prazdny kruzok alebo bar zobrazuje simulacny odhad
  # - plny kruzok alebo bar sa zobrazuje pozorovane udaje
  
  
  if (pic) {
    par(mfrow = c(2, 2))
    # a) Priebeh odhalenych pripadov: skutocnych a pre najlepsi fit
    rad1 <- hist(I1[, tmax + 5], 
                 breaks = 0:(tmax + 1) - 0.5, 
                 plot = FALSE)$counts[2:(tmax + 1)]
    rad1c <- cumsum(rad1)
    Ctc <- cumsum(Ct)
    mx <- max(c(rad1c[tmax], Ctc[tmax]))
    print(c(rad1c[tmax], Ctc[tmax]))
    plot(Ctc, type = "b", pch = 19, ylim = c(0, mx), col = "red",
        main = "Kum. pozitivne testy", ylab = "pocet", xlab = "cas")
    grid(col = "black")
    lines(rad1c, pch = 1, type = "b", col = "red")
                 
    # b) Simulovany kumulatívny priebeh skutocneho poctu nakazenych
    I1.boliI <- matrix(0, nrow = nrow(I1), ncol = tmax)
    for (i in 1:nrow(I1)) I1.boliI[i, ] <- sign(cumsum(abs(I1[i, 1:tmax])))
    rad1 <- apply(I1.boliI, 2, sum)
    # Toto je odhad pre kpsi posunuty o den
    plot(0:(tmax - 1), rad1, pch = 1, col = "red",
        main = "Kum. symp. infikovani", type = "b",
        ylab = "pocet", xlab = "cas")
    grid(col = "black")
                 
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
                 
    # d) Odhad distribucie vekov pozitivne testovanych
    rad1 <- sign(I[, tmax + 5])*I[, tmax + 2]
    hist(rad1[rad1 != 0], breaks = 1:10 - 0.5, labels = TRUE,
        main = "Odhad veku pozit. test.",
        xlab = "dekada veku", ylab = "pocet")
    par(mfrow = c(1, 1))              
  }
  
  # Vratime do pripadneho master optimalizacneho programu vektor chyb
  return(list(errv = errv, I1 = I1, I = I, tmax = tmax))
}

corona_explore2 <- function(b0v, gammav, predv, n) {
  # Pre tuto verziu ma gammav je len dlzku 1
  # Sluzi na identifikaciu plauzibilnych hodnot b02 a pred pre dane gamma2 
  # Potrebna je funkcia corona_sim2, ktora robi jeden simulacny beh
  # Spusti napriklad corona_explore2(seq(30, 420, by = 15), 1.06, 1:20, 10)
  
  dm <- c(length(b0v), length(gammav), length(predv))
  Nsim <- prod(dm); k <- 0
  Vfit <- array(0, dim = dm)
  for (ib0 in 1:length(b0v)) {
    for (igamma in 1:length(gammav)) {
      for (ipred in 1:length(predv)) {
        res <- corona_sim2(predv[ipred],
                           c(b0v[ib0], 1.25, 11),
                           c(b0v[ib0], gammav[igamma], Inf), n, FALSE)$errv
        print(res)
        Vfit[ib0, igamma, ipred] <- sum(res < 1)
        k <- k + 1; print(paste(round(100*k/Nsim), "%"))
      }
    }
  }
  
  filled.contour(Vfit[,1,], x = b0v, y = predv, levels = 0:n,
                 plot.title = title(main = paste("Fit, gamma2 =", gammav[1]),
                                    xlab = "b0", ylab = "pred"))
  
  #return(Vfit)
}