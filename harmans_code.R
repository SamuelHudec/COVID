# Staci skopirovat obsah tohto suboru do R-ka a prvu simulaciu spustit prikazom:
# corona_explore(seq(60, 180, by = 10), 1.06, 25:38)
# Potom mozete skusat ine parametre a modifikovat kod podla lubovole 
# Prosba: Obracajte sa prosim na mna len s informaciami a otazkami, ktore su 
# naozaj dolezite, pretoze pracujem paralelne na mnohych veciach (vacsinou v pozadi)
# Tento kod nie je viazany ziadnym copyrightom ani narokom na citacie a podobne hluposti
# Ide mi vylucne o to, ze by tento model a kod niekomu mohol pomoct 
# Precitajte si este prosim disclaimer nizsie

corona_sim <- function(x) {
  
  # Simulacia pozorovaneho poctu pozitivnych testovani na COVID-19
  #
  # Disclaimer: Moja specializacia su iste optimalizacne metody pouzitelne
  # o.i. na navrhovanie statistickych experimentov a na teoriu stochastickych
  # simulacii, nie som obornik na epidemiologicke modely. Tento moj simulacny 
  # program treba brat velmi kriticky a davam ho k dispozicii len preto,
  # ze zatial vykazuje prekvapivo dobru zhodu s poctami zistenych pripadov nakazy
  # v zavislosti od poctu testov na SR. Okrem ineho umoznuje kalibrovanim na
  # zistene pocty infekcii odhadnut skutocny pocet infikovanych, skutocne tempo
  # rastu nakazy a ine, priamo nepozorovatelne charakteristiky. Mozno bude niekomu 
  # na nieco uzitocny, napriklad ako nezavisle potvrdenie vysledkov modelov, ktore 
  # pripravuju profesionali z IZP a FMFI UK. 
  # Ospravedlnujem sa, ze je kod napisany a okomentovany neporiadne a dokonca moze
  # obsahovat chyby, ale na nakvalitnejsiu pracu v tomto zhone nemam schopnosti
  # RH, FMFI UK 26.3.2020
  # 
  # Zlozky vstupneho vektora x:
  #
  # b0 ... parameter beta-rozdelenia miery priznakov pre neinfikovanych
  #        Cim vacsie b0 tym menej sa nam do testovania zamiesavaju ne-covidi
  # gamma2 ... exponencialna miera denneho prirastku realne infikovanych
  #        po vyhlaseni nudzoveho stavu 12.3. 
  # tmax ... vek epidemie na Slovensku k aktualnemu dnu
  
  measure = FALSE
  
  b0 <- x[1] 
  gamma2 <- min(x[2], 1.4)  # Ak by som omylom zadal nealisticky velke
  # gamma2 > 1.4, spadme mi Rko (uz sa mi to stalo)
  tmax <- max(x[3], 12)
  
  # b1 az b9 ... parameter b rozdelenia Beta(1,b) maxima priznakov
  #              pre infikovanych v danych dekadach veku
  #              Vypocitane tak, aby ked prekroci maximum priznakov
  #              hodnotu 0.5, tak to znamena umrtie
  b <- -log(p, base = 2)
  
  # Ct ... pocty pozitivne testovanych ludi v SR od zaciatku epidemie
  # Tt ... pocty vsetkych testovanych ludi v SR od zaciatku epidemie
  # (zdroj: ezdravie.nczisk.sk/sk?category=COVID)
  Ct <- positive_tested
  Tt <- Ct + tested ## infikovaný neboli pod jedním a testovany??
  pred <- tmax - length(Ct)
  Ct <- c(rep(0, pred), Ct)
  Tt <- c(rep(0, pred), Tt)
  
  # Vygeneruj maticu I infikovanych ludi od dna 1 az po den tmax
  # pomocou Poissonovho rozdelenia s multiplikatorom strednej hodnoty gamma
  # Je to mozne pouzit aj iny model. Posledny stlpec v I je den infikovania
  I <- matrix(0, nrow = 1, ncol = tmax + 1)
  I[1, tmax + 1] <- 1
  I.act <- 1; Nt <- rep(1, tmax)
  
  if (measure) start = Sys.time()
  
  for (t in 2:tmax) {
    
    # Do nudzoveho stavu 12.3. bolo gamma niekde medzi 1.2 a 1.3
    # Cislo 15 sa posuva kazdy den; chcelo by to urobit krajsie
    ## na základe čoho sa posuva čislo 15??
    gamma <- gamma2; if (t < tmax - 15) gamma <- 1.25
    I.act <- I.act * gamma; Nt[t] <- rpois(1, lambda = I.act)
    if (Nt[t] > 0)
      ## toto je zbytočne drahé
      I <- rbind(I, cbind(matrix(0, nrow = Nt[t], ncol = tmax), t))
  }
  Ni <- nrow(I)   
  #print(c("infikovanych spolu", Ni))
  if (measure){
    end = Sys.time()
    print("Matrix preparing")
    print(end - start)
  }
  # Ak je infikovanych nerealne vela, preskoc simulaciu, aby si nezdrzoval
  # V istom bode nebude 10000 nerealne vela...
  if (Ni > 10000)
    return(list(chyba = Inf, Ni = 10000, smrt = 15,
                pp = 15, cZt = rep(0, tmax)))
  
  if (measure) start = Sys.time()
  # Pre kazdeho infikovaneho
  for (i in 1:Ni) {
    
    # generuj jeho vek nahodne podla a1-a9
    vek <- sample(1:9, prob = a)
    
    # generuj mieru priznakov v case az po tmax podla veku a b1-b9
    # Sposob generovania je snaha o napodobenie zisteni z 
    # https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf
    smax <- rbeta(1, shape1 = 1, shape2 = b[vek])
    # cas nadobudutia najsilnejsich priznakov od infikovania
    Tm <- ceiling(28*smax + 14*runif(1))
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
    # tak od toho casu prirad -Inf, co znamena umrtie
    if (sum(I[i, 1:tmax] > 0.5) > 0) {
      ts <- min((1:tmax)[I[i, 1:tmax] > 0.5])
      I[i, ts:tmax] <- -Inf
    }
    
  }
  if (measure){
    end = Sys.time()
    print("Simulating individual")
    print(end - start)
  }
  # Zratajme pocty umrti
  dth <- length((1:Ni)[I[, tmax] == -Inf])
  #print(c("umrtia", dth))
  
  # Ak je pocet umrti nerealne velky, preskoc simulaciu
  # Po istom case nemusi byt 15 umrti nerealne velke cislo...
  if (dth > 15)
    return(list(chyba = Inf, Ni = Ni, smrt = 15,
                pp = 15, cZt = rep(0, tmax)))
  
  # Vyrobime vektor zistenych poctov infekcie   
  Zt <- rep(0, tmax)
  tzac <- min((1:tmax)[Tt > 0])
  
  if (measure) start = Sys.time()
  # Pre kazdy den:
  for (t in tzac:tmax) {
    
    #  kolko realne infikovanych je medzi Tt[t] s najvyssimi priznakmi
    if (Tt[t] > 0) {
      # kriticka medza priznakov, ktora cloveku zabezpeci otestovanie
      krit <- qbeta(1 - Tt[t]/N, shape1 = 1, shape2 = b0)
      # Zt[t] bude pocet testovanych infikovanych v dni t
      # (predpokladame spolahlivost testov, co je zatial pravda)
      Zt[t] <- sum(I[ ,t] >= krit)
      I[I[ ,t] >= krit, t:tmax] <- -I[I[ ,t] >= krit, t:tmax]
    }
  }
  if (measure){
    end = Sys.time()
    print("simulate for every day")
    print(end - start)
  }
  
  # Percento pozitivnych testov
  pp <- round(100*mean(Zt[Tt > 0]/Tt[Tt > 0]))
  #print(c("percento pozitivnych testov", pp))
  
  # Chybu simulacie zratame porovnanim s pozorovanymi vysledkami testov
  # Cislo 20 sa zvysuje kazdy den o 1. Chcelo by to krajsie napisat
  Ztv <- cumsum(Zt[(tmax - 20):tmax])
  Ctv <- cumsum(Ct[(tmax - 20):tmax])
  chyba <- sum((Ctv - Ztv)^2/Ctv)
  
  # Nakresli priebeh, ale len ak nie je uplne odveci
  # (aby sme nezdrzovali opakovane simulacie kreslenim uletenych priebehov)
  # start = Sys.time()
  # if (chyba < 40) {
  #   par(mfrow = c(1, 1))
  #   mxcum <- max(c(sum(Zt), sum(Ct)))
  #   plot(cumsum(Zt), type = "b", pch = 19, ylim = c(0, mxcum),
  #        main = paste(b0, "|", gamma, "|", tmax, "|", Ni, 
  #                     "|", sum(Zt), "|", round(chyba, 2)))
  #   points(cumsum(Ct), pch = 19, type = "b", col = "red")
  #   mx <- max(c(Zt, Ct))
  #   lines(Zt[tmax:1]/mx*mxcum, type = "b", lty = "dotted")
  #   lines(Ct[tmax:1]/mx*mxcum, type = "b", lty = "dotted", col = "red")
  # }
  # end = Sys.time()
  # print("ploting")
  # print(end - start)
  
  
  list(chyba = chyba, Ni = Ni, smrt = dth, pp = pp, cZt = cumsum(Zt))
}

corona_explore <- function(b0v, gammav, tmaxv) {
  # Pre tuto verziu ma gammav je len dlzku 1
  # Potrebna je funkcia corona_sim, ktora robi jeden simulacny beh
  # K 26.3. su dobre fitujuce parametre pre SR napriklad
  # corona_explore(seq(60, 180, by = 10), 1.06, 25:38)
  # Avsak gamma v celom rozmedzi 0.99-1.1 fituje celkom dobre
  
  dm <- c(length(b0v), length(gammav), length(tmaxv))
  Nsim <- prod(dm); k <- 0
  Vch <- VNi <- Vs <- Vpp <- array(0, dim = dm)
  for (ib0 in 1:length(b0v)) {
    for (igamma in 1:length(gammav)) {
      for (itmax in 1:length(tmaxv)) {
        res1 <- corona_sim(c(b0v[ib0], gammav[igamma], tmaxv[itmax]))
        res2 <- corona_sim(c(b0v[ib0], gammav[igamma], tmaxv[itmax]))
        res3 <- corona_sim(c(b0v[ib0], gammav[igamma], tmaxv[itmax]))
        pchyba <- mean(c(res1$chyba, res2$chyba, res3$chyba))
        psmrt <- mean(c(res1$smrt, res2$smrt, res3$smrt))
        ppp <- mean(c(res1$pp, res2$pp, res3$pp))
        Vch[ib0, igamma, itmax] <- pchyba
        VNi[ib0, igamma, itmax] <- min(c(res1$Ni, 15000))
        Vs[ib0, igamma, itmax] <- min(c(psmrt, 15))
        Vpp[ib0, igamma, itmax] <- min(c(ppp, 15))
        # k <- k + 1; print(round(100*k/Nsim))
      }
    }
  }
  return(list(Vch = Vch, VNi = VNi, Vs = Vs, Vpp = Vpp))
}