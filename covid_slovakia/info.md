---
title: "Info"
output: html_document
---



# Theoretical background

We propose a simulation model to roughly estimate plausible values of some directly unobservable characteristics of the spread of the infection of the COVID-19 epidemic in Slovakia.At the moment of this writing we can only use the data series of the numbers of all tests and all confirmed cases, without a single death, and very few confirmed recovered individuals.

This manuscript reflects the current state of a work in progress. The main purpose of
this work is to support some modelling decisions of the governmental Institute of Health Policies of the Slovak Republic and other teams, for instance with respect to the calibration of more complex SIR-type models.

An important requirement of this model is that it is easy to implement and understand
such that it can undergo scrutiny from a broader community of professionals and produce useful results at the early stages of the epidemic.

[__Full text__](http://www.iam.fmph.uniba.sk/ospm/Harman/COR01.pdf)

***

# Technical background

App is powered by engine which code you can see below or [__As.txt__](http://www.iam.fmph.uniba.sk/ospm/Harman/programs/corona_sim.txt) and app lives in [__GitHub__](https://github.com/SamuelHudec/COVID).


```
  [1] "# Staci skopirovat obsah tohto suboru do R-ka a prvu simulaciu spustit napr prikazom:"   
  [2] "# corona_sim2(6, c(120, 1.25, 11), c(120, 1.00, Inf), 100)"                              
  [3] "# Potom mozete skusat ine parametre a modifikovat kod podla lubovole "                   
  [4] "# Prosba: Obracajte sa prosim na mna len s informaciami a otazkami, ktore su "           
  [5] "# naozaj dolezite, pretoze pracujem paralelne na mnohych veciach (vacsinou v pozadi)"    
  [6] "# Tento kod nie je viazany ziadnym copyrightom ani narokom na citacie a podobne hluposti"
  [7] "# Ide mi vylucne o to, ze by tento model a kod mohol pomoct "                            
  [8] "# Precitajte si este prosim disclaimer nizsie"                                           
  [9] ""                                                                                        
 [10] "errors <- function(x1, x2) {"                                                            
 [11] "  # Vektor podvojnych divergencii dvoch neklesajucoch postupnosti nezapornych cisel"     
 [12] "  "                                                                                      
 [13] "  n <- length(x1)"                                                                       
 [14] "  Err <- rep(0, n)"                                                                      
 [15] "  "                                                                                      
 [16] "  for (i in 1:n) {"                                                                      
 [17] "    if (x1[i] < 100) {"                                                                  
 [18] "      lf1 <- log(factorial(x1[i]))"                                                      
 [19] "    } else {"                                                                            
 [20] "      lf1 <- x1[i]*log(x1[i]) - x1[i] + 0.5*log(2*pi*x1[i])"                             
 [21] "    }"                                                                                   
 [22] "    if (x2[i] < 100) {"                                                                  
 [23] "      lf2 <- log(factorial(x2[i]))"                                                      
 [24] "    } else {"                                                                            
 [25] "      lf2 <- x2[i]*log(x2[i]) - x2[i] + 0.5*log(2*pi*x2[i])"                             
 [26] "    }"                                                                                   
 [27] "    m <- (x1[i] + x2[i])/2"                                                              
 [28] "    if (m < 100) {"                                                                      
 [29] "      lfm <- log(gamma(m + 1))"                                                          
 [30] "    } else {"                                                                            
 [31] "      lfm <- m*log(m) - m + 0.5*log(2*pi*m)"                                             
 [32] "    }"                                                                                   
 [33] "    if (m > 0) Err[i] <- lf1 + lf2 - 2*lfm   "                                           
 [34] "  }"                                                                                     
 [35] "  "                                                                                      
 [36] "  Err"                                                                                   
 [37] "}"                                                                                       
 [38] ""                                                                                        
 [39] ""                                                                                        
 [40] "corona_sim2 <- function(pred, x1, x2, n, pic = TRUE) {"                                  
 [41] "  "                                                                                      
 [42] "  # Verzia 3"                                                                            
 [43] "  # Simulacia (zatial) dvoch inicialnych faz epidemie COVID-19 na Slovensku"             
 [44] "  # Bez predikcii do buducnosti (tiez je mozne doplnit neskor)"                          
 [45] "  #"                                                                                     
 [46] "  # Disclaimer: Moja specializacia su statisticke optimalizacne a simulacne"             
 [47] "  # metody; nie som priamo odbornik na epidemiologicke modely. Tento moj simulacny "     
 [48] "  # program davam k dispozicii preto, lebo zatial vykazuje dobru zhodu s poctami"        
 [49] "  # zistenych pripadov nakazy v zavislosti od poctu testov na Slovensku."                
 [50] "  # Okrem ineho umoznuje kalibrovanim na realne data pomoct odhadnut skutocny"           
 [51] "  # pocet infikovanych, aktualne tempo rastu nakazy a ine nepozorovatelne udaje."        
 [52] "  # RH, FMFI UK 4.4.2020"                                                                
 [53] "  # "                                                                                    
 [54] "  # Mometalne plauzibilne scenare:"                                                      
 [55] "  # corona_sim2(9, c(080, 1.25, 11), c(080, 0.95, Inf), 100)"                            
 [56] "  # corona_sim2(6, c(120, 1.25, 11), c(120, 1.00, Inf), 100)"                            
 [57] "  # corona_sim2(2, c(280, 1.25, 11), c(280, 1.05, Inf), 100)"                            
 [58] "  #"                                                                                     
 [59] "  # Vstupne parametere:"                                                                 
 [60] "  #"                                                                                     
 [61] "  # pred ... kolko dni pred prvym marcom doslo k efektivnemu zaciatku epidemie"          
 [62] "  #"                                                                                     
 [63] "  # Zlozky vstupneho vektora x1 urcujuceho prvu fazu"                                    
 [64] "  # b01 ... parameter beta-rozdelenia miery priznakov pre neinfikovanych"                
 [65] "  # gamma1 ... exponencialna miera denneho prirastku realne infikovanych"                
 [66] "  # fin1 ... posledny den tejto fazy od 1.3"                                             
 [67] "  #"                                                                                     
 [68] "  # Zlozky vstupneho vektora x2 urcujuceho druhu fazu"                                   
 [69] "  # b02 ... parameter beta-rozdelenia miery priznakov pre neinfikovanych"                
 [70] "  # gamma2 ... exponencialna miera denneho prirastku realne infikovanych"                
 [71] "  # fin2 ... posledny den tejto fazy od 1.3.; zatial sa prepise na tmax - pred "         
 [72] "  #"                                                                                     
 [73] "  # n ... pocet simulacnych behov pre dany vektorovy parameter, aspon 3"                 
 [74] "  "                                                                                      
 [75] "  #### Hruba kontrola vstupu ####"                                                       
 [76] "  "                                                                                      
 [77] "  if (pred < 1 || pred > 30) stop(\"Nerealne pred\")"                                    
 [78] "  b01 <- x1[1]; if (b01 < 10) stop(\"Nerealne b01\")"                                    
 [79] "  gamma1 <- x1[2]; if (gamma1 < 1 || gamma1 > 2) stop(\"Nerealne gamma1\")"              
 [80] "  fin1 <- x1[3]; if (fin1 < 1) stop(\"Nemozne fin1\")"                                   
 [81] "  b02 <- x2[1]; if (b02 < 10) stop(\"Nerealne b02\")"                                    
 [82] "  gamma2 <- x2[2]; if (gamma2 < 0.5 || gamma2 > 1.3) stop(\"Nerealne gamma2\")"          
 [83] "  if (n < 3) stop(\"Prilis malo simulacnych behov\")"                                    
 [84] "  "                                                                                      
 [85] "  limNi <- 10000 # kolko simulovanych pripadov je uz nerealne (preskoci vypocet)"        
 [86] "  "                                                                                      
 [87] "  #### Zadanie znamych dat zo SR ####"                                                   
 [88] "  "                                                                                      
 [89] "  N <- 5450000 # celkovy pocet ludi na Slovensku"                                        
 [90] "  # a[1] az a[9] ... pomer ludi v SR v jednotlivych dekadach veku"                       
 [91] "  a <- c(0.11, 0.10, 0.12, 0.16, 0.15, 0.13, 0.13, 0.07, 0.03)"                          
 [92] "  # pravdepodobnosti umrtia v jednotlivych dekadach veku"                                
 [93] "  # rovnaky zdroj ako pouziva IZP, ale toto sa musi postupne spresnovat"                 
 [94] "  p <- c(0.005, 0.01, 0.03, 0.08, 0.15, 0.60, 2.20, 5.10, 9.30)/100"                     
 [95] "  # b[1] az b[9] ... parameter b rozdelenia Beta(1,b) maxima priznakov"                  
 [96] "  #                  pre symptomaticky infikovanych v danych dekadach veku."             
 [97] "  #                  Vypocitane tak, aby ked prekroci maximum priznakov"                 
 [98] "  #                  hodnotu 0.5, tak to znamena umrtie."                                
 [99] "  b <- -log(p, base = 2)"                                                                
[100] "  "                                                                                      
[101] "  # Testy"                                                                               
[102] "  # Ct ... pocty pozitivne testovanych ludi v SR od zaciatku epidemie"                   
[103] "  # Tt ... pocty vsetkych testovanych ludi v SR od zaciatku epidemie"                    
[104] "  #        predpoklada sa nenulovy pocet testov kazdy den"                               
[105] "  # (zdroj: ezdravie.nczisk.sk/sk?category=COVID)"                                       
[106] "  # Prvy pripad je 6.3."                                                                 
[107] "  Ct <- c(0,0,0,0,0,1,2,2,2,0,3, # <- 11.3."                                             
[108] "          11,11,12,17,11,24,8,19,14,41, # <- 21.3."                                      
[109] "          7,19,12,10,43,23,22,22,27,37, # <- 31.3."                                      
[110] "          26,24,21,14)"                                                                  
[111] "  Tt <- Ct + c(5,37,32,38,50,49,64,72,69,116,99, # <- 11.3."                             
[112] "               35,118,197,228,148,293,217,283,354,399, # <- 21.3."                       
[113] "               235,432,464,325,870,724,698,379,661,840, # <- 31.3."                      
[114] "               1165,1430,1889,1510)"                                                     
[115] "  tmax <- length(Ct) + pred # Celkovy pocet dni epidemie"                                
[116] "  Ct <- c(rep(0, pred), Ct)"                                                             
[117] "  Tt <- c(rep(0, pred), Tt)"                                                             
[118] "  "                                                                                      
[119] "  fin2 <- tmax - pred"                                                                   
[120] "  "                                                                                      
[121] "  # Potvrdene uzdravenia (zatial sa nepouziva)"                                          
[122] "  Ht <- c(0,0,0,0,0,0,0,0,0,0,0, # <- 11.3."                                             
[123] "          0,0,0,0,0,0,0,0,0,0,0, # <- 21.3."                                             
[124] "          0,0,0,0,2,0,0,0,0,1,0, # <- 31.3."                                             
[125] "          0,2,3)"                                                                        
[126] "  "                                                                                      
[127] "  # Potvrdene umrtia (zatial sa nepouziva)"                                              
[128] "  St <- c(0,0,0,0,0,0,0,0,0,0,0, # <- 11.3."                                             
[129] "          0,0,0,0,0,0,0,0,0,0,0, # <- 21.3."                                             
[130] "          0,0,0,0,0,0,0,0,0,0,0, # <- 31.3."                                             
[131] "          0,0,0)"                                                                        
[132] "  "                                                                                      
[133] "  # Distibucia dekad vekov odhalenych nakazenych (zatial sa nepouziva)"                  
[134] "  h <- c(0,0,0,0,0,0,0,0,0)"                                                             
[135] "  "                                                                                      
[136] "  I1 <- I2 <- I3 <- NA # Tieto premenne budu matice I pre 3 najlepsie fity"              
[137] "  # I2 a I3 sa zatial nepouziva"                                                         
[138] "  err1 <- err2 <- err3 <- Inf # Toto budu chyby troch najlepsich natic I"                
[139] "  errv <- rep(0, n) # Vektor chyb pre vsetkych n behov"                                  
[140] "  "                                                                                      
[141] "  # Hlavny cyklus; najdeme matice I1,I2,I3 najlepsich fitov a vektor errv"               
[142] "  for (irun in 1:n) {"                                                                   
[143] "    "                                                                                    
[144] "    # Vygeneruj maticu I infikovanych ludi od dna 1 az po den tmax epidemie"             
[145] "    # pomocou Poissona s multiplikatorom strednej hodnoty gamma1 do casu fin1"           
[146] "    # a multiplikatorom gamma2 medzi casmi fin1+1 a fin2"                                
[147] "    # Je mozne pouzit aj iny model, klasicke power-law, renewal, ..."                    
[148] "    "                                                                                    
[149] "    # Interpretacia stlpcov pre maticu I vygenerovanych infikovanych"                    
[150] "    # Stlpce 1 az tmax su miery priznakov v jednotlive dni"                              
[151] "    # Stlpec tmax+1 je den nastupu priznakov"                                            
[152] "    # Stlpec tmax+2 je vek infikovaneho"                                                 
[153] "    # Stlpec tmax+3 je smax infikovaneho; zatial sa nepouziva"                           
[154] "    # Stlpec tmax+4 je Tm infikovaneho; zatial sa nepouziva"                             
[155] "    # Stlpec tmax+5 je den zachytenia infikovaneho testom"                               
[156] "    I <- matrix(0, nrow = 0, ncol = tmax + 5)"                                           
[157] "    "                                                                                    
[158] "    # Vytvorime prazdnu maticu I spravneho rozmeru, s dnami \"nastupu priznakov\""       
[159] "    I.act <- 1 # Aktualna stredna hodnota denneho poctu ludi ktorym nastupia priznaky"   
[160] "    "                                                                                    
[161] "    # Prva faza; fin1 je posledny den prvej fazy"                                        
[162] "    for (t in 1:(fin1 + pred)) {"                                                        
[163] "      novi <- rpois(1, lambda = I.act)"                                                  
[164] "      if (novi > 0) {"                                                                   
[165] "        In <- matrix(0, nrow = novi, ncol = tmax + 5); In[, tmax + 1] <- t"              
[166] "        I <- rbind(I, In)"                                                               
[167] "      }"                                                                                 
[168] "      if (nrow(I) > limNi) return(rep(Inf, n))"                                          
[169] "      I.act <- I.act * gamma1 # Stredna hodnota odovzdana do dalsieho dna"               
[170] "    }"                                                                                   
[171] "    "                                                                                    
[172] "    # Druha faza"                                                                        
[173] "    for (t in (fin1 + pred + 1):(fin2 + pred)) {"                                        
[174] "      novi <- rpois(1, lambda = I.act)"                                                  
[175] "      if (novi > 0) {"                                                                   
[176] "        In <- matrix(0, nrow = novi, ncol = tmax + 5); In[, tmax + 1] <- t"              
[177] "        I <- rbind(I, In)"                                                               
[178] "      }"                                                                                 
[179] "      if (nrow(I) > limNi) return(rep(Inf, n))"                                          
[180] "      I.act <- I.act * gamma2 # Stredna hodnota odovzdana do dalsieho dna"               
[181] "    }"                                                                                   
[182] "    "                                                                                    
[183] "    Ni <- nrow(I) # Pocet vsetkych infikovanych v tomto simulacnom behu"                 
[184] "    "                                                                                    
[185] "    # Pre kazdeho infikovaneho vygeneruj potrebne udaje v riadku matice I"               
[186] "    # okrem nastupu priznakov (uz je v I) a casu zachytenia (urci sa neskor)"            
[187] "    # Priznaky v stlpcoch 1:tmax je cca miera \"podozrenia\" na pritomnost virusu"       
[188] "    "                                                                                    
[189] "    for (i in 1:Ni) {"                                                                   
[190] "      "                                                                                  
[191] "      # generuj vek nahodne podla a[1]-a[9]"                                             
[192] "      vek <- sample(1:9, 1, prob = a); I[i, tmax + 2] <- vek"                            
[193] "      "                                                                                  
[194] "      # generuj mieru priznakov v case az po tmax podla veku a b[1]-b[9]"                
[195] "      # Momentalny sposob generovania je snaha o napodobenie zisteni z clankov"          
[196] "      smax <- rbeta(1, shape1 = 1, shape2 = b[vek]); I[i, tmax + 3] <- smax"             
[197] "      # cas nadobudutia najsilnejsich priznakov od infikovania"                          
[198] "      Tm <- ceiling(28*smax + 14*runif(1)); I[i, tmax + 4] <- Tm"                        
[199] "      # cas vyliecenia od infikovania (ak nedoslo k umrtiu)"                             
[200] "      Tv <- 2*Tm  "                                                                      
[201] "      "                                                                                  
[202] "      # priebeh intenzity priznakov pocas choroby"                                       
[203] "      x <- rep(0, Tv)"                                                                   
[204] "      x[1:Tm] <- seq(0, smax, length = Tm)"                                              
[205] "      x[Tm:Tv] <- seq(smax, 0, length = Tv - Tm + 1)"                                    
[206] "      "                                                                                  
[207] "      # zaradenie priebehu priznakov na spravne miesto v matici I"                       
[208] "      tinf <- I[i, tmax + 1]"                                                            
[209] "      tkon <- min(c(tmax, tinf + Tv - 1))"                                               
[210] "      I[i, tinf:tkon] <- x[1:(tkon - tinf + 1)]"                                         
[211] "      "                                                                                  
[212] "      # ak sa vygeneruje miera v niektorom case > 0.5"                                   
[213] "      # tak od toho casu prirad Inf, co znamena umrtie"                                  
[214] "      if (sum(I[i, 1:tmax] > 0.5) > 0) {"                                                
[215] "        ts <- min((1:tmax)[I[i, 1:tmax] > 0.5])"                                         
[216] "        I[i, ts:tmax] <- Inf"                                                            
[217] "      }"                                                                                 
[218] "    }"                                                                                   
[219] "    "                                                                                    
[220] "    # Prva faza "                                                                        
[221] "    for (t in 1:fin1) {"                                                                 
[222] "      # kriticka medza priznakov, ktora zivemu cloveku zabezpeci otestovanie"            
[223] "      # Predp., ze mrtvi, ktori neboli identifikovani skor, uz testovani nie su"         
[224] "      krit <- qbeta(1 - Tt[t]/N, shape1 = 1, shape2 = b01)"                              
[225] "      najdeni <- (I[ ,t] >= krit) & (I[ ,t] < Inf)"                                      
[226] "      # pozitivny test formalne zaznacime preklopenim priznakov okolo nuly"              
[227] "      I[najdeni, t:tmax] <- -I[najdeni, t:tmax]"                                         
[228] "      I[najdeni, tmax + 5] <- t"                                                         
[229] "    }"                                                                                   
[230] "    "                                                                                    
[231] "    # Druha faza "                                                                       
[232] "    for (t in (fin1 + 1):(fin2 + pred)) {"                                               
[233] "      # kriticka medza priznakov, ktora zivemu cloveku zabezpeci otestovanie"            
[234] "      # Predp., ze mrtvi, ktori neboli identifikovani skor, uz testovani nie su"         
[235] "      krit <- qbeta(1 - Tt[t]/N, shape1 = 1, shape2 = b02)"                              
[236] "      najdeni <- (I[ ,t] >= krit) & (I[ ,t] < Inf)"                                      
[237] "      # pozitivny test formalne zaznacime preklopenim priznakov okolo nuly"              
[238] "      I[najdeni, t:tmax] <- -I[najdeni, t:tmax]"                                         
[239] "      I[najdeni, tmax + 5] <- t"                                                         
[240] "    }"                                                                                   
[241] "    "                                                                                    
[242] "    # Vypocitame chybu simulacie v danom behu z realnych pozitivnych testov"             
[243] "    # Neskor moze byt chyba zalozena aj na inych odlisnostiach s realitou"               
[244] "    Zt <- hist(I[, tmax + 5], breaks = 0:(tmax + 1) - 0.5, "                             
[245] "               plot = FALSE)$counts[2:(tmax + 1)]"                                       
[246] "    errI <- mean(errors(cumsum(Zt), cumsum(Ct)))"                                        
[247] "    errv[irun] <- errI"                                                                  
[248] "    "                                                                                    
[249] "    # Zistime, ci zaradit aktualne I medzi 3 najlepsie fity"                             
[250] "    if (errI < err1) {"                                                                  
[251] "      I3 <- I2; I2 <- I1; I1 <- I"                                                       
[252] "      err3 <- err2; err2 <- err1; err1 <- errI"                                          
[253] "    } else if (errI < err2) {"                                                           
[254] "      I3 <- I2; I2 <- I"                                                                 
[255] "      err3 <- err2; err2 <- errI"                                                        
[256] "    } else if (errI < err3) {"                                                           
[257] "      I3 <- I"                                                                           
[258] "      err3 <- errI"                                                                      
[259] "    }"                                                                                   
[260] "    "                                                                                    
[261] "  }"                                                                                     
[262] "  "                                                                                      
[263] "  # Vratime do pripadneho master optimalizacneho programu vektor chyb"                   
[264] "  if(pic){"                                                                              
[265] "    return(list(I1 = I1, I = I, tmax = tmax, Ct = Ct))"                                  
[266] "  }else{return(errv)}"                                                                   
[267] "  "                                                                                      
[268] "}"                                                                                       
[269] ""                                                                                        
[270] "corona_explore2 <- function(predv, b0v, gamma1, fin1, gamma2, n) {"                      
[271] "  # Pre tuto verziu ma gammav je len dlzku 1"                                            
[272] "  # Sluzi na identifikaciu plauzibilnych hodnot b02 a pred pre dane gamma2 "             
[273] "  # Potrebna je funkcia corona_sim2, ktora robi jeden simulacny beh"                     
[274] "  # Spusti napriklad corona_explore2(seq(30, 420, by = 15), 1.06, 1:20, 10)"             
[275] "  "                                                                                      
[276] "  dm <- c(length(b0v), length(predv))"                                                   
[277] "  Vfit <- array(0, dim = dm)"                                                            
[278] "  for (ib0 in 1:length(b0v)) {"                                                          
[279] "    for (ipred in 1:length(predv)) {"                                                    
[280] "      res <- corona_sim2(predv[ipred],"                                                  
[281] "                         c(b0v[ib0], gamma1, fin1),"                                     
[282] "                         c(b0v[ib0], gamma2, Inf), n, FALSE)"                            
[283] "      Vfit[ib0, ipred] <- mean(res) # alebo sum(res < 1)"                                
[284] "    }"                                                                                   
[285] "  }"                                                                                     
[286] "  return(Vfit)"                                                                          
[287] "}"                                                                                       
```

