# KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM
# Finančni praktikum 2021/22
# Neža Kržan

# Funkcije, ki jih bomo uporabili pri reševanju naloge

#scan          # branje podatkov
#hist          # risanje histograma

#pexp          # porazdelitvena funkcija eksponentne porazdelitve
#dexp          # gostota verjetnosti
#rexp          # simulacija

#ppareto1      # Paretova porazdelitev
#dpareto1
#rpareto1

#pweibull      # Weibullova porazdelitev
#dweibull
#rweibull

#rbinom        # binomska porazdelitev
#rpois         # Poissonova porazdelitev
#rnbinom       # negativna binomska porazdelitev

#mde           # ocenjevanje parametrov po metodi najmanjše razdalje

#curve         # risanje krivulj
#stepfun       # risanje odsekoma konstantnih (stopničastih) funkcij

#ecdf          # vzorčna porazdelitvena funkcija

#discretize    # diskretizacija porazdelitvene funkcije
#aggregateDist # računanje porazdelitve kumulativne škode (Panjerjev algoritem)

#mean          # povprečje
#var           # vzorčna disperzija

#knots         # mesta skokov kumulativne porazdelitve
#diff          # višine skokov kumulativne porazdelitve
#diffinv       # inverzna funkcija diff

library(actuar)

#============================================================
#PRVA NALOGA - Porazdelitev individualnih škodnih zahtevkov
#============================================================
#------------------------------------------------------------
#(a) Izbira vzorca, uvoz podatkov, histogram
#------------------------------------------------------------
#Izbira vzorca: 
  #Vzorec 1

#Uvoz podatkov:
  podatki_vzorec1 <- read.delim("vzorec1.txt")
  colnames(podatki_vzorec1) <- c("Podatki")

#Histogram vzorčne porazdelotve škodnih zahtevkov
  histogram <-hist(podatki_vzorec1$Podatki, 
                   main="Histogram odškodnin",
                   xlab="Višina odškodnin", 
                   col="lightblue")
  
#------------------------------------------------------------
#(b)Porazdelitev spremenljivke Y
#------------------------------------------------------------
  #parametri <- mde(podatki_vzorec1$Podatki, ppareto1, start=list("shape" = 1, "min" = 20), measure = "CvM")
  #shape_pareto1=parametri$estimate[1]
  #min_pareto1=parametri$estimate[2]
  
  parametri <- mde(podatki_vzorec1$Podatki, pweibull, start=list("shape" = 1, "scale" = 5), measure = "CvM")
  shape_weibull=parametri$estimate[1]
  scale_weibull=parametri$estimate[2]
  
#------------------------------------------------------------
#(c)Histogram - gostota spremeljivke Y
#------------------------------------------------------------
#Histogram - Weibullova poprazdelitev
  #hist(podatki_vzorec1$Podatki, 
       #main="Histogram odškodnin",
       #xlab="Višina odškodnin", 
       #col="lightblue",
       #probability = TRUE,
       #ylim=c(0,0.8),
       #breaks=10) 
  #curve(dpareto1(x, shape_pareto1, min_pareto1), col="blue", add=TRUE, lwd=2)
  #legend("topright", legend = c("Pareto porazdelitev"),
         #col = c("blue"), lty=1:1)
  
   hist(podatki_vzorec1$Podatki, 
       main="Histogram odškodnin",
       xlab="Višina odškodnin", 
       col="lightblue",
       probability = TRUE,
       ylim=c(0,0.8),
       breaks=15) 
  curve(dweibull(x, shape_weibull, scale_weibull), col="blue", add=TRUE, lwd=2)
  legend("topright", legend = c("Weibullova porazdelitev"),
         col = c("blue"), lty=1:1)
  
#Teoretična porazdelitvena funkcija
  plot(ecdf(podatki_vzorec1$Podatki), 
       xlab="Višina odškodnine", ylab="Porazdelitvena funkcija", 
       main="Porazdelitvena funkcija odškodnin")
  curve(pweibull(x, shape_weibull, scale_weibull), col="blue", add=TRUE, lwd=2)
  legend("right", legend = c("empirična porazdelitev", "Weibullova porazdelitev"),
         col = c("black", "blue"), lty=1:1, bty="n")
  
#------------------------------------------------------------
#(d)Upanje in disperzija
#------------------------------------------------------------
#N - Bin(n, p)
  n=20
  p=1/2
  upanje_N <- n*p
  disperzija_N <- n*p*(1-p)
  
#Spremenljivka Y
  upanje_Y <- scale_weibull*gamma(1+(1/shape_weibull))
  disperzija_Y <- (scale_weibull)^2*(gamma(1+(2/shape_weibull)) - (gamma(1+(1/shape_weibull)))^2)
  
#Upanje in disperzija S(Walldowe identitete)
  upanje_S <- upanje_N * upanje_Y
  disperzija_S <- disperzija_Y*upanje_N + upanje_Y^2 * disperzija_N

#============================================================
#DRUGA NALOGA - Določanje porazdelitve kumulativne škode s 
                #Panjerjevim algoritmom
#============================================================
#------------------------------------------------------------
#(a) Z zaokroževanjem diskretizirajte porazdelitev spremenljivke Y 
#------------------------------------------------------------
#Verjetnostntna gostota Y(h=0.25(rekla je tako)), uporabi funkcijo discretize
  h=0.25
  diskritirana_Y <- discretize(pweibull(x, shape_weibull, scale_weibull), 
                               from = 0, to = (h*(10/h)), by = h, 
                               method="rounding")

#------------------------------------------------------------
#(b)Graf porazdelutvene funkcije Y - uporabi stepfun
#------------------------------------------------------------
  plot(stepfun(seq(0, (10/h - 1) * h , h), diffinv(diskritirana_Y)), 
       main = "Weibullova porazdelitev", ylab="Porazdelitvena funkcija", 
       col="orange")
  curve(pweibull(x, shape_weibull, scale_weibull), col="black", add=TRUE, lwd=2)

#------------------------------------------------------------
#(c) Porazdelitvena funkcija s Panjerejvim algoritmom
#------------------------------------------------------------ 
#Izračun komulativnih škod 
  h=0.25
  komulativne_skode <- discretize(pweibull(x, shape_weibull, scale_weibull), 
                                  from = 0, to = (h*(10/h)), by = h)

#Panjerjev algoritem s funkcijo aggregateDist
  panjarjev_algoritem <- aggregateDist(method="recursive", model.freq = "binom",
                                    model.sev=komulativne_skode, 
                                    size = n, prob = p, 
                                    x.scale = 0.25, maxit=10000, tol = 0.0005)
  
#------------------------------------------------------------
#(d) Upanje in disperzija komulativne škode spremenljivke S
#------------------------------------------------------------   
  upanje_komulativna_skoda <- mean(panjarjev_algoritem)
  disperzija_komulativna_skoda <- sum(diff(panjarjev_algoritem)*knots(panjarjev_algoritem)**2) - upanje_komulativna_skoda**2
  
#============================================================
#TRETJA NALOGA - Določanje porazdelitve kumulativne škode z 
                 #Monte Carlo simulacijami
#============================================================  
#------------------------------------------------------------
#(a) Simulacija
#------------------------------------------------------------
  stevilo_zahtevkov <- rbinom(10000, n, p) 
  vsota_skode <- c()
  for (i in stevilo_zahtevkov){
    vsota_skode <- c(vsota_skode, sum(rweibull(i, shape_weibull, scale_weibull)))
  }

#------------------------------------------------------------
#(b) Ocenitev upanja in disperzije spremenljivke S
#------------------------------------------------------------
#Ocenitev iz simulacije
  ocena_upanje <- mean(vsota_skode) #35.43182
  ocena_disperzija<- var(vsota_skode) #66.65821 

#Podatki iz naloge 2(d)
  upanje_komulativna_skoda #34.0302
  disperzija_komulativna_skoda #61.89979
  
#------------------------------------------------------------
#(c) Graf simulirane porazdelitvene funkcije za S
#------------------------------------------------------------   
#Graf
  plot(ecdf(vsota_skode), col="green", add =TRUE)
  legend("bottomright", legend = c("Panjarjev algoritem", "Monte Carlo Simulacija"),
         col = c("black", "green"), lty=1:1, bty="n")
  
#Graf 2(c)
  plot(panjarjev_algoritem, pch=100)
  