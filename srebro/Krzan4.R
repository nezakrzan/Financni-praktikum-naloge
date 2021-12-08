# KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM
# Finančni praktikum 2021/22
# Neža Kržan

#============================================================
#PRVA NALOGA - Uvoz in predstavitev podatkov
#============================================================
#------------------------------------------------------------
#(a) Uvoz podatkov in obdobje zadnjih 6 mesecev
#------------------------------------------------------------
podatki_srebro <- read.csv("srebro21.csv")
podatki_srebro_6m <- podatki_srebro[127:1,]
podatki_srebro_6m <- podatki_srebro_6m[c(5)]

podatki_srebro_6m$Close <- as.numeric(gsub("\\$", "", podatki_srebro_6m$Close))

#------------------------------------------------------------
#(b) Graf časovne vrste
#------------------------------------------------------------
casovna_vrsta <- ts(podatki_srebro_6m)
srebro6m_graf <- ts.plot(casovna_vrsta, xlab = "Time", ylab = "USD", main = "Srebro")
points(casovna_vrsta, pch = 20)

#============================================================
#DRUGA NALOGA - Glajenje z drsečim povprečjem reda k
#============================================================
#------------------------------------------------------------
#(a) Funkcija G(vrsta,k)
#------------------------------------------------------------
G <- function(vrsta, k) {
  len <- length(vrsta)
  glajenje <- c()
  for (i in 1:(len-k)) {
    glajenje[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  zglajene_vrednosti <- ts(glajenje)
  return(zglajene_vrednosti)
}

#------------------------------------------------------------
#(b) Glajenje časovne vrste, k = 5 in napoved
#------------------------------------------------------------
glajenje_5 <- G(casovna_vrsta, 5)
len <- length(casovna_vrsta) #127L
napoved <- sum(casovna_vrsta[(len-5+1):len])/5 #24.172

#------------------------------------------------------------
#(c) Graf - zglajena vrsta(dodamo na graf 1(b))
#------------------------------------------------------------
ts.plot(casovna_vrsta, glajenje_5, xlab = "Time", ylab = "USD", main="Drseče povprečje",lwd=1:2,col=1:10)
points(casovna_vrsta, pch = 20)

#------------------------------------------------------------
#(d) Srednja kvadratna napaka - izračun
#------------------------------------------------------------
MSE <- function(vrsta, zglajena_vrsta, k) {
  T <- length(vrsta)
  delna_vsota <- 0
  for (i in (k+1): T) {
    delna_vsota <- delna_vsota + (vrsta[i] - zglajena_vrsta[i-k])^2
  }
  napaka <- (1/(T-k))*delna_vsota
  return(napaka)
}

MSE_5 <- MSE(casovna_vrsta, glajenje_5, 5) #0.289625639344262

#------------------------------------------------------------
#(e) Ponovno točba (b), (c), (d), k1 = 15, k2 = 30
#------------------------------------------------------------
glajenje_15 <- G(casovna_vrsta, 15)
napoved_15 <- sum(casovna_vrsta[(len-15+1):len])/15 #24.5226666666667
MSE_15 <- MSE(casovna_vrsta, glajenje_15, 15) #0.67463103968254

glajenje_30 <- G(casovna_vrsta, 30)
napoved_30 <- sum(casovna_vrsta[(len-30+1):len])/30 #24.2123333333333
MSE_30 <- MSE(casovna_vrsta, glajenje_30, 30) #0.974371639175258

#Grafi:
par(mfrow = (c(2,2)))
graf_5 <- ts.plot(casovna_vrsta, glajenje_5, xlab = "Time", ylab = "USD", main="Drseče povprečje reda 5",lwd=1:2,col=1:10)
points(casovna_vrsta, pch = 20)
graf_15 <- ts.plot(casovna_vrsta, glajenje_15, xlab = "Time", ylab = "USD", main="Drseče povprečje reda 15",lwd=1:2,col=1:10)
points(casovna_vrsta, pch = 20)
graf_30 <- ts.plot(casovna_vrsta, glajenje_30, xlab = "Time", ylab = "USD", main="Drseče povprečje reda 30",lwd=1:2,col=1:10)
points(casovna_vrsta, pch = 20)

#============================================================
#TRETJA NALOGA - Eksponentno glajenje
#============================================================
#------------------------------------------------------------
#(a) Funkcija EG(vrsta, alpha)
#------------------------------------------------------------
EG <- function(vrsta, alpha) {
  glajena_vrsta <- c(vrsta[1])
  l <- length(vrsta)
  for (i in 2:l) {
    glajena_vrsta[i] <- alpha*vrsta[i] + (1-alpha)*glajena_vrsta[i-1]
  }
  return(ts(glajena_vrsta))
}

#------------------------------------------------------------
#(b) 0.1 ≤ α ≤ 0.3  Glajenje časovne vrste,napoved in graf
#------------------------------------------------------------
par(mfrow = (c(1,1)))
izbrani_alpha = 0.2 #ne spomnim se dobro, ali je bilo naročeno, da mora biti 0.2

EG_napoved <- EG(casovna_vrsta, izbrani_alpha)

ts.plot(casovna_vrsta, EG_glajenje, xlab = "Time", ylab = "USD", main="Eksponento glajenje",lwd=1:2,col=1:10)
points(casovna_vrsta, pch = 20)
