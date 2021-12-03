# OBRESTNE KRIVULJE
# Finančni praktikum 2021/22
# Neža Kržan

#============================================================
#PRVA NALOGA
#============================================================

library(dplyr)
#------------------------------------------------------------
#(a) Uvoz podatkov
#------------------------------------------------------------
  library(readr)
  euribor_2015 <- read.csv2("hist_EURIBOR_2015.csv", row.names=1, sep = ",")
  euribor_2016 <- read.csv2("hist_EURIBOR_2016.csv", row.names=1, sep = ",")
  euribor_2017 <- read.csv2("hist_EURIBOR_2017.csv", row.names=1, sep = ",")

#------------------------------------------------------------
#(b) Zaporedne številke stolpcev prvih delovnih dni v mesecu
#------------------------------------------------------------
#Izbira prvih delovnih dni
  euribor_2015_prvi <- rbind(euribor_2015)[ ,c("X02.01.2015", "X02.02.2015", "X02.03.2015", "X01.04.2015",
                                               "X04.05.2015", "X01.06.2015", "X01.07.2015", "X03.08.2015", 
                                               "X01.09.2015", "X01.10.2015", "X02.11.2015", "X01.12.2015")]
  euribor_2016_prvi <- rbind(euribor_2016)[ ,c("X04.01.2016", "X01.02.2016", "X01.03.2016", "X01.04.2016",
                                               "X02.05.2016", "X01.06.2016", "X01.07.2016", "X01.08.2016",
                                               "X01.09.2016", "X03.10.2016", "X01.11.2016", "X01.12.2016")]
  euribor_2017_prvi <- rbind(euribor_2017)[ ,c("X02.01.2017", "X01.02.2017", "X01.03.2017", "X03.04.2017",
                                               "X02.05.2017", "X01.06.2017", "X03.07.2017", "X01.08.2017",
                                               "X01.09.2017", "X02.10.2017", "X01.11.2017", "X01.12.2017")]
#Transponiranje tabel s funkcijo t()
  library(TSstudio)
  euribor_2015_prvit <- t(euribor_2015_prvi)
  euribor_2016_prvit <- t(euribor_2016_prvi)
  euribor_2017_prvit <- t(euribor_2017_prvi)

#Poimenovanje stolpcev
  colnames(euribor_2015_prvit) <- c("1w","2w", "1m", "2m", "3m", "6m", "9m", "12m")
  colnames(euribor_2016_prvit) <- c("1w","2w", "1m", "2m", "3m", "6m", "9m", "12m")
  colnames(euribor_2017_prvit) <- c("1w","2w", "1m", "2m", "3m", "6m", "9m", "12m")
  
#Združitev vseh treh tabel
  euribor <- data.frame(rbind(euribor_2015_prvit,euribor_2016_prvit, euribor_2017_prvit))
  colnames(euribor) <- c("1w","2w", "1m", "2m", "3m", "6m", "9m", "12m")
  euribor_1 <- data.frame(rbind(euribor_2015_prvit,euribor_2016_prvit, euribor_2017_prvit)[ ,c("6m", "12m")])
  colnames(euribor_1) <- c("6m","12m")
  
#------------------------------------------------------------
#(c)Graf
#------------------------------------------------------------
#Sprememeba dataframe v Time-Series Object
  podatki1 <- ts(euribor_1[,"6m"], start=2015, frequency = 12)
  podatki2 <- ts(euribor_1[,"12m"], start=2015, frequency = 12)
  
#Graf
ts.plot(podatki1, podatki2, gpars=list(xlab="ČAS", ylab="%", col=c("blue", "red")), main="EURIBOR") 
  legend("topright", legend=c("6m","12m"), lty=1, col=c("blue", "red"), bty = "n")
  
#============================================================
#DRUGA NALOGA
#============================================================
#Poimenovanje tabele
  euribor_2a <- euribor
  euribor_2 <- data.frame(euribor_2a)
  colnames(euribor_2) <- c("1w","2w", "1m", "2m", "3m", "6m", "9m", "12m")
  
#------------------------------------------------------------ 
#(a) Izbira datumov
#------------------------------------------------------------
cas <- c(1/4, 1/2, 1, 2, 3, 6, 9, 12)
  
# izberem tri datume, ki se mi zdijo zanimivi
  izbrani_datumi <- euribor_2[c("X02.01.2015","X01.11.2017","X01.12.2017"),]
  colnames(izbrani_datumi) <- cas
  izbrani_datumi <- t(izbrani_datumi)
  
#------------------------------------------------------------  
#(b) Graf
#------------------------------------------------------------
  plot(cas, izbrani_datumi[,1], type = "b", col = "red2", lwd = 1, main="ČASOVNA STRUKTURA Euribor", xlab="Dospetje- mesec", ylab="%", ylim=c(-0.4,0.3))
  points(cas,  izbrani_datumi[,1] , pch = 21, cex = 1, bg = "red", col = "red", lwd = 3)  
  lines(cas, izbrani_datumi[,2], type="b", col="blue2")
  points(cas,  izbrani_datumi[,2] , pch = 21, cex = 1, bg = "blue", col = "blue", lwd = 3)  
  lines(cas, izbrani_datumi[,3], type="b", col="green2")
  points(cas,  izbrani_datumi[,3] , pch = 21, cex = 1, bg = "green", col = "green", lwd = 3)  
  text(11, 0.20, "2.1.2015", col="red")
  text(11, -0.15, "1.11.2017", col="blue")
  text(11, -0.24, "1.12.2017", col="green") 
  
#Komentar:
      #Vse tri krivulje so normalne krivulje donosnosti(vse tri so naraščajoče), 
      #zanimivo je tudi, da se krivulji za datuma 2.1.2017 in 1.12.2017 v prvih 
      #mesecih praktično ne razlikujeta, šele proti koncu pride do ralike.
      
#============================================================
#TRETJA NALOGA
#============================================================
#Poimenovanje tabele in priprava podatkov za računanje
  euribor_3 <- as.data.frame(euribor_2)
  t <- as.numeric(euribor_3[,"6m"]) 
  u <- as.numeric(euribor_3[,"12m"])

#------------------------------------------------------------
#(a)Vse možne terminske obrestne mere
#------------------------------------------------------------
#Uporabimo splošno formulo za izračun terminske obrestne mere(u=1 oz. 12 mesecev, t=0.5 oz. 6 mesecev)
    tom <-(1/(1/2)) * ((1+u)/(1+(1/2)*t) - 1)

#------------------------------------------------------------
#(b)Napoved
#------------------------------------------------------------
  terminskaobrestnamera <- tom
  terminskaobrestnamera <- round(terminskaobrestnamera, digits = 6)
  terminskaobrestnamera <- c(NA, NA, NA, NA, NA, NA, terminskaobrestnamera[7:36])
  euribor_napoved1 <- cbind(euribor_3[,0], t, u, terminskaobrestnamera)
  colnames(euribor_napoved1) <- c("Euribor6m", "Euribor12m", "Napoved6m")
  euribor_napoved <- euribor_napoved1[, c("Euribor6m", "Napoved6m")]  

#Vsa leta skupaj
  dejanskaobrestnamera <- euribor_napoved[, c("Euribor6m")]
  napovedanaobrestnamera <- euribor_napoved[, c("Napoved6m")]
  lin <- lm(napovedanaobrestnamera ~ dejanskaobrestnamera)

  leto_2015 <- euribor_napoved[1:12,]
  leto_2016 <- euribor_napoved[13:24,]
  leto_2017 <- euribor_napoved[25:36,]

#------------------------------------------------------------
#(b)Graf
#------------------------------------------------------------  
  range = c(min(euribor_napoved,na.rm = TRUE), max(euribor_napoved,na.rm = TRUE))
  plot(leto_2015[1:12,"Napoved6m"],leto_2015[1:12,"Euribor6m"],
       xlab="Napoved", ylab="Opazovano",
       main ="6m Euribor 2015-2017",
       type="p",
       pch=19,
       xlim=range, ylim=range,
       col="green")
  par(new=TRUE)
  plot(leto_2016[1:12,"Napoved6m"],leto_2016[1:12,"Euribor6m"],
       xlab="", ylab="",
       main ="",
       type="p",
       pch=19,
       xlim=range, ylim=range,
       col="grey")
  par(new=TRUE)
  plot(leto_2017[1:12,"Napoved6m"],leto_2017[1:12,"Euribor6m"],
       xlab="", ylab="",
       main ="",
       type="p",
       pch=19,
       xlim=range, ylim=range,
       col="yellow")
  legend("topleft",legend=c("2015","2016","2017"), pch=19, col=c("green","grey","yellow"),bty = "n")
  par(new=TRUE)
  abline(lm(euribor_napoved[,"Euribor6m"]~euribor_napoved[,"Napoved6m"]))
  par(new=TRUE)
  abline(coef = c(0,1),lwd=1, lty=2)
  
#------------------------------------------------------------
#(c)Grafi-posamezno leto
#------------------------------------------------------------ 
  plot(leto_2015[1:12,"Napoved6m"],leto_2015[1:12,"Euribor6m"],
       xlab="Napoved", ylab="Opazovano",
       main ="6m Euribor 2015",
       type="p",
       pch=19,
       xlim=range, ylim=range, 
       col="green")
  par(new=TRUE)
  abline(lm(euribor_napoved[1:12,"Euribor6m"]~euribor_napoved[1:12,"Napoved6m"]), col="green")
  par(new=TRUE)
  abline(coef = c(0,1),lwd=1, lty=2)
  
  plot(leto_2016[1:12,"Napoved6m"],leto_2016[1:12,"Euribor6m"],
       xlab="Napoved", ylab="Opazovano",
       main ="6m Euribor 2016",
       type="p",
       pch=19,
       xlim=range, ylim=range,
       col="grey")
  par(new=TRUE)
  abline(lm(euribor_napoved[13:24,"Euribor6m"]~euribor_napoved[13:24,"Napoved6m"]), col="grey")
  par(new=TRUE)
  abline(coef = c(0,1),lwd=1, lty=2)
  
  plot(leto_2017[1:12,"Napoved6m"],leto_2017[1:12,"Euribor6m"],
       xlab="Napoved", ylab="Opazovano",
       main ="6m Euribor 2017",
       type="p",
       pch=19,
       xlim=range, ylim=range,
       col="yellow")
  par(new=TRUE)
  abline(lm(euribor_napoved[25:36,"Euribor6m"]~euribor_napoved[25:36,"Napoved6m"]), col="yellow")
  par(new=TRUE)
  abline(coef = c(0,1),lwd=1, lty=2)

#------------------------------------------------------------
#(e)Diskusija grafov
#------------------------------------------------------------ 
  # Točke pri grafih v nalogi (c) in (d) bi morale biti na simetrali lihih kvadratov - pomenilo bi
  #da sta pričakovana obrestna mera in opazovana obrestna mera enaki.
  #Empirični podatki hipotezo ovržejo.