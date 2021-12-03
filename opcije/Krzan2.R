# OPCIJE
# Finančni praktikum 2021/22
# Neža Kržan

library(combinat)
library(Rlab)

#============================================================
#PRVA NALOGA
#============================================================
#------------------------------------------------------------
#(a) Izplačila do zapadlosti
#------------------------------------------------------------
#Priprava podatkov 
  vrsta1 = c(50.00, 52.50, 49.88, 47.38, 45.01, 47.26)
  vrsta2 = c(50.00, 52.50, 55.12, 57.88, 60.78, 63.81)
  vrsta3 = c(50.00, 47.50, 49.88, 47.38, 45.01, 42.76)
  vrsta4 = c(50.00, 47.50, 45.12, 47.38, 45.01, 47.26)
  vrsta5 = c(50.00, 52.50, 49.88, 52.37, 54.99, 52.24)
  w = c(0, 1, 2, 3, 4, 5)
  
  X = "call"
  Y = "put"
  
#------------------------------------------------------------
#(b) Funkcija izplacilo(vrsta, W, type)
#------------------------------------------------------------
  izplacilo <- function(vrsta, W, type){
    I = sum(vrsta*W)/sum(W)
    if(type=="call"){
      return(max(tail(vrsta, n=1) - I,0))
    }
    else #še za put
    {
      return(max(I- tail(vrsta, n=1), 0))
    }
  }  
  
#Test funkcije izplacilo(vrsta, W, type) - podatki s spletne učilnice
  izplacilo(c(50,52.5,49.88,52.37,49.75,52.24),c(1,0,1,0,1,0),"call")
  izplacilo(c(50,52.5,55.12,57.88,60.78,63.81),1:6,"put")
  izplacilo(c(60,61.2,59.98,58.78,57.6,58.75,57.58),rep(1,7),"put")
  izplacilo(c(60,58.8,57.62,58.78,59.95,61.15,62.37),7:1,"call")
  izplacilo(c(70,66.5,69.83,73.32,76.98,73.13,69.48),c(0,1,2,1,3,2,3),"put")
  
#------------------------------------------------------------
#(a) Izplačila do zapadlosti - nadaljevanje
#------------------------------------------------------------ 
  X1_call <- izplacilo(vrsta1, w, X)
  Y1_put <- izplacilo(vrsta1, w, Y)
  X2_call <- izplacilo(vrsta2, w, X)
  Y2_put <- izplacilo(vrsta2, w, Y)
  X3_call <- izplacilo(vrsta3, w, X)
  Y3_put <- izplacilo(vrsta3, w, Y)
  X4_call <- izplacilo(vrsta4, w, X)
  Y4_put <- izplacilo(vrsta4, w, Y)
  X5_call <- izplacilo(vrsta5, w, X)
  Y5_put <- izplacilo(vrsta5, w, Y)

#Tabela izplačil
  S0 <- c(50.00, 50.00, 50.00, 50.00, 50.00)
  S1 <- c(52.50, 52.50, 47.50, 47.50, 52.50)
  S2 <- c(49.88, 55.12, 49.88, 45.12, 49.88)
  S3 <- c(47.38, 57.38, 47.38, 47.38, 52.37)
  S4 <- c(45.01, 60.78, 45.01, 45.01, 54.99)
  S5 <- c(47.26, 63.81, 42.76, 47.26, 52.24)
  izplaciloX <- c(X1_call, X2_call, X3_call, X4_call, X5_call)
  izplaciloY <- c(Y1_put, Y2_put, Y3_put, Y4_put, Y5_put)
  izplacila <- data.frame(S0, S1, S2, S3, S4, S5, izplaciloX, izplaciloY)

#============================================================
#DRUGA NALOGA
#============================================================
#------------------------------------------------------------
#(a) Funkcija binomski(S0,u,d,R,T,W,type)
#------------------------------------------------------------
  binomski <- function(S0, u, d, R, T, W, type){
    hc = hcube(rep(2,T)) - 1
    pot = u ** hc * (d ** (1 - hc))
    drevo <- cbind(rep(S0, 2**T), pot)
    kom_prod = t(apply(drevo, 1, cumprod))
    izplacila <- apply(kom_prod, 1, izplacilo, W=W, type=type)
    q = (1+R-d)/(u-d)
    stevilo = rowSums(hc)
    Q = q ** stevilo * (1-q) ** (T-stevilo)
    povprecje = sum(izplacila*Q)
    premija = povprecje / (1+R)**T
    return(premija)
  }
  
#Test funkcije binomski(S0, u, d, R, T W, type) -  podatki s spletne učilnice
  binomski(50,1.05,0.95,0.03,5,rep(1,6),"put") #0.1271746
  binomski(50, 1.05, 0.9 , 0.03, 10,0:10 , "call") #4.359636
  binomski(60, 1.05, 0.95, 0.01, 8,c(0,rep(1,8)),"put" ) #0.9115613
  binomski(70, 1.05, 1, 0, 7, rep(1,8), "call") #0
  binomski(80, 1.1 , 0.95, 0.05,  9, 12:3, "put" ) #0.1221909
  binomski(90, 1.15, 0.8 , 0.01, 11,rep(c(1,0),6), "call") #15.03903
  
#Test funkcije na naših podatkih
  binomski(50, 1.05, 0.95, 0.03, 5, c(1,2,3,4,5,6), "put") #0.1271746
  binomski(50, 1.05, 0.95, 0.03, 5, c(1,2,3,4,5,6), "call") #2.482287
  
#------------------------------------------------------------
#(b) Funkcija monte(S0,u,d,R,T,W,type,N)
#------------------------------------------------------------
  monte <-function(S0, u, d, R, T, W, type, N){
    q = (1+R-d)/(u-d)
    drevo1 <- matrix(rbinom(N*T, 1, q), N, T)
    drevo2 <- d**(1- drevo1) * u**(drevo1)
    
    i <- rowSums(drevo1)
    Q <- q**i *(1-q)**(T-i)
    
    drevo2 <- t(apply(drevo2, 1, cumprod))
    vrednosti <- cbind(S0, S0*drevo2) 
    
    izplacila <- apply(vrednosti, 1, izplacilo, W=W, type=type)
    E = sum(izplacila)/ length(izplacila)
    return (E/(1+R)^T)
  }

#Test funkcije monte(S0,u,d,R,T,W,type,N) -  podatki s spletne učilnice
  monte(50,1.05,0.9,0.03,10,0:10,"call",100) #4.70097
  monte(70, 1.05, 1, 0, 7,c(0,rep(1,7)), "put",2000) #0
  monte(90, 1.15, 0.8, 0.01, 10,11:1, "call",50000) #16.79661

#Uporaba funkcije
  monte(60,1.05,0.95,0.01,15,rep(1,16),"put",10) #0.03352054
  monte(60,1.05,0.95,0.01,15,rep(1,16),"put",100) #0.8069921
  monte(60,1.05,0.95,0.01,15,rep(1,16),"put",1000) #0.9495852
  
#============================================================
#TRETJA NALOGA
#============================================================
#------------------------------------------------------------
#(a) Ponovimo nalogo (2b) z M = 100-krat
#(b) Standardna napaka metode Monte Carlo
#------------------------------------------------------------
  monte10 <- c()
  monte100 <- c()
  monte1000 <- c()
  
  for(i in c(1:100)){
    monte10 <- c(monte10,monte(60,1.05,0.95,0.01,15,rep(1,16),"put", 10))
    monte100 <- c(monte100,monte(60,1.05,0.95,0.01,15,rep(1,16),"put", 100))
    monte1000 <- c(monte1000,monte(60,1.05,0.95,0.01,15,rep(1,16),"put",1000))
  } 
  
  vrednost_binomski <- binomski(60,1.05,0.95,0.01,15,rep(1,16),"put")
  
  min <-floor(min(monte10, monte100, monte1000))
  max <- ceiling((max(monte10, monte100, monte1000)))

#Standardne napake
#N=10
  povprecnaocena_monte10 <- mean(monte10)
  odklon_monte10 <- sqrt(var(monte10))
  standardni_leviodklon_monte10 <- vrednost_binomski - odklon_monte10
  standardni_desniodklon_monte10 <- vrednost_binomski + odklon_monte10
  
#N=100
  povprecnaocena_monte100 <- mean(monte100)
  odklon_monte100 <- sqrt(var(monte100))
  standardni_leviodklon_monte100 <- vrednost_binomski - odklon_monte100
  standardni_desniodklon_monte100 <- vrednost_binomski + odklon_monte100
  
#N=1000 
  povprecnaocena_monte1000 <- mean(monte1000)
  odklon_monte1000 <- sqrt(var(monte1000))
  standardni_leviodklon_monte1000 <- vrednost_binomski - odklon_monte1000
  standardni_desniodklon_monte1000 <- vrednost_binomski + odklon_monte1000
 
#Histogram N=10
  hist(monte10, breaks = 7,
       main = "MONTE CARLO: N=10",
       xlab = "premija",
       xlim = c(min, max),
       col="yellow")
  abline(v=povprecnaocena_monte10, col="green", lwd=1.1)
  abline (v = vrednost_binomski, col = "red", lty = "dashed", lwd=1.1)
  arrows (x0 = vrednost_binomski, y0=0, x1=standardni_desniodklon_monte10, col="green", length = 0.1, lwd=1.1)
  arrows(x0 = vrednost_binomski, y0=0, x1=standardni_leviodklon_monte10, col="green", length = 0.1, lwd=1.1)
  legend("topright", 
         legend = c("Monte Carlo", "analiza modela"),
         col = c("green", "red"),
         cex=0.8,
         lty=c("solid","dashed"),
         bty="n")
  
#Histogram N=100
  hist(monte100, breaks = 7,
       main = "MONTE CARLO: N=100",
       xlab = "premija",
       xlim = c(min, max),
       col="yellow")
  abline(v=povprecnaocena_monte100, col="green",lwd=1.1)
  abline (v = vrednost_binomski, col = "red", lty = "dashed",lwd=1.1)
  arrows (x0 = vrednost_binomski, y0=0, x1=standardni_desniodklon_monte100, col="green", length = 0.1,lwd=1.1)
  arrows(x0 = vrednost_binomski, y0=0, x1=standardni_leviodklon_monte100, col="green", length = 0.1,lwd=1.1)
  legend("topright", 
         legend = c("Monte Carlo", "analiza modela"),
         col = c("green", "red"),
         cex=0.8,
         lty=c("solid","dashed"),
         bty="n")
  
#Histogram N=1000
  hist(monte100, breaks = 7,
       main = "MONTE CARLO: N=1000",
       xlab = "premija",
       xlim = c(min, max),
       col="yellow")
  abline(v=povprecnaocena_monte1000, col="green",lwd=1.1)
  abline (v = vrednost_binomski, col = "red", lty = "dashed",lwd=1.1)
  arrows (x0 = vrednost_binomski, y0=0, x1=standardni_desniodklon_monte1000, col="green", length = 0.1,lwd=1.1)
  arrows(x0 = vrednost_binomski, y0=0, x1=standardni_leviodklon_monte1000, col="green", length = 0.1,lwd=1.1)
  legend("topright", 
         legend = c("Monte Carlo", "analiza modela"),
         col = c("green", "red"),
         cex=0.8,
         lty=c("solid","dashed"),
         bty="n")
  
  