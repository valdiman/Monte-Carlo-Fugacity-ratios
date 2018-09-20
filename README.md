# Monte-Carlo-Fugacity-ratios
R code to calculate the air-water fugacity ratios of individual PCB congener from Indiana Harbor and Ship Canal from air and water passive samples collected in 2007

# set working directory on your computer
setwd("Z:/.../Fugacity")

# Start functions

final.result = function(MW.PCB, H0.mean, H0.error, 
            C.PCB.water.mean, C.PCB.water.error, C.PCB.air.mean, C.PCB.air.error, nOrtho.Cl)
{
  ## fixed parameters
  
  R = 8.3144
  T = 298.15
  
  ## Monte Carlo simulation
  
  fug.ratio <- NULL
  for (replication in 1:1000)
  {
    
  ## random parameters
    
  a <- rnorm(1, 0.085, 0.007)
  b <- rnorm(1, 1, 0.5)
  c <- rnorm(1, 32.7, 1.6)
  H0 <- rnorm(1, H0.mean, H0.error) # should be normalized distribution
    
  ## Specific condition for each deployment
  C.PCB.water <- rnorm(1, C.PCB.water.mean, C.PCB.water.error) #ng/L
  C.PCB.air <- rnorm(1, C.PCB.air.mean, C.PCB.air.error) #ng/m3
  T.water <- rnorm(1, T.water.mean, T.water.error) #C 
  T.air <- rnorm(1, T.air.mean, T.air.error) #C

  ## Equations
    
  DeltaUaw <- (a*MW.PCB-b*nOrtho.Cl+c)*1000
  K <- 10^(H0)*101325/(R*T)
  K.air.water <- K*exp(-DeltaUaw/R*(1/(T.water+273.15)-1/T))
  K.final <- K.air.water*(T.water+273.15)/(T.air+273.15) # no units
    
  fug.ratio <- c(fug.ratio, (C.PCB.water*K.final)/(C.PCB.air/1000))
    
  }
  
  mmm <- mean(fug.ratio)
  sss <- sd(fug.ratio)
  q2.5 <- quantile(fug.ratio, 0.025, na.rm = TRUE)
  q97.5 <- quantile(fug.ratio, 0.975, na.rm = TRUE)
  
  c(mmm, sss, q2.5, q97.5) # 95% confidence intervals
}
  # Individual chemical properties, meteorological and environmental conditions
  
  ## Chemical properties (e.g., deployment 8)
  
  pars <- read.csv("Variables/Variables congenerD8.csv")
  Congener <- pars$Congener
  MW.PCB <- pars$MW.PCB
  H0.mean <- pars$H0
  H0.error <- pars$X
  C.PCB.water.mean <- pars$C.PCB.water #ng/L
  C.PCB.water.error <- pars$X.1 #20%
  C.PCB.air.mean <- pars$C.PCB.air #ng/m3
  C.PCB.air.error <- pars$X.2 #20%
  nOrtho.Cl <- pars$nOrtho.Cl
  
  ## Meteorological and environmental conditions
  
  parsM <- read.csv("Meteo_param.csv") #change number before braquets, for different deployment!
  T.air.mean <- parsM$D8[1]
  T.air.error <- parsM$X.8[1]
  T.water.mean <- parsM$D8[2]
  T.water.error <- parsM$X.8[2]
  
  # Results
  
  Num.Congener <- length(Congener)
  
  result <- NULL
  for (i in 1:Num.Congener)
  {
    result <- rbind(result, final.result(MW.PCB[i], H0.mean[i], H0.error[i], 
              C.PCB.water.mean[i], C.PCB.water.error[i], C.PCB.air.mean[i], C.PCB.air.error[i], nOrtho.Cl[i]))
  }
  
  final.result = data.frame(Congener, result)
  names(final.result) = c("Congener", "Mean", "Std", "2.5%CL", "97.5%CL")
  
  ## Example for deployment 8
  write.csv(final.result, row.names=F, file="Results/Fugacity_ratioD8_2.csv")
  
  
