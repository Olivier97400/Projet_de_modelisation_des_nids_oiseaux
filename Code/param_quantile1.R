#Titre  : 1er choix des paramètres initiaux, adapté à des échantillons 
#de même proportions et même variance
#Arguments : X est l'échantillon et J le nombre d'espèces obervé dans l'échantillon 
#Value : Elle retourne un tableau de 3 colonnes contenant 
#les coéfficients des lois normales de chaque espèces
#Examples : param_quantile1(echantillon(data__test_th, 1000),3), avec 

#data_test_th = data.frame(bird_names = c("European Goldfinch", "Ring Ouzel"),
#proportion_alpha = c(0.3,0.7), mean = c(38, 298.6),sd = c(9.1, 125.1))
param_quantile1 <- function(X,J){
  N <- length(X)
  Alpha = rep(1/J,J)
  Mean <- rep(NA,J)
  
  for(j in 1:J){
    Mean[j] <- floor(j*N/J+1)
  }
  
  X <- sort(X)
  Sd <- sqrt(var(X[1:Mean[1]]))
  Sd <- rep(Sd,J)
  
  data <- data.frame(init_alpha = Alpha, init_mu = Mean, init_sd = Sd)
  return(data)
}