#Titre  : 2eme choix des paramètres initiaux, adapté à des échantillons 
#de même proportions
#Arguments : X est l'échantillon et J le nombre d'espèces obervé dans l'échantillon 
#Value : Elle retourne un tableau de 3 colonnes contenant 
#les coéfficients des lois normales de chaque espèces
#Examples : param_quantile2(echantillon(data__test_th, 1000),3), avec 

#data_test_th = data.frame(bird_names = c("European Goldfinch", "Ring Ouzel"),
#proportion_alpha = c(0.3,0.7), mean = c(38, 298.6),sd = c(9.1, 125.1))

param_quantile2 <- function(X,J){
  N <- length(X)
  Alpha = rep(1/J,J)
  Mean <- rep(NA,J)
  X <- sort(X)
  
  index1_Q <- 1
  index2_Q <- 0
  for(j in 1:J-1){
    index2_Q <- floor(j*N/J)+1
    Mean[j] <- mean(X[index1_Q:index2_Q])
    index1_Q <- index2_Q+1
  }
  Mean[J] <- mean(X[index1_Q:N])
  
  Sd <- sqrt(var(X[1:floor(N/J)+1]))
  Sd <- rep(Sd,J)
  
  data <- data.frame(init_alpha = Alpha, init_mu = Mean, init_sd = Sd)
  return(data)
}