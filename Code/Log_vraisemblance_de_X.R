#Titre  : Log_vraisemblance de X
#Arguments : X est l'échantillon et data_param le tableau de 4 colonnes contenant 
#le nom des espèces et les coéfficients d'initialisations obtenus par une méthode 
#d'initialisation
#Value : Elle retourne la valeur de la log-vraisemblance de l'échantillon avec 
#les paramètres issus de data_param

#Examples : X <- echantillon(data__test_th, 1000)
#data <- data.frame(espèces = 1:2)
#data <- cbind(data,param_quantile2(X,2))
#log_Vrais_X(data,X), avec 

#data_test_th = data.frame(bird_names = c("European Goldfinch", "Ring Ouzel"),
#proportion_alpha = c(0.3,0.7), mean = c(38, 298.6),sd = c(9.1, 125.1))
log_Vrais_X <- function(data_param,X){
  J <- length(data_param[,1])
  logVrai_X <- 0
  for(i in 1:length(X)){
    sum_j <- 0
    for(j in 1:J){
      sum_j <- sum_j +  data_param[j,2]*dnorm(X[i],mean = data_param[j,3], 
                                              sd = data_param[j,4])
    }
    logVrai_X <- logVrai_X + log(sum_j)
  }
  return(logVrai_X)
}