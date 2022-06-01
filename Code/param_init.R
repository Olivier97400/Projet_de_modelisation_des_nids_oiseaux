#Titre  : Choix des paramètres initaux entre plusieurs fonctions d'initialisations
#Arguments : X est l'échantillon et data_param le tableau de 
#1+3*(le nombre de fonctions d'initalisations) colonnes contenant 
#le nom des espèces et les coéfficients d'initialisations obtenus par ces fonctions
#Value : Elle retourne le tableau des paramètres initaux pour le quel on a obtenu 
#une log-vraisemblance la plus grande 

#Examples : X <- simulation(data_test_th, 1000)
#data <- data.frame(espèces = 1:2)
#data <- cbind(data,param_quantile1(X,2),param_quantile2(X,2))
#param_init(data,X), avec 

#data_test_th = data.frame(bird_names = c("European Goldfinch", "Ring Ouzel"),
#proportion_alpha = c(0.3,0.7), mean = c(38, 298.6),sd = c(9.1, 125.1))
param_init <- function(data,X){
  J <- length(data[,1])
  col <- length(data[1,])-1
  res <- -Inf
  data_res <- data.frame()
  for(i in seq(1,col,by = 3)){
    data_i <- data.frame(espèces = data[,1], alpha = data[,i+1],
                         moyennes = data[,i+2], sd = data[,i+3])
    log_Vrai <- log_Vrais_X(data_i,X)
    if (res <= log_Vrai){
      res <- log_Vrai
      data_res <- data_i
    }
  }
  return(data_res)
}
