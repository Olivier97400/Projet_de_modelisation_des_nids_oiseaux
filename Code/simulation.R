# Cette fonction génère aléatoirement un échantillon de taille n issue 
# d'un mélange gaussien
# Les paramètres (alpha, mu et sigma) des différents mélanges gaussiens 
# sont contenus dans data_th
# La fonction prends en argument data_th 
# (le dataframe contenant les paramètres alpha, mu et sigma)
# et n qui indique le nombre de valeurs que l'on souhaite générer aléatoirement
# La fonction retourne un vecteur contenant les n valeurs du mélange gaussien
# qui ont été generées aléatoirement
simulation = function(data_th, n=100){
  X = rep(NA,n) #echantillon
  vect_alpha = data_th[,2]
  vect_mean = data_th[,3]
  vect_sd = data_th[,4]
  for(i in 1:n){
    Z = runif(1)
    if (Z <= vect_alpha[1]){
      X[i] = rnorm(1, vect_mean[1], vect_sd[1])
    }else{
      k = 1
      l = 2
      Bool = FALSE
      cumul_alpha = cumsum(vect_alpha)
      while(Bool == FALSE){
        if((cumul_alpha[k]<=Z) & (cumul_alpha[l]>=Z)){
          Bool = TRUE
          param_index = l
        }
        k = k+1
        l = l+1
      }
      X[i] = rnorm(1, vect_mean[param_index], vect_sd[param_index])
    }
  }
  return(X)
}