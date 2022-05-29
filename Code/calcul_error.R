# Cette fonction calcule l'erreur absolue de tous les paramètres alpha, mu et sigma
# elle calcule la différence en valeur absolue entre la valeur du paramètre estimé
# par l'algorithme EM et la valeur du paramètre théorique
# Elle prend en paramètre data_th le dataframe contenant les 
# paramètres alpha, mu et sigma théoriques
# et Data_EM dataframe contenant les paramètres alpha, mu et sigma estimés 
# par l'algorithme EM
# Cette fonction retourne le dataframe contenant les erreurs absolues de chaque
# paramètre

calcul_error = function(data_th, data_EM){
  J = dim(data_th)[1]
  df_error = data.frame(error_alpha = rep(NA, J), error_mu = rep(NA, J),
                        error_sigma = rep(NA, J))
  for(c in 2:4){
    df_error[,c-1] = abs(data_th[,c] - data_EM[,c])
  }
  return(df_error)
}