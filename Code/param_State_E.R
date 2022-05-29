# Fonction générant dataframe (vide)
# contenant les valeurs de P_thetat(j|X = X_i) (etape E de l'algo)
# Cette fonction prend en argument n (la taille de l'échantillon)
# Et J le nombre de mélange
param_State_E = function(n, J){
  data_stateE = data.frame(matrix(NA, nrow = n, ncol = J))
  for(j in 1:J){
    names(data_stateE)[j] = paste("H(.,",j,")",sep="")
  }
  return(data_stateE)
}