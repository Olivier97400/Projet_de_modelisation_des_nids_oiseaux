# Cette fonction calcule l'erreur absolue entre les paramètres estimés par
# l'algo EM
# et les paramètres théoriques pour k echantillons générés aléatoirement
# Elle prend en argument data_th le dataframe contenant les paramètres
# alpha, mu et sigma théoriques
# data_init le dataframe contenant les paramètres (alpha, mu, sigma)
# initiaux choisis
# k le nombre d'échantillons que l'on souhaite générer aléatoirement
# n la taille des échantillons (les k échantillons seront de tailles n)
# N le nombre d'iterations de l'algorithme EM
# Elle retourne un dataframe contenant les erreurs absolues de chaque paramètre
# des k échantillons de Monte Carlo

Monte_Carlo = function(data_th, data_init, k, n, N){
  J = dim(data_th)[1]
  df_MonteCarlo = data.frame()
  iteration = c()
  for(i in 1:k){
    X = simulation(data_th, n)
    data_EM = algo_EM(data_init, X, N)
    df_error = calcul_error(data_th, data_EM)
    df_MonteCarlo = rbind((df_MonteCarlo), df_error)
    v_iter = rep(paste("itération",i,sep="_"), J)
    iteration = append(iteration, v_iter)
  }
  df_MonteCarlo = cbind(iteration, df_MonteCarlo)
  return(df_MonteCarlo)
}