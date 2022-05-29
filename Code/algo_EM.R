# Cette fonction est une implémentation de l'algorithme EM
# Elle prend en argument data_init
# (data_init est un dataframe contenant les paramètres (alpha, mu, sigma)
# initiaux choisis)
# un vecteur X qui est l'échantillon de taille n
# K le nombre d'itérations de l'algorithme EM
# Cette fonction va retourner un dataframe contenant les valeurs des
# paramètres alpha, mu et sigma
# qui ont été mises à jour après K itérations de l'algorithme EM

algo_EM = function(data_init, X, K){
  J = dim(data_init)[1]
  n = length(X)
  data_stateE = param_State_E(n, J)
  
  for(k in 1:K){
    vect_alpha = data_init[,2] #de longueur J
    vect_mean = data_init[,3]
    vect_sd = data_init[,4]
    # vecteur contenant la somme des des numérateurs de P_thetat(j|X = X_i)
    # pour chaque valeur de l’échantillon 
    v = rep(0,n) 
    
    # Etape E
    for(j in 1:J){
      # On remplie le tableau param_State_E contenant P_thetat(j|X=X_i)
      data_stateE[,j] = vect_alpha[j]*dnorm(X,vect_mean[j],vect_sd[j])
      v = v+data_stateE[,j]
    }
    for(j in 1:J){
      data_stateE[,j] = data_stateE[,j]/v
    }
    
    # Etape M
    H = data_stateE
    for(col in 2:4){ #on met a jour le data_init
      for(ind in 1:J){
        
        # on met à jour les alpha
        if(col == 2){
          data_init[,col][ind] = mean(H[,ind])
        }
        # on met à jour les mu
        if(col == 3){
          data_init[,col][ind] = (sum(X*H[,ind]))/(sum(H[,ind]))
        } 
        # on met à jour les sigma
        if(col == 4){
          data_init[,col][ind] = sqrt((sum( (X-rep(data_init[,col-1][ind],n))^2
                                            *H[,ind] ))/sum(H[,ind]))
        }
      }
    }
  }
  new_df = data_init
  colnames(new_df) = c('bird_names', 'alpha', 'mu', 'sigma')
  return(new_df)
}