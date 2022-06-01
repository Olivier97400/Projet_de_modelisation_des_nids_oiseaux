#Titre  : 3eme choix des paramètres initiaux, utilise le max de la densité de X 
#et la méthode des kmeans pour déterminer les valeurs d'une même espèce
#Arguments : X est l'échantillon et J le nombre d'espèces obervé dans l'échantillon 
#Value : Elle retourne un tableau de 3 colonnes contenant 
#les coéfficients des lois normales de chaque espèces
#Examples : param_kmeans(echantillon(data__test_th, 1000),3), avec 

#data_test_th = data.frame(bird_names = c("European Goldfinch", "Ring Ouzel"),
#proportion_alpha = c(0.3,0.7), mean = c(38, 298.6),sd = c(9.1, 125.1))

#Problème sur la précision des centres dans la fonction kmeans de R
param_kmeans <- function(X,J){
  density <- density(X)
  abscisse <- density$x
  ordonné <- density$y
  
  m_X <- abscisse[ggpmisc:::find_peaks(ordonné)]#positions des max
  
  centers <- data.frame(moyennes = m_X)
  size_m_X <- length(m_X)
  alpha <- rep(NA,J)
  var <- rep(NA,J)
  
  if (size_m_X > J){
    min <- m_X[m_X < mean(X)] 
    max <- m_X[m_X > mean(X)]
    min_max <- sort(c(abs(min-mean(X)),abs(max-mean(X))))
    i <- 1 
    while (size_m_X > J){
      if (!is.null(min_max)){
        m_X <- m_X[m_X != min_max[i]] #On enlève les max les plus éloignés
        size_m_X <- length(m_X)
      }
      else{
        m_X2 <- sort(m_X)
        m_X <- m_X[m_X != m_X2[i]] #On enlève les max les plus faibles
      }
      i <- i+1
    }
  }
  
  if (size_m_X==J){
    k <- kmeans(X,centers)
    m_X <- k$centers #permet d'éviter d'avoir de faux max
    indiv <- k$cluster
    for (j in 1:J){
      alpha[j] <- (1/100)*k$size[j]
      var[j] <- sqrt(var(X[indiv==j]))
    }
    param_init <- data.frame(espèces = 1:J, alpha = alpha, moyenne = m_X, variance = sqrt(var))
    return(param_init)
  }
  
  
  while (size_m_X<J){
    k <- kmeans(X,centers)
    indice_kmeans <- which.max(k$size)
    nv_kmeans <- kmeans(X[k$cluster==indice_kmeans],2)
    moyennes <- k$centers[-(indice_kmeans:size_m_X)]
    moyennes <- c(moyennes,nv_kmeans$centers)
    moyennes <- c(moyennes,k$centers[-(1:indice_kmeans)])
    m_X <- moyennes
    centers <- data.frame(moyennes = m_X)
    size_m_X <- size_m_X+1
  }
  
  k <- kmeans(X,centers)
  for (j in 1:J){
    indiv <- k$cluster
    alpha[j] <- (1/100)*k$size[j]
    var[j] <- sqrt(var(X[indiv==j]))
  }
  m_X <- k$centers
  param_init <- data.frame(espèces = 1:J, alpha = alpha, moyenne = m_X, 
                           variance = sqrt(var))
  return(param_init)
}