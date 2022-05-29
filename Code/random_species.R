# Cette fonction prend en argument df le dataframe regroupant toutes les données
# c'est à dire les noms, les moyennes et les écart-types des nids des 13
# espèces d'oiseaux
# n le nombre d'espèces à tirer aléatoirement
# cette fonction retourne un dataframe stockant les noms,
# moyennes et écart-types des nids des n espèces d'oiseaux tirées aléatoirement
random_species = function(df, n){
  J = dim(df)[1]
  index_species = sample(1:J, n, replace = FALSE)
  random_prop = runif(n, 0, 1)
  random_prop = random_prop/sum(random_prop)
  random_df = df[index_species,]
  random_df = cbind(proportion_alpha = random_prop, random_df)
  random_df = random_df[,c(2,1,3,4)]
  return(random_df)
}