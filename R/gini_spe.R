###################################################################################
##'
##' @title
##' Calculer l'indice de Gini de spécificité globale
##'
##' @param bdd data.frame. La base de données avec en première colonne le code géographique
##' et les effectifs des différentes catégories dans les colonnes suivantes.
##'
##' @description
##' Cette fonction calcule l'indice de Gini de spécificité globale qui est une mesure de
##' l'inégalité d'une distribution. Il est compris entre 0 et 1.
##'
##'
##' @examples
##' bdd <- data.frame(zones=c("zone1","zone2","zone3","zone4"),categorie1=c(22,14,7,55),
##' categorie2=c(32,17,12,9),categorie3=c(41,32,10,16))
##' gini_spe(bdd)
##'
##' @return data.frame. Renvoit la base de données initiale avec une colonne supplémentaire contenant
##' l'indice de Gini de spécificité globale.
##' @importFrom reldist gini
##' @export
##'
##'

gini_spe <- function(bdd){
  
  if(class(bdd)[1]!="data.frame"){
    bdd <- as.data.frame(bdd)
  }

  BDD <- bdd[,2:ncol(bdd)]
  K <- ncol(BDD) # nb de categories
  I <- nrow(BDD) # nb de zones
  gini_specialisation <- NA
  abscisse <- NA
  ordonnee <- NA

  for ( i in 1:I) {
    for ( k in 1:K) {
      abscisse[k] <- sum(BDD[1:I,k])/sum(BDD[1:I,1:(K)]) #Xk/X poids de la catégorie k dans tout le territoire
      ordonnee[k] <- BDD[i,k]/sum(BDD[i,1:(K)]) #Xik / Xi poids de la catégorie k dans la zone i
    }
    gini_specialisation[i] <- reldist::gini((ordonnee/abscisse), weights=abscisse)
  }
  res<- cbind(bdd,gini_specialisation)
  return(res)
}

