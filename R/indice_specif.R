###################################################################################
##'
##' @title
##' Calculer l'indice de spécificté de Kubrak
##'
##' @param bdd data.frame. La base de données avec en première colonne le code géographique
##' et les effectifs des différentes catégories dans les colonnes suivantes.
##' @param diff boolean. Indique si l'indice de spécificité doit être calculé par différence
##' (diff=TRUE) ou par ratio (diff=FALSE). C'est-à-dire si on fait la différence par
##' rapport au reste du territoire de la part d'une zone dans une catégorie ou le ratio.
##' Par défaut il vaut FALSE.
##'
##'
##' @description
##' Cette fonction calcule l'indice de spécificté de Kubrak
##' L'indice de spécificté de Kubrak est calculé pour chaque zone et chaque catégorie.
##' r(ik) = X(ik)/X(k)  / ( (X(k) - X(ik)) / (X-X(i)) ) (par ratio)
##' r(ik) = X(ik)/X(k)  - ( (X(k) - X(ik)) / (X-X(i)) ) (par différence)
##'
##' @examples
##' bdd <- data.frame(zones=c("zone1","zone2","zone3","zone4"),categorie1=c(22,14,7,55),
##' categorie2=c(32,17,12,9),categorie3=c(41,32,10,16))
##' indice_specif(bdd)
##' indice_specif(bdd,diff=TRUE)
##'
##'
##' @return data.frame. La base de données avec autant de colonnes supplémentaires que de catégories
##' et contenant l'indice de spécificité de cette catégorie pour une zone géographique par rapport au
##' reste du territoire. Les nouvelles colonnes se nomment "indic_specif_" suivi du nom de la
##' variable initiale.
##'
##' @export
##'
##'
####################################################################################################
indice_specif <- function(bdd,diff=FALSE){
  
  if(class(bdd)[1]!="data.frame"){
    bdd <- as.data.frame(bdd)
  }

  BDD <- bdd[,2:ncol(bdd)]
  K <- ncol(BDD) # nb de categories
  I <- nrow(BDD) # nb de zones
  indice_specificite <- matrix(nrow=I, ncol=K)
  variables <- colnames(BDD)
  # par ratio
  if (!diff){
    for ( i in 1:I) {
      for ( k in 1:K) {
        indice_specificite[i,k] <-  (BDD[i,k]/sum(BDD[i,1:(K)])) / ((sum(BDD[1:I,k])-BDD[i,k]) / (sum(BDD[1:I,1:(K)])-sum(BDD[i,1:(K)])))
      }
    }
  }
  # par différence
  else {
    for ( i in 1:I) {
      for ( k in 1:K) {
        indice_specificite[i,k] <-  (BDD[i,k]/sum(BDD[i,1:(K)])) - ((sum(BDD[1:I,k])-BDD[i,k]) / (sum(BDD[1:I,1:(K)])-sum(BDD[i,1:(K)])))
      }
    }
  }

  colnames(indice_specificite) <- paste0("indice_specif_",variables)
  res <- cbind(bdd,indice_specificite)
  return(res)
}

