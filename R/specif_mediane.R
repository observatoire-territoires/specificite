###################################################################################
##'
##' @title
##' Calculer un coefficient de spécificté globale comparant chaque zone
##' à la médiane du reste du territoire
##'
##' @param bdd data.frame. La base de données avec en première colonne le code géographique
##' et les effectifs ou part des différentes catégories dans les colonnes suivantes.
##' @param part boolean.  Indique si les données sont remplies en nombre (part=FALSE) ou
##' en part (part=TRUE) : dans ce cas, la somme des valeurs d'une ligne vaut 1.
##' Par défaut vaut FALSE.
##'
##' @description
##' Cette fonction calcule un coefficient de spécificté globale comparant chaque zone
##' à la médianedu reste du territoire
##'
##'
##' @examples
##' bdd <- data.frame(zones=c("zone1","zone2","zone3","zone4"),categorie1=c(22,14,7,55),
##' categorie2=c(32,17,12,9),categorie3=c(41,32,10,16))
##' specif_mediane(bdd)
##' specif_mediane(bdd,part=TRUE)
##'
##' @return data.frame. Renvoit la base de données initiale avec une colonne supplémentaire contenant le
##' coefficient de spécificté globale comparant chaque zone à la médiane
##' @import janitor
##' @import magrittr
##' @importFrom dplyr select
##' @export
##'
##'

specif_mediane <- function(bdd,part=FALSE){
  
  if(class(bdd)[1]!="data.frame"){
    bdd <- as.data.frame(bdd)
  }

  BDD <- bdd[,2:ncol(bdd)]
  K <- ncol(BDD) # nb de categories
  I <- nrow(BDD) # nb de zones
  mediane <- NA
  # si la base est remplie en nombre
  if (!part){
    # pour pouvoir calculer la médiane il faut d'abord calculer les parts de chaque cétagorie dans chaque zone
    bdd_pct <- bdd %>%
      adorn_totals("col") %>%                  # Rajoute la colonne "Total"
      adorn_percentages(denominator="row")  # calcule %
    bdd_pct <- bdd_pct[,-ncol(bdd_pct)] # enleve la derniere colonne "Total"
    BDD <- bdd_pct[,2:ncol(bdd_pct)]
    for ( i in 1:I) {
      mediane[i] <- 0
      for ( k in 1:K) {
        mediane[i] <- mediane[i] + abs(BDD[i,k] - median(BDD[-i,k]))
      }
    }
  }
  # si la base est rempli en part/pourcentage avec X(ik)/X(i)
  else {
    for ( i in 1:I) {
      mediane[i] <- 0
      for ( k in 1:K) {
        mediane[i] <- mediane[i] + abs(BDD[i,k] - median(BDD[-i,k]))
      }
    }
  }
  res <- cbind(bdd,mediane)
  return(res)
}

