##' specificite : calculer des indices de spécificité d'une zone géographique
##'
##' Ce package a pour but de calculer différents indices permettant de connaitre la spécificité
##' d'une zone géographique par rapport au reste du territoire. Il y a des indices de spécificité
##' globale : Krugman, Gini, comparaison à la médiane et des indices de spécificité pour chaque
##' catégorie. Il est aussi possible de réaliser des cartes des différents indices.
##'
##' \tabular{ll}{
##'   Package: \tab specifite\cr
##'   Type: \tab Package\cr
##'   Version: \tab 1.0.0\cr
##'   Date: \tab 2018-07-03\cr
##'   License: \tab GPL-2\cr
##'   LazyLoad: \tab yes\cr
##' }
##'
##'
##' @name specificite-package
##' @aliases specificite
##' @rdname specificite-package
##' @docType package
##' @keywords package
##' @importFrom stats median
##' @importFrom graphics plot
##' @author Elodie Molitor
##'
##'
##'
##' @examples
##' # base de données avec une première colonne contenant les différentes zones géographiques
##' # et les autres colonnes les effectifs de chaque catégorie :
##' bdd <- data.frame(zones=c("zone1","zone2","zone3","zone4"),categorie1=c(22,14,7,55),
##' categorie2=c(32,17,12,9),categorie3=c(41,32,10,16))
##' 
##' # calcul des indices de spécificité globale :
##' krugman(bdd)
##' gini_spe(bdd)
##' specif_mediane(bdd)
##' 
##' # calcul des indices de spécificité pour chaque catégorie :
##' indice_specif(bdd)
##' 
NULL
