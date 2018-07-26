####################################################################################################
##'
##' @title
##' Calculer le coefficient de spécificité globale de Krugman, réaliser la carte 
##'
##' @param bdd data.frame. La base de données avec en première colonne le code géographique
##' et les effectifs des différentes catégories dans les colonnes suivantes.
##' @param carte spdf. Fond de carte à la même échelle que la base de données. Par défaut NA.
##' Si non renseigné aucune carte n'est affichée.
##' @param methode string. Méthode de discrétisation de la carte. Voir \link[cartography]{choroLayer}
##' pour les différentes méthodes possibles. Par défaut la méthode est "fisher-jenks".
##' @param titre character. Titre de la carte. Par défaut il est "Indice de Krugman".
##' @param n int. Nombre de classes à faire dans la carte. Par défaut : 5.
##' @param cols character. Les couleurs à utiliser pour la carte. Voir \link[cartography]{carto.pal}
##' pour les palettes de couleur. Par défaut : carto.pal(pal1 = "purple.pal", n1 = 5)
##' @param codgeo1 character. Nom de la variable correspondant au code géographique dans le fond de carte.
##' Par défaut vaut NA et la première colonne est considérée comme code géographique.
##' @param codgeo2 character. Nom de la variable correspondant au code géographique dans la base de données.
##' Par défaut vaut NA et la première colonne est considérée comme code géographique.
##' @param contour boolean. Indique si le contour du fond plan doit être affiché sur la carte.
##' Par défaut vaut TRUE.
##'
##'
##'
##' @description
##' Cette fonction calcule le coefficient de spécificté globale de Krugman.
##' Le coefficient de Krugman est un indice de spécificité globale d'une zone par rapport
##' au reste du territoire, il est calculé pour chaque zone. Ce coefficient est compris entre
##' 0 et 1, s'il est égal à 1 le territoire est très specifique par rapport aux autres.
##' Cette fonction peut également afficher la carte de l'indice de Krugman lorsque les variables
##' nécessaires sont données à la fonction (fond de carte à la bonne échelle au minimum).
##'
##'
##' @examples
##' bdd <- data.frame(zones=c("zone1","zone2","zone3","zone4"),categorie1=c(22,14,7,55),
##' categorie2=c(32,17,12,9),categorie3=c(41,32,10,16))
##' krugman(bdd)
##'
##'
##' @return data.frame. Renvoit la base de données initiale avec une colonne supplémentaire contenant le
##' coefficient de Krugman. 
##' Affiche seulement la carte si le fond de plan est donné.
##' @importFrom cartography choroLayer carto.pal
##' @importFrom graphics plot
##' @export
##'
##'


krugman <- function(bdd,carte=NA, codgeo1=NA,codgeo2=NA, methode="fisher-jenks", contour=TRUE,
                    titre="Indice de Krugman", n=5,cols=carto.pal(pal1 = "purple.pal", n1 = n)){
  
  if(class(bdd)[1]!="data.frame"){
    bdd <- as.data.frame(bdd)
  }

  BDD <- bdd[,2:ncol(bdd)]
  K <- ncol(BDD) # nb de categories
  I <- nrow(BDD) # nb de zones
  krugman <- NA
  for ( i in 1:I) { #pour toutes les zones
    krugman[i] <- 0
    for ( k in 1:K) { #pour chaque categorie
      krugman[i] <- krugman[i] + abs( (BDD[i,k] / sum(BDD[i,1:(K)])) - sum(BDD[-i,k]) / sum(BDD[-i,1:(K)]) )
    }
    krugman[i] <- (1/2)*krugman[i]
  }
  res <- cbind(bdd,krugman)
  if(class(carte)[1]=="sf"){
    if(is.na(codgeo1)){
      codgeo1 <- colnames(carte)[1]
    }
    if(is.na(codgeo2)){
      codgeo2 <- colnames(res)[1]
    }
    carto <- merge(carte,res,by.x=codgeo1, by.y=codgeo2,all.x=TRUE)
    choroLayer(x=carto,
               var="krugman",
               method=methode,
               nclass=n,
               col = cols,
               border = NA,
               colNA = "grey60",
               legend.pos = "topleft",
               legend.title.txt = titre,
               legend.values.rnd = 2,
               add = FALSE)
    if (contour){
      plot(carte, col=NA, add=TRUE) # warning si plusieurs variables dans carte, sinon faire select(carte,geometry)
    }
  }
  else{
    return(res)
  }
}
