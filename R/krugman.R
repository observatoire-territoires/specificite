####################################################################################################
##'
##' @title
##' Cette fonction calcule le coefficient de specificte globale de Krugman
##'
##' @param bdd data.frame. La base de donnees avec en première colonne le code geographique
##' et les effectifs des differentes categories dans les colonnes suivantes.
##' @param carte spdf. Fond de carte a la même echelle que la base de donnees. Par defaut NA.
##' Si non renseigne aucune carte n'est affichee.
##' @param methode string. Methode de discretisation de la carte. Voir \link[cartography]{choroLayer}
##' pour les differentes methodes possibles. Par defaut la methode est "fisher-jenks".
##' @param titre character. Titre de la carte. Par defaut il est "Indice de Krugman".
##' @param n int. Nombre de classes a faire dans la carte. Par defaut : 5.
##' @param cols character. Les couleurs a utiliser pour la carte. Voir \link[cartography]{carto.pal}
##' pour les palettes de couleur. Par defaut : carto.pal(pal1 = "purple.pal", n1 = 5)
##' @param codgeo1 character. Nom de la variable correspondant au code geographique dans le fond de carte
##' @param codgeo2 character. Nom de la variable correspondant au code geographique dans la base de donnees
##' @param contour boolean. Indique si le contour du fond plan doit être affiché sur la carte.
##' Par défaut vaut TRUE.
##'
##'
##'
##' @description
##' Le coefficient de Krugman est un indice de specificite globale d'une zone par rapport
##' au reste du territoire, il est calcule pour chaque zone. Ce coefficient est compris entre
##' 0 et 1, s'il est egal a 1 le territoire est très specifique par rapport aux autres.
##'
##'
##' @examples
##' bdd <- data.frame(zones=c("zone1","zone2","zone3","zone4"),categorie1=c(22,14,7,55),
##' categorie2=c(32,17,12,9),categorie3=c(41,32,10,16))
##' krugman(bdd)
##'
##'
##' @return data.frame. Renvoit la base de donnees initiale avec une colonne supplementaire contenant le
##' coefficient de Krugman.
##' @importFrom cartography choroLayer carto.pal
##' @importFrom graphics plot
##' @export
##'
##'


krugman <- function(bdd,carte=NA, codgeo1="NA",codgeo2="NA", methode="fisher-jenks", contour=TRUE,
                    titre="Indice de Krugman", n=5,cols=carto.pal(pal1 = "purple.pal", n1 = n)){

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
    carto <- merge(carte,res,by.x=codgeo1, by.y=codgeo2)
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
  return(res)
}
