####################################################################################################
##'
##' @title
##' Réaliser la carte d'un coefficient de spécificité globale comparant chaque zone
##' à la médiane du reste du territoire 
##'
##' @param bdd data.frame. La base de données avec en première colonne le code géographique
##' et les effectifs des différentes catégories dans les colonnes suivantes.
##' @param part boolean.  Indique si les données sont remplies en nombre (part=FALSE) ou
##' en part (part=TRUE) : dans ce cas, la somme des valeurs d'une ligne vaut 1.
##' Par défaut vaut FALSE.
##' @param carte spdf. Fond de carte à la même échelle que la base de données. Par defaut NA.
##' @param methode string. Méthode de discrétisation de la carte. Voir \link[cartography]{choroLayer}
##' pour les différentes méthodes possibles. Par défaut la méthode est "fisher-jenks".
##' @param titre character. Titre de la carte. Par défaut il est "Indice de specificite globale par rapport a la mediane".
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
##' Cette fonction affiche la carte du coefficient de spécificité globale comparant chaque zone
##' à la médiane du reste du territoire. 
##'
##'
##' @examples
##' # mettre un exemple
##'
##'
##' @return Affiche la carte du coefficient de spécificité globale comparant chaque zone
##' à la médiane du reste du territoire 
##' @importFrom cartography choroLayer carto.pal
##' @importFrom graphics plot
##' @export
##'
##'


carte_specif_mediane <- function(bdd,part=FALSE,carte=NA, codgeo1=NA,codgeo2=NA, methode="fisher-jenks", contour=TRUE,
                    titre="Indice de specificite globale \npar rapport a la mediane", n=5,cols=carto.pal(pal1 = "purple.pal", n1 = n)){
  
  if(class(bdd)[1]!="data.frame"){
    bdd <- as.data.frame(bdd)
  }
  
  res <- specificite::specif_mediane(bdd,part)
  
  if(class(carte)[1]=="sf"){
    if(is.na(codgeo1)){
      codgeo1 <- colnames(carte)[1]
    }
    if(is.na(codgeo2)){
      codgeo2 <- colnames(res)[1]
    }
    carto <- merge(carte,res,by.x=codgeo1, by.y=codgeo2,all.x=TRUE)
    choroLayer(x=carto,
               var="mediane",
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
}
