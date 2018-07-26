###################################################################################
##'
##' @title
##' Réaliser la carte de l'indice de spécificité de Kubrak pour une catégorie
##'
##' @param bdd data.frame. La base de données avec en première colonne le code géographique
##' et les effectifs des différentes catégories dans les colonnes suivantes.
##' @param numCol int. Indique le numéro de colonne de la catégorie à considérer (en comptant 
##' la 1ere colone de la zone géographique).
##' @param diff boolean. Indique si l'indice de spécificité doit être calculé par différence
##' (diff=TRUE) ou par ratio (diff=FALSE). C'est-à-dire si on fait la différence par
##' rapport au reste du territoire de la part d'une zone dans une catégorie ou le ratio.
##' Par défaut il vaut FALSE.
##' @param carte spdf. Fond de carte à la même échelle que la base de données. 
##' Si non renseigne aucune carte n'est affichee.
##' @param methode string. Méthode de discrétisation de la carte. Voir \link[cartography]{choroLayer}
##' pour les différentes méthodes possibles. Par défaut la méthode est "fisher-jenks".
##' @param titre character. Titre de la carte. Par défaut, il est "Indice de specicite de " suivi du nom de
##' la catégorie considérée.
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
##' @description
##' Cette fonction réalise la carte de l'indice de spécificté de Kubrak pour une catégorie.
##' L'indice de spécificté de Kubrak est calculé pour chaque zone et la catégorie demandée.
##' r(ik) = X(ik)/X(k)  / ( (X(k) - X(ik)) / (X-X(i)) ) (par ratio)
##' r(ik) = X(ik)/X(k)  - ( (X(k) - X(ik)) / (X-X(i)) ) (par différence)
##' où i correspond à la zone et k à la catégorie
##' 
##'
##' @examples
##' # mettre un exemple
##'
##' @return Affiche la carte de l'indice de spécificité de Kubrak calculé pour une catégorie.
##' @importFrom cartography choroLayer carto.pal
##' @importFrom graphics plot
##' @export
##'
##'
####################################################################################################
carte_indice_specif <- function(bdd,numCol,diff=FALSE,carte, codgeo1=NA,codgeo2=NA, methode="fisher-jenks", contour=TRUE,
                                titre=NA, n=5,cols=carto.pal(pal1 = "purple.pal", n1 = n)){
  
  if(class(bdd)[1]!="data.frame"){
    bdd <- as.data.frame(bdd)
  }
  
  if (is.na(titre)){
    titre <- paste0("Indice de specificite de ",colnames(bdd)[numCol])
  }
  BDD <- bdd[,2:ncol(bdd)]
  K <- ncol(BDD) # nb de categories
  I <- nrow(BDD) # nb de zones
  indice_specif_var <- numeric(I)
  k <- numCol-1
  # par ratio
  if (!diff){
    for ( i in 1:I) {
      indice_specif_var[i] <- (BDD[i,k]/sum(BDD[i,1:(K)])) / ((sum(BDD[1:I,k])-BDD[i,k]) / (sum(BDD[1:I,1:(K)])-sum(BDD[i,1:(K)])))
    }
  }
  # par différence
  else {
    for ( i in 1:I) {
      indice_specif_var[i] <- (BDD[i,k]/sum(BDD[i,1:(K)])) - ((sum(BDD[1:I,k])-BDD[i,k]) / (sum(BDD[1:I,1:(K)])-sum(BDD[i,1:(K)])))
    }
  }
  res <- cbind(bdd,indice_specif_var)
  if(class(carte)[1]=="sf"){
    if(is.na(codgeo1)){
      codgeo1 <- colnames(carte)[1]
    }
    if(is.na(codgeo2)){
      codgeo2 <- colnames(res)[1]
    }
    carto <- merge(carte,res,by.x=codgeo1, by.y=codgeo2,all.x=TRUE)
    choroLayer(x=carto,
               var="indice_specif_var",
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

