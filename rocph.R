# Tracé rapide d'une courbe ROC
#
rocph <- function(zz,titre,cutx){
  ff <- prediction(zz,tt$escarrej)
  zz <- performance(ff,"tpr","fpr")
  plot(zz, colorize = TRUE, print.cutoffs.at = cutx, main = titre)
}


