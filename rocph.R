# Tracé rapide d'une courbe ROC
#
rocph <- function(zz,titre){
  ff <- prediction(zz,tt$escarrej)
  zz <- performance(ff,"tpr","fpr")
  plot(zz, colorize = TRUE, print.cutoffs.at = seq(40,120,10), main = titre)
}


