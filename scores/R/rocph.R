#' rocph
#' Tracé d'une courbe ROC simple avec l'AUC
#' @param zz valeur numerique a tester
#' @param varx variable de test, binaire
#' @param titre titre
#' @param cutx Valeurs de zz a inscrire sur la courbe
#' @import ROCR
#'
#' @return Courbe ROC
#' @export
#'
#' @examples a=1
rocph <- function(zz, varx, titre, cutx){
  ff <- prediction(zz, varx)
  zz <- performance(ff, "tpr", "fpr")
  aucz <- performance(ff, "auc")
  aucx <- round(aucz@y.values[[1]],3 )
  plot(zz, colorize = FALSE, print.cutoffs.at = cutx, main = titre, text.adj=c(1.2,0.1))
  abline(a = 0, b = 1, col = "grey", lty = 2)
  text( x= 0.8, y = 0.2, paste0("AUC = ", aucx))
}
