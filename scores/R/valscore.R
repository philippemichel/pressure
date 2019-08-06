#' Pré-validation d'un score
#' Calcule le tableau du risque et des effectifs par niveau, avec son histogramme, courbe ROC avec l'AUC.
#' @param sc : score à tester
#' @param varx : variable d'outcome
#' @param titre : titre du graphique
#' @return Tableau, histogramme, courbe ROC
#' @import ggplot2
#'
#' @examples a = 2
valscore <- function(sc, varx, titre = "Score"){
  tt$sc <- sc
  tzz <- table(sc,varx)
  tsc <- tzz[,2]
  ssc <- rowSums(tzz)
  cpc <- NULL
  binf <- NULL
  bsup <- NULL
  for (lg in 1:dim(tzz)[1]){
    bzz <- binom.test(tsc[[lg]],ssc[[lg]])
    cpc <- c(cpc,round(bzz$estimate[[1]]*100,1))
    binf <- c(binf,round(bzz$conf.int[[1]]*100,1))
    bsup <- c(bsup,round(bzz$conf.int[[2]]*100,1))
  }
  zz <- data.frame(row.names(tzz),ssc,cpc,binf,bsup)
  names(zz) <- c("score","n","risque", "binf","bsup")
  kable(zz, row.names = FALSE)
  ggplot(zz) +
    aes(x = score, y = risque, fill = score) +
    geom_bar(stat = "identity") +
    geom_errorbar(ymin = binf, ymax = bsup) +
    labs(title = titre,
         x = "Score",
         y = " % "
    ) +
    theme_light() +
    theme(plot.title = element_text(size = 12, face="bold"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "none")
  #
  rocph(sc,varx, titre, min(sc, na.rm = TRUE):max(sc, na.rm = TRUE))
}
