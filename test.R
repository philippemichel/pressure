bartabx <- function(varx, titre){
tzz <- table(varx,tt$escarrej)
tvarx <- tzz[,2]
svarx <- rowSums(tzz)
cpc <- NULL
binf <- NULL
bsup <- NULL
lg <- dim(tzz)[1]
for (ll in 1:lg) {
  bzz <- binom.test(tvarx[[ll]],svarx[[ll]])
  cpc <- c(cpc,round(bzz$estimate[[1]]*100,1))
  binf <- c(binf,round(bzz$conf.int[[1]]*100,1))
  bsup <- c(bsup,round(bzz$conf.int[[2]]*100,1))
  #lig <- paste0(pcx," [",binf,";",bsup,"]")
}
zz <- data.frame(row.names(tzz),svarx,cpc,binf,bsup)
names(zz) <- c("score","n","risque", "binf","bsup")
kable(zz, row.names = FALSE, format = "latex", booktabs = TRUE)
gg <- ggplot(zz) +
  aes(x = score, y = risque, fill = score) +
  geom_bar(stat = "identity") + 
  geom_errorbar(ymin = binf, ymax = bsup) +
  labs(title = titre,
       x = "Score",
       y = " % escarres"
  ) + 
  theme_light() + 
  theme(plot.title = element_text(size = 12, face="bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")
plot(gg)
}
