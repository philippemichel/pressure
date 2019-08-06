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
  geom_errorbar(ymin = binf, y max = bsup) +
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

for (i in 0:9){
    print(i)
    tt$seuil2 <- as.factor(ifelse(tt$sc2 > i,"haut","bas"))
    zz <- cc(tt$escarrej,tt$seuil2, graph = FALSE)
    print(zz)
}

zz <- dj[tt$alité.avant == "oui"]
zx <- dj[tt$alité.avant == "non"]
cutx=2
ff1 <- prediction(zz,tt$escarrej[tt$alité.avant == "oui"])
zz1<- performance(ff,"tpr","fpr")
plot(zz1, colorize = TRUE, print.cutoffs.at = cutx)
ff2 <- prediction(zx,tt$escarrej[tt$alité.avant == "non"])
zz2<- performance(ff2,"tpr","fpr")
plot(zz2, colorize = TRUE, print.cutoffs.at = cutx, add = TRUE)

# Socre avec le poids en numérique
scp = (ttc$alité.avant == "oui") +
  (ttc$poids_ad *0.12) +
  (ttc$corticoïdes == "oui") + 
  (ttc$déficit.neuro == "oui") 
tt$scp <- scp
tzz <- table(scp,tt$escarrej)
tscp <- tzz[,2]
sscp <- rowSums(tzz)
cpc <- NULL
binf <- NULL
bsup <- NULL
for (ll in 1:5){
  bzz <- binom.test(tscp[[ll]],sscp[[ll]])
  cpc <- c(cpc,round(bzz$estimate[[1]]*100,1))
  binf <- c(binf,round(bzz$conf.int[[1]]*100,1))
  bsup <- c(bsup,round(bzz$conf.int[[2]]*100,1))
  #lig <- paste0(pcx," [",binf,";",bsup,"]")
}
zz <- data.frame(0:4,sscp, cpc,binf,bsup)
names(zz) <- c("score","n","risque", "binf","bsup")
kable(zz, row.names = FALSE)
ggplot(zz) +
  aes(x = score, y = risque, fill = score) +
  geom_bar(stat = "identity") + 
  geom_errorbar(ymin = binf, ymax = bsup) +
  labs(title = "Score 1 (sans pondération)",
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
rocph(scp,"Score sans pondération", 0:12)
