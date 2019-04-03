##################################################################################
# REPRESENTATION GRAPHIQUE DE LA PUISSANCE EN FONCTION DES PARAMETRES DE L'ETUDE # 
##################################################################################

library(stats)
library(pwr)
library(lattice)
library(latticeExtra)

###################################
### Comparaison de 2 fréquences ###
######################################################################################################################################

# Fonction permettant (i) de calculer la puissance de l'étude selon les paramètres renseignés
# et (ii) de créer le data.frame qui servira de base pour le graphique de représentation des
# résultats :

PowerFreq.compute<-function( 
  Nmin, # Taille d'échantillon minimum souhaitée
  Nmax, # Taille d'échantillon maximum souhaitée
  rat, # Rapport nombre de cas/nombre de témoins (1200 cas / 800 témoins = 1.5)
  freq.mean, # Fréquence moyenne de l'outcome en population générale (0.15 pour 15%)
  Dmin, # Ecart minimum souhaité pour les fréquences de l'outcome entre les 2 groupes (Dmin != 0)
  Dmax, # Ecart maximum souhaité pour les fréquences de l'outcome entre les 2 groupes
  pix, # Nombre de subdivisions entre Nmin et Nmax. Plus "pix" est grand, meilleure sera la clareté du graphique (mais plus long sera le calcul). 100 < pix < 300 suffit a priori
  hyp, # Si hyp = 0, La fréquence moyenne de l'outcome dans le groupe des cas augmente avec l'écart entre les groupes. Sinon, elle diminue (h != 0)
  alpha, # erreur de première espèce pour le calcul de puissance
  alter # Alternative pour le calcul de puissance ("two.sided","less","greater")
){
  
  study.size<-seq(Nmin,Nmax,(Nmax-Nmin)/pix)
  
  if(rat>=1){
    N.cases=round(study.size/(1+1/rat),0)
    N.controls<-round(study.size-N.cases,0)
  }else{
    N.controls<-round(study.size/(1+1/rat),0)
    N.cases<-round(study.size-N.controls,0)}
  
  delta.freq<-seq(from=Dmin,to=Dmax,by=((Dmax-Dmin)/(length(study.size)-1)))
  
  if(hyp==0){
    P.cases<-freq.mean+delta.freq
    P.controls<-freq.mean-delta.freq
  }else{
    P.cases<-freq.mean-delta.freq
    P.controls<-freq.mean+delta.freq}
  
  h<-2*asin(sqrt(P.cases))-2*asin(sqrt(P.controls))
  
  DTset<-data.frame(N.cases,N.controls)
  
  for(j in 1:dim(DTset)[1]){
    for (i in 1:dim(DTset)[1]) { 
      DTset[i,j+2]=round(pwr.2p2n.test(h[j],n1=DTset$N.cases[i],n2=DTset$N.controls[i],
                                       sig.level=alpha,alternative=alter)$power,3)
    }
  }
  
  DTset2<-expand.grid(delta=seq(Dmin,Dmax,(Dmax-Dmin)/pix),Ntot=study.size)
  power_vec<-DTset[,3:ncol(DTset)]
  DTset2$power<-c(t(power_vec))
  
  return(DTset2)
  
}



# Fonction permettant de représenter graphiquement les résultats en sélectionnant la zone
# d'intérêt pour la discussion du choix des paramètres de l'étude :

Power.Graph<-function(
  x, # Data.frame généré avec la fonction "PowerFreq.compute"
  Xmin, # Limite inférieure sur l'axe des abscisses (Selon écart à la moyenne des 2 groupes)
  Xmax, # Limite supérieure sur l'axe des abscisses (Selon écart à la moyenne des 2 groupes)
  Ymin, # Limite inférieure sur l'axe des ordonnées (Taille d'échantillon)
  Ymax, # Limite supérieure sur l'axe des ordonnées (Taille d'échantillon)
  Tp, # Titre du graphique
  Tx, # Titre de l'axe des abscisses
  Ty # Titre de l'axe des ordonnées
){
  
  par(mar=rep(6,4))
  figure<-levelplot(power ~ delta*Ntot, 
                    data = x,
                    main = as.character(Tp),
                    xlab = as.character(Tx),
                    ylab = as.character(Ty),
                    xlim = c(Xmin,Xmax),
                    ylim = c(Ymin,Ymax),
                    bty = 2,
                    col.regions = rev(terrain.colors(100)),
                    contour = T,
                    pretty = T,
                    cuts = 10,
                    region = TRUE,
                    #labels = list(at=cut(x$power,breaks=c(seq(0.1,1,0.1))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),                    
                    labels = c(NA,"0","10","20","30","40","50","60","70","80","90","100"),
                    #labels = T,
                    drop.unused.levels = TRUE,
                    label.style = "align",
                    colorkey = list(at=as.numeric(factor(c(seq(from=0, to=1, by=.10)))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%"))
                    #arguments à utiliser pour personnaliser les axes X et Y :  
                    #scales = list(y=list(at=as.numeric(c(1200,1400,1600,1800,2000)),labels=c("720/480","840/560","960/640","1080/720","1200/800")),
                    #x=list(as.numeric(0,2,4,6,8,10),labels=c("0%","+/- 2%","+/- 4%","+/- 6%","+/- 8%","+/- 10%")))
  )
  
  print(figure)
}




##### Exemple 1

Exemple1<-PowerFreq.compute(
  Nmin = 500,
  Nmax = 1100,
  rat = 2,
  freq.mean = 0.30,
  Dmin = 0.001,
  Dmax = 0.1,
  pix = 400,
  hyp = 0,
  alpha = 0.05,
  alter = as.character("greater")
)

Power.Graph(
  x = Exemple1, 
  Xmin = 0.001, 
  Xmax = 0.1, 
  Ymin = 500, 
  Ymax =1100, 
  Tp = "Puissance de l'étude en fonction de la taille d'échantillon
  et de l'écart attendu entre les pourcentages de mortalité des 2 groupes
  (tests unilatéraux)", 
  Tx = "Ecart à la moyenne (30%) des % des 2 groupes", 
  Ty = "Taille d'échantillon (Ncas/Ntémoins = 2)"
)

##### Exemple 2

Exemple2<-PowerFreq.compute(
  Nmin=500,
  Nmax=1100,
  rat=1.5,
  freq.mean=0.30,
  Dmin=0.001,
  Dmax=0.1,
  pix=400,
  hyp=0,
  alpha=0.05,
  alter=as.character("greater")
)

Power.Graph(
  x = Exemple2, 
  Xmin = 0.001, 
  Xmax = 0.1, 
  Ymin = 500, 
  Ymax = 1100, 
  Tp = "Puissance de l'étude en fonction de la taille d'échantillon
  et de l'écart attendu entre les pourcentages de mortalité des 2 groupes
  (tests unilatéraux)", 
  Tx = "Ecart à la moyenne (30%) des % des 2 groupes (survie à 2 ans)", 
  Ty = "Taille d'échantillon (Ncas/Ntémoins = 0.5)"
)



##### Exemple 3

Exemple3<-PowerFreq.compute(
  Nmin=500,
  Nmax=1100,
  rat=1,
  freq.mean=0.30,
  Dmin=0.001,
  Dmax=0.1,
  pix=400,
  hyp=0,
  alpha=0.05,
  alter=as.character("greater")
)

Power.Graph(
  x = Exemple3, 
  Xmin = 0.001, 
  Xmax = 0.1, 
  Ymin = 500, 
  Ymax = 1100, 
  Tp = "Titre 3", 
  Tx = "Ecart à la moyenne des % des 2 groupes", 
  Ty = "Taille d'échantillon"
)


##### Exemple 4

Exemple4<-PowerFreq.compute(
  Nmin=500,
  Nmax=1100,
  rat=3,
  freq.mean=0.30,
  Dmin=0.001,
  Dmax=0.1,
  pix=400,
  hyp=0,
  alpha=0.05,
  alter=as.character("greater")
)

Power.Graph(
  x = Exemple4, 
  Xmin = 0.001, 
  Xmax = 0.1, 
  Ymin = 500, 
  Ymax = 1100, 
  Tp = "Titre 3", 
  Tx = "Ecart à la moyenne des % des 2 groupes", 
  Ty = "Taille d'échantillon"
)


##### Exemple 5

Exemple5<-PowerFreq.compute(
  Nmin=500,
  Nmax=1100,
  rat=3,
  freq.mean=0.40,
  Dmin=0.001,
  Dmax=0.1,
  pix=400,
  hyp=0,
  alpha=0.05,
  alter=as.character("greater")
)

##### Exemple 6

Exemple6<-PowerFreq.compute(
  Nmin=500,
  Nmax=1100,
  rat=1,
  freq.mean=0.40,
  Dmin=0.001,
  Dmax=0.1,
  pix=400,
  hyp=0,
  alpha=0.05,
  alter=as.character("greater")
)


######################################################################################################################################

x<-Exemple1
par(mar=rep(6,4))
levelplot(power ~ delta*Ntot, 
          data = x,
          main = "Puissance de l'étude en fonction de la taille d'échantillon
          et de l'écart attendu entre les pourcentages de mortalité des 2 groupes
          (tests unilatéraux)",
          xlab = "Ecart à la moyenne (30% à 1 an) des % de mortalité des 2 groupes",
          ylab = "Taille de l'échantillon (n[VO2 basse] / n[VO2 normal] = 2)",
          xlim = c(0.001,0.1),
          ylim = c(500,1100),
          bty = 2,
          col.regions = rev(terrain.colors(100)),
          contour = T,
          pretty = T,
          cuts = 10,
          region = TRUE,
          #labels = list(at=cut(x$power,breaks=c(seq(0.1,1,0.1))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),                    
          labels = c(NA,"0","10","20","30","40","50","60","70","80","90","100"),
          #labels = T,
          drop.unused.levels = TRUE,
          label.style = "align",
          colorkey = list(at=as.numeric(factor(c(seq(from=0, to=1, by=.10)))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),
          #arguments à utiliser pour personnaliser les axes X et Y :  
          scales = list(y=list(at=as.numeric(c(500,600,700,800,900,1000,1100)),labels=c("334/166","400/200","467/233","534/266","600/300","667/333","734/366")),
                        x=list(as.numeric(0,2,4,6,8,10),labels=c("0%","+/- 2%","+/- 4%","+/- 6%","+/- 8%","+/- 10%")))
) + 
  layer(panel.points(0.04,1000,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.04,col="black",lty=2),
        panel.abline(h=1000,col="black",lty=2),
        panel.points(0.05,700,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.05,col="black",lty=2),
        panel.abline(h=700,col="black",lty=2)
  )


##########################################################################################################################################################################

rm(x)
x<-Exemple2
par(mar=rep(6,4))
levelplot(power ~ delta*Ntot, 
          data = x,
          main = "Puissance de l'étude en fonction de la taille d'échantillon
          et de l'écart attendu entre les pourcentages de mortalité des 2 groupes
          (tests unilatéraux)",
          xlab = "Ecart à la moyenne (30% à 1 an) des % de mortalité des 2 groupes",
          ylab = "Taille de l'échantillon (n[VO2 basse] / n[VO2 normal] = 1.5)",
          xlim = c(0.001,0.1),
          ylim = c(500,1100),
          bty = 2,
          col.regions = rev(terrain.colors(100)),
          contour = T,
          pretty = T,
          cuts = 10,
          region = TRUE,
          #labels = list(at=cut(x$power,breaks=c(seq(0.1,1,0.1))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),                    
          labels = c(NA,"0","10","20","30","40","50","60","70","80","90","100"),
          #labels = T,
          drop.unused.levels = TRUE,
          label.style = "align",
          colorkey = list(at=as.numeric(factor(c(seq(from=0, to=1, by=.10)))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),
          #arguments à utiliser pour personnaliser les axes X et Y :  
          scales = list(y=list(at=as.numeric(c(500,600,700,800,900,1000,1100)),labels=c("300/200","360/240","420/280","480/320","540/360","600/400","660/440")),
                        x=list(as.numeric(0,2,4,6,8,10),labels=c("0%","+/- 2%","+/- 4%","+/- 6%","+/- 8%","+/- 10%")))
) + 
  layer(panel.points(0.04,1000,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.04,col="black",lty=2),
        panel.abline(h=1000,col="black",lty=2),
        panel.points(0.05,700,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.05,col="black",lty=2),
        panel.abline(h=700,col="black",lty=2)
  )

###################################################################################################################################################################

rm(x)

x<-Exemple3
par(mar=rep(6,4))
levelplot(power ~ delta*Ntot, 
          data = x,
          main = "Puissance de l'étude en fonction de la taille d'échantillon
          et de l'écart attendu entre les pourcentages de mortalité des 2 groupes
          (tests unilatéraux)",
          xlab = "Ecart à la moyenne (30% à 1 an) des % de mortalité des 2 groupes",
          ylab = "Taille de l'échantillon (n[VO2 basse] / n[VO2 normal] = 1)",
          xlim = c(0.001,0.1),
          ylim = c(500,1100),
          bty = 2,
          col.regions = rev(terrain.colors(100)),
          contour = T,
          pretty = T,
          cuts = 10,
          region = TRUE,
          #labels = list(at=cut(x$power,breaks=c(seq(0.1,1,0.1))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),                    
          labels = c(NA,"0","10","20","30","40","50","60","70","80","90","100"),
          #labels = T,
          drop.unused.levels = TRUE,
          label.style = "align",
          colorkey = list(at=as.numeric(factor(c(seq(from=0, to=1, by=.10)))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),
          #arguments à utiliser pour personnaliser les axes X et Y :  
          scales = list(y=list(at=as.numeric(c(500,600,700,800,900,1000,1100)),labels=c("250/250","300/300","350/350","400/400","450/450","500/500","550/550")),
                        x=list(as.numeric(0,2,4,6,8,10),labels=c("0%","+/- 2%","+/- 4%","+/- 6%","+/- 8%","+/- 10%")))
)+ 
  layer(panel.points(0.04,1000,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.04,col="black",lty=2),
        panel.abline(h=1000,col="black",lty=2),
        panel.points(0.05,700,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.05,col="black",lty=2),
        panel.abline(h=700,col="black",lty=2)
  )


###################################################################################################################################################################

rm(x)

x<-Exemple4
par(mar=rep(6,4))
levelplot(power ~ delta*Ntot, 
          data = x,
          main = "Puissance de l'étude en fonction de la taille d'échantillon
          et de l'écart attendu entre les pourcentages de mortalité des 2 groupes
          (tests unilatéraux)",
          xlab = "Ecart à la moyenne (30% à 1 an) des % de mortalité des 2 groupes",
          ylab = "Taille de l'échantillon (n[VO2 basse] / n[VO2 normal] = 3)",
          xlim = c(0.001,0.1),
          ylim = c(500,1100),
          bty = 2,
          col.regions = rev(terrain.colors(100)),
          contour = T,
          pretty = T,
          cuts = 10,
          region = TRUE,
          #labels = list(at=cut(x$power,breaks=c(seq(0.1,1,0.1))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),                    
          labels = c(NA,"0","10","20","30","40","50","60","70","80","90","100"),
          #labels = T,
          drop.unused.levels = TRUE,
          label.style = "align",
          colorkey = list(at=as.numeric(factor(c(seq(from=0, to=1, by=.10)))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),
          #arguments à utiliser pour personnaliser les axes X et Y :  
          scales = list(y=list(at=as.numeric(c(500,600,700,800,900,1000,1100)),labels=c("375/125","450/150","525/175","600/200","675/225","750/250","825/275")),
                        x=list(as.numeric(0,2,4,6,8,10),labels=c("0%","+/- 2%","+/- 4%","+/- 6%","+/- 8%","+/- 10%")))
)+ 
  layer(panel.points(0.04,1000,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.04,col="black",lty=2),
        panel.abline(h=1000,col="black",lty=2),
        panel.points(0.05,700,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.05,col="black",lty=2),
        panel.abline(h=700,col="black",lty=2)
  )


###################################################################################################################################################################

rm(x)

x<-Exemple5
par(mar=rep(6,4))
levelplot(power ~ delta*Ntot, 
          data = x,
          main = "Puissance de l'étude en fonction de la taille d'échantillon
          et de l'écart attendu entre les pourcentages de mortalité des 2 groupes
          (tests unilatéraux)",
          xlab = "Ecart à la moyenne (40% à 2 ans) des % de mortalité des 2 groupes",
          ylab = "Taille de l'échantillon (n[VO2 basse] / n[VO2 normal] = 3)",
          xlim = c(0.001,0.1),
          ylim = c(500,1100),
          bty = 2,
          col.regions = rev(terrain.colors(100)),
          contour = T,
          pretty = T,
          cuts = 10,
          region = TRUE,
          #labels = list(at=cut(x$power,breaks=c(seq(0.1,1,0.1))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),                    
          labels = c(NA,"0","10","20","30","40","50","60","70","80","90","100"),
          #labels = T,
          drop.unused.levels = TRUE,
          label.style = "align",
          colorkey = list(at=as.numeric(factor(c(seq(from=0, to=1, by=.10)))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),
          #arguments à utiliser pour personnaliser les axes X et Y :  
          scales = list(y=list(at=as.numeric(c(500,600,700,800,900,1000,1100)),labels=c("375/125","450/150","525/175","600/200","675/225","750/250","825/275")),
                        x=list(as.numeric(0,2,4,6,8,10),labels=c("0%","+/- 2%","+/- 4%","+/- 6%","+/- 8%","+/- 10%")))
)+ 
  layer(panel.points(0.05,1000,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(h=1000,col="black",lty=2),
        panel.points(0.05,700,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.05,col="black",lty=2),
        panel.abline(h=700,col="black",lty=2)
  )



###################################################################################################################################################################

rm(x)

x<-Exemple6
par(mar=rep(6,4))
levelplot(power ~ delta*Ntot, 
          data = x,
          main = "Puissance de l'étude en fonction de la taille d'échantillon
          et de l'écart attendu entre les pourcentages de mortalité des 2 groupes
          (tests unilatéraux)",
          xlab = "Ecart à la moyenne (40% à 2 ans) des % de mortalité des 2 groupes",
          ylab = "Taille de l'échantillon (n[VO2 basse] / n[VO2 normal] = 1)",
          xlim = c(0.001,0.1),
          ylim = c(500,1100),
          bty = 2,
          col.regions = rev(terrain.colors(100)),
          contour = T,
          pretty = T,
          cuts = 10,
          region = TRUE,
          #labels = list(at=cut(x$power,breaks=c(seq(0.1,1,0.1))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),                    
          labels = c(NA,"0","10","20","30","40","50","60","70","80","90","100"),
          #labels = T,
          drop.unused.levels = TRUE,
          label.style = "align",
          colorkey = list(at=as.numeric(factor(c(seq(from=0, to=1, by=.10)))),labels=c("0%","10","20","30","40","50","60","70","80","90","100%")),
          #arguments à utiliser pour personnaliser les axes X et Y :  
          scales = list(y=list(at=as.numeric(c(500,600,700,800,900,1000,1100)),labels=c("250/250","300/300","350/350","400/400","450/450","500/500","550/550")),
                        x=list(as.numeric(0,2,4,6,8,10),labels=c("0%","+/- 2%","+/- 4%","+/- 6%","+/- 8%","+/- 10%")))
)+  
  layer(panel.points(0.05,1000,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(h=1000,col="black",lty=2),
        panel.points(0.05,700,pch=10,col="red4",cex=2,lwd=2),
        panel.abline(v=0.05,col="black",lty=2),
        panel.abline(h=700,col="black",lty=2)
  )

