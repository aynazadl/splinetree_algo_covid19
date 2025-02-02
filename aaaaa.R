install.packages("dplyr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
install.packages("plotly")



#dashboarding & reporting
## Affichage du % d'hosp par departement dans le temps
g.per.host <- ggplot(dataf, aes(x=jour, y=per.hosp)) +
  geom_line(aes(color=dep)) + 
  xlab("jour") + ylab("percentage d'hopitalisation pour chaque 100.000 Hab")

g.per.dc <- ggplot(dataf, aes(x=jour, y=per.dc)) +
  geom_line(aes(color=dep)) + 
  xlab("jour") + ylab("percentage de d?c?s pour chaque 100.000 Hab")

g.per.rea <- ggplot(dataf, aes(x=jour, y=per.rea)) +
  geom_line(aes(color=dep)) + 
  xlab("jour") + ylab("percentage de reanimation pour chaque 100.000 Hab")

g.per.rad <- ggplot(dataf, aes(x=jour, y=per.rad)) +
  geom_line(aes(color=dep)) + 
  xlab("jour") + ylab("percentage de personne rendus ? domicile pour chaque 100.000 Hab")

a=ggplotly(g.per.host)
b=ggplotly(g.per.dc)
c=ggplotly(g.per.rea)
d=ggplotly(g.per.rad)
subplot(a,b,c,d)

## Afficher les variables par chaque departement

g.percentages <- ggplot(data = dataf, aes(x = jour))+
  geom_line(aes(y = per.dc), color="darkred")
z=ggplotly(g.percentages)
##################################################splinetree
install.packages("splinetree")
library(splinetree)
nlsySample_subset <- nlsySample[nlsySample$ID %in% sample(unique(nlsySample$ID), 500),]
splitForm <- ~HISP+WHITE+BLACK+HGC_MOTHER+HGC_FATHER+SEX+Num_sibs
tree1 <- splineTree(splitForm, BMI~AGE, 'ID', nlsySample_subset, degree=3, intercept=TRUE, cp=0.005)
stPrint(tree1)
stPlot(tree1)

split_formula <- ~dataf$hosp+dataf$per.rea+dataf$per.rad
tformula <- dataf$dc~ dataf$jour
sample_tree <- splineTree(split_formula, tformula,  idvar = "dep",
                          data = dataf, degree = 1, df = 2, 
                          intercept = FALSE, cp = 0.005)

tree1 <- splineTree(~dc+hosp+rad, 
                    rea ~ jour, "dep", dataf, degree = 1, df=2, intercept = FALSE, cp = 0.005)
Print(tree1)

idCounter <- function(x)  {
  unlist(lapply(rle(x)$lengths, seq_len))
}

dataf$TripCounter <- idCounter(dataf$jour)
############################################@
#forest 
