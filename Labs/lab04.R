##
## Lab 4
##

## ------------------------------------------------------------------------
table(smp$subst.cons, smp$abus)


## ------------------------------------------------------------------------
tab <- table(smp$subst.cons, smp$abus)
prop.table(tab)


## ------------------------------------------------------------------------
prop.table(tab, margin=1)
prop.table(tab, margin=2)


## ------------------------------------------------------------------------
xtabs(~ subst.cons + abus, data=smp)


## ------------------------------------------------------------------------
barplot(tab, beside=TRUE, legend=c("abus = 0", "abus = 1"),
        xlab="Consommation substance", ylab="Effectifs",
        col=c("cornflowerblue", "deepskyblue4"))


## ------------------------------------------------------------------------
chisq.test(tab)


## ------------------------------------------------------------------------
res <- chisq.test(tab)
res$observed
res$expected


## ------------------------------------------------------------------------
fisher.test(tab)


## ------------------------------------------------------------------------
summary(smp$age)
table(smp$subst.cons)


## ------------------------------------------------------------------------
plot(density(smp$age[smp$subst.cons == 1], na.rm=TRUE), lwd=2,
     col="orange", xlim=c(19,83), main="")
lines(density(smp$age[smp$subst.cons == 0], na.rm=TRUE), lwd=2,
      col="deepskyblue4")


## ------------------------------------------------------------------------
tapply(smp$age, smp$subst.cons, mean, na.rm=TRUE)


## ------------------------------------------------------------------------
with(smp, tapply(age, subst.cons, mean, na.rm=TRUE))


## ----ttest---------------------------------------------------------------
t.test(smp$age[smp$subst.cons == 0], smp$age[smp$subst.cons == 1])


## ------------------------------------------------------------------------
smp$subst.cons <- factor(smp$subst.cons, levels=c(0, 1), 
                         labels=c("Non", "Oui"))
table(smp$subst.cons)


## ------------------------------------------------------------------------
aggregate(age ~ subst.cons, data=smp, mean)


## ------------------------------------------------------------------------
aggregate(age ~ subst.cons, data=smp, var)


## ------------------------------------------------------------------------
boxplot(age ~ subst.cons, data=smp,
        xlab="Consommation substance", ylab="Âge (années)",
        col="cornflowerblue", border="cornflowerblue")


## ------------------------------------------------------------------------
## boxplot(age ~ subst.cons, data=smp, xlab="Consommation substance",
##         ylab="Âge (années)", col="cornflowerblue", border="cornflowerblue",
##         pars=list(medcol="white", whiskcol="cornflowerblue",
##             staplecol="cornflowerblue", whisklty=1), lwd=1.5)


## ------------------------------------------------------------------------
t.test(age ~ subst.cons, data=smp)
t.test(age ~ subst.cons, data=smp, var.equal=TRUE)


## ------------------------------------------------------------------------
library(gplots)
plotmeans(age ~ subst.cons, data=smp, col="cornflowerblue",
          barcol="cornflowerblue", pch=19,
          xlab="Consommation de substance", ylab="Age (années)")


## ------------------------------------------------------------------------
wilcox.test(age ~ subst.cons, data=smp)


## ------------------------------------------------------------------------
cor(smp$n.enfant, smp$age)
cor(smp$n.enfant, smp$age, use="pairwise")


## ------------------------------------------------------------------------
cor(smp$n.enfant, smp$age, use="pairwise", method="spearman")


## ------------------------------------------------------------------------
cor.test(smp$n.enfant, smp$age)


## ------------------------------------------------------------------------
cor.test(~ n.enfant + age, data=smp)


## ------------------------------------------------------------------------
cor.test(~ n.enfant + age, data=smp, conf.level=.90)


## ------------------------------------------------------------------------
plot(n.enfant ~ age, smp, pch=19, col=rgb(0.39,0.58,0.93,0.35),
     xlab="Âge (années)", ylab="Nombre d'enfants")
lines(lowess(n.enfant ~ age, smp), col="orange", lwd=2)  ## package gplots
grid()
