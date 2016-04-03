file <- "../Data/satisfaction_hopital.csv"
d <- read.csv2(file)
satis <- d

    ## recodage des variables catégorielles effectué en semaine 1
    satisf$profession.c <- factor(satisf$profession,labels=c("agriculteur","artisan","cadre","intermédiaire","employé","ouvrier","sans emploi","autre"))
satisf$service.c <- factor(satisf$service, labels=c("1","2","3","4","5","6","7","8"))
mod <- lm(score.relation~ age+sexe+score.information+amelioration.sante + amelioration.moral + profession.c + service.c, data=satisf)
drop1(mod,.~.,test="F")
summary(mod)
## Condition de validité du test
## normalité du terme de bruit (résidus du modèle)
hist(resid(mod), col="blue")
## l'histogramme montre que les termes de bruit suivent une loi normale.
## On peut donc considérer que le modèle est valide.