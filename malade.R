data<-read.csv("data.csv")
# I- Netooyage des donnes
# 1- Modifier les noms des colones
head(data) # les noms des colones ils sont nommer V1,V2...
colnames(data)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","target")
head(data) # Oki pour les noms des colones
# 2- pretraitement de la colone cible
print(data$target) # erreur trouver (2,3,4) alors reellemt on 0 ou 1 donc (2,3,4) on change par 1
data$target[data$target %in% c(2,3,4)]<-1
# 3- traitement valeur manquant(NA,?)
# a-) voir la somme de ligne du valeur manquant
library(dplyr)
data<-data%>%
  filter_all(all_vars(.!="?"))# Oki pour les valeurs manquant
# II- Recoder le type des variables(quantitaive et qualitative)
# 1- Qualitatives(tous en factor)
data$sex=as.factor(data$sex)
data$cp=as.factor(data$cp)
data$fbs=as.factor(data$fbs)
data$restecg=as.factor(data$restecg)
data$exang=as.factor(data$exang)
data$slope=as.factor(data$slope)
data$thal=as.factor(data$thal)
data$ca=as.factor(data$ca)
data$target=as.factor(data$target)
# 2- Quantitative (Integer) sauf les colone avec virgule
data$age=as.integer(data$age)
data$trestbps=as.integer(data$trestbps)
data$chol=as.integer(data$chol)
# 3- Recoder les  variable qualitative
levels(data$sex)<-c("Femme","Homme")
levels(data$cp)<-c("Angine de poitrines stable","Angine de poitrines instable","Douleur non angineuse","Asymtomatique")
levels(data$fbs)<-c("Non","Oui")
levels(data$restecg) <- c("Normal", "Anomalies", "Hypertrophie")
levels(data$exang) <- c("Non", "Oui")
levels(data$slope) <- c("En hausse", "Stable", "En baisse")
levels(data$ca) <- c("Absence d'anomalie", "Faible", "Moyen", "Eleve")
levels(data$thal) <- c("Non", "Thalassemie sous controle", "Thalassemie instable")
levels(data$target) <- c("Non", "Oui")
# 4- verifier si il y ' a des colone manquant
apply(data, 2, anyNA) # Oki

# III- Test statistique
# 1- calculer le pourcentage pour les variable quatitative
table(data$sex) # effectif
prop.table(table(data$sex)) # frequence
round(prop.table(table(data$sex)),4) # frequence arrondi
round(prop.table(table(data$sex)),4)*100
# Variable cp
table(data$cp)
round(prop.table(table(data$cp)), 4)*100

# Variable fbs
table(data$fbs)
round(prop.table(table(data$fbs)), 4)*100

# Variable restecg
table(data$restecg)
round(prop.table(table(data$restecg)), 4)*100

# Variable exang
table(data$exang)
round(prop.table(table(data$exang)), 4)*100

# Variable slope
table(data$slope)
round(prop.table(table(data$slope)), 4)*100

# Variable ca
table(data$ca)
round(prop.table(table(data$ca)), 4)*100

# Variable thal
table(data$thal)
round(prop.table(table(data$thal)), 4)*100

# Variable target
table(data$target)
round(prop.table(table(data$target)), 4)*100




#---------------------------------------------------------------#
#                    DEBUT du Tests statistiques
#---------------------------------------------------------------#

## Calcul des pourcentages
round(prop.table(table(data$sex, data$target), margin = 1), 4)*100
round(prop.table(table(data$cp, data$target), margin = 1), 4)*100
round(prop.table(table(data$fbs, data$target), margin = 1), 4)*100
round(prop.table(table(data$restecg, data$target), margin = 1), 4)*100
round(prop.table(table(data$exang, data$target), margin = 1), 4)*100
round(prop.table(table(data$slope, data$target), margin = 1), 4)*100
round(prop.table(table(data$ca, data$target), margin = 1), 4)*100
round(prop.table(table(data$thal, data$target), margin = 1), 4)*100




## Minimum, quartiles, mediane, moyenne et maximum
summary(data$age)
summary(data$trestbps)
summary(data$chol)
summary(data$thalach)
summary(data$oldpeak)

## Variance et ecart-type
var(data$age)
sd(data$age)
var(data$trestbps)
sd(data$trestbps)
var(data$chol)
sd(data$chol)
var(data$thalach)
sd(data$thalach)
var(data$oldpeak)
sd(data$oldpeak)

#---------------------------------------------------------------#
#                     Tests Khi2(variables qualitatives)
#---------------------------------------------------------------#

## H0 : Les deux variables sont independantes (si p-value > 0,05)
## H1 : Les deux variables sont dependantes (si p-value < 0,05)
chisq.test(data$sex, data$target)#H1
chisq.test(data$cp, data$target)#H1
chisq.test(data$fbs, data$target)#H0
chisq.test(data$restecg, data$target)#H1
chisq.test(data$exang, data$target)#H1
chisq.test(data$slope, data$target)#H1
chisq.test(data$ca, data$target)#H1
chisq.test(data$thal, data$target)#H1

## Calcul des moyennes
tapply(data$age, data$target, mean)
tapply(data$trestbps, data$target, mean)
tapply(data$chol, data$target, mean)
tapply(data$thalach, data$target, mean)
tapply(data$oldpeak, data$target, mean)


## Test de Shapiro-Wilk(Variable Quantitative)
### H0 : L'echantillon suit une distribution normale (si p-value > 0,05)
### H1 : L'echantillon ne suit pas une distribution normale (si p-value < 0,05)
shapiro.test(filter(data,target=="Oui")$age) # H1
shapiro.test(filter(data, target == "Oui")$trestbps) # H1
shapiro.test(filter(data, target == "Oui")$chol) # H0
shapiro.test(filter(data, target == "Oui")$thalach) # H0
shapiro.test(filter(data, target == "Oui")$oldpeak) # H1


## Test de Mann-Whitney( qui ne suit pas une distribution normal)
### H0 : Il n'y a pas de difference significative entre la moyenne des deux variables (si p-value > 0,05)
### H1 : Il y a une difference significative entre la moyenne des deux variables (si p-value < 0,05)
wilcox.test(data$age~data$target)
wilcox.test(data$trestbps~data$target)
wilcox.test(data$oldpeak~data$target)
## Test de Student(qui suit une distribution normal)
### H0 : Il n'y a pas de difference significative entre la moyenne des deux variables (si p-value > 0,05)
### H1 : Il y a une difference significative entre la moyenne des deux variables (si p-value < 0,05)
t.test(data$chol~data$target)
t.test(data$thalach~data$target)


#---------------------------------------------------------------#
#                    FIN du Tests statistiques
#---------------------------------------------------------------#


#---------------------------------------------------------------
#                     DEBUT Machine Learning
#---------------------------------------------------------------


# 1. Division des donnees
set.seed(99)
library(caTools)
donne_repartition<-sample.split(data$target,SplitRatio = 0.8)
donne_train=subset(data,donne_repartition==TRUE)
donne_test<-subset(data,donne_repartition==FALSE)

# 2. creation du modele
RegressionLogistique<-glm(target~.,data = donne_train,family = "binomial")
summary(RegressionLogistique)

# 3. Optimisation du modele de regression logistique pour ne conserver que les variables significatives

RegressionLogistique<-update(RegressionLogistique,.~.-restecg)
RegressionLogistique<-update(RegressionLogistique,.~.-slope)
RegressionLogistique<-update(RegressionLogistique,.~.-thal)
RegressionLogistique<-update(RegressionLogistique,.~.-age)
RegressionLogistique<-update(RegressionLogistique,.~.-fbs)
RegressionLogistique<-update(RegressionLogistique,.~.-chol)
RegressionLogistique<-update(RegressionLogistique,.~.-thalach)
#RegressionLogistique<-update(RegressionLogistique,.~.-cp)
RegressionLogistique<-update(RegressionLogistique,.~.-trestbps)

# 4. prediction

prediction<-predict(RegressionLogistique,donne_test,type="response")
prediction
tableau_prediction<-as.data.frame(prediction)

fonction_tableau_prediction<-function(x){
  ifelse(x>0.5,1,0)
}
tableau_prediction<-apply(tableau_prediction,2,fonction_tableau_prediction)

# 5. Mesure des performace du modele

library(performance)
library(caret)
levels(donne_test$target)
levels(donne_test$target)<-c(0,1)
confusionMatrix(as.factor(donne_test$target),as.factor(tableau_prediction))

# 6. Tableau de comparaison

tableau_comparaison<-cbind(donne_test,tableau_prediction)
tableau_comparaison$prediction<-as.factor(tableau_comparaison$prediction)
levels(tableau_comparaison$target)<-c("Non","Oui")
levels(tableau_comparaison$prediction)<-c("Non","Oui")



# 6. Test de Hosmer et Lemeshow
## H0 : L'ajustement du modele aux donnees est bon (si p-value > 0.05)
## H1 : L'ajustement du modele aux donnees est mauvais (si p-value < 0.05)
library(performance)
performance_hosmer(RegressionLogistique)










