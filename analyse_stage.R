theme_gtsummary_language("fr",decimal.mark = ".",big.mark = "")
library(readxl)
library(dplyr)
library(questionr)
library(tidyverse)
library(forcats)
library(haven)
library(forcats)
library(labelled)
library(sjlabelled)
library(foreign)
library(janitor)
library(funModeling)
library(summarytools)
library(tidyr)
library(writexl)
library(gtsummary)
library(ggplot2)
library(plotrix)
library(gtsummary)
library(labelled)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(lmtest)
library(tseries)
library(smd)
inset <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_5-16-2023_10_9.xlsx")
attach(inset)
dim(inset)
# nettoyage de la base de données
# sélectionner les individus don l'age est supérieur 
# ou égal à 15
inset <- inset%>%
  filter(inset$Situation_age>=15)
dim(inset)
str(inset$Situation_cp)

# récupérer les habitant de l'agglo PAU
inset <- inset%>%
  filter(inset$Situation_cp=="64000" | inset$Situation_cp== "64100" | inset$Situation_cp== "64110" | inset$Situation_cp== "64121" | inset$Situation_cp=="64140" | inset$Situation_cp=="64150" | inset$Situation_cp== "64160" | inset$Situation_cp== "64170" | inset$Situation_cp== "64230" | inset$Situation_cp== "64290" | inset$Situation_cp== "64320" | inset$Situation_cp=="64350" | inset$Situation_cp=="64360" | inset$Situation_cp=="64370" | inset$Situation_cp=="64400" | inset$Situation_cp=="64410" | inset$Situation_cp=="64420" | inset$Situation_cp=="64450" | inset$Situation_cp=="64460" | inset$Situation_cp=="64480" | inset$Situation_cp=="64510" | inset$Situation_cp=="64680" | inset$Situation_cp=="64800")
dim(inset)

data <- inset
attach(data)
# analyse univarié
# catégorie socio prof
BL1 <- data %>%
  select(Situation_csp) %>%
  tbl_summary(label=list(Situation_csp~"Catégories Socio-proféssionnelles"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1)
 BL1%>%
  modify_header(update=list(
    label~"**Variables**"))
 # genre
 BL2 <- data %>%
   select(Situation_genre) %>%
   tbl_summary(label=list(Situation_genre~"Genre"),
               statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
               digits=all_continuous()~1)
 BL2%>%
   modify_header(update=list(
     label~"**Variables**"))
 
 BL3 <- data %>%
   select(Situation_age_tranches) %>%
   tbl_summary(label=list(Situation_age_tranches~"Tranches d'ages"),
               statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
               digits=all_continuous()~1)
               
 BL3%>%
   modify_header(update=list(
     label~"**Variables**"))
# data visualitions
 t=table(data$Situation_genre)
 p1=c( 35 , 65 )
 l=c("Homme", "Femme")
 lab=paste(l,"",p1,"%",sep=" ")
 
 pie3D(t,labels= lab,
       explode = 0.2, radius = 1, theta =.8,
       labelcex = 1, shade = 1.8,
       col=c("brown2","blue"),
       main=" ",labelcol = 1,
       cex.main=1)
 t=table(data$Situation_age_tranches)
barplot(t, cex.names = 0.5, col = 2:7) 

# Analyse univarié 
    #  [3] "Preoccupation_environnement" 
data$Preoccupation_environnement <- ifelse(Preoccupation_environnement=="Pas du tout d'accord" | Preoccupation_environnement=="Plutôt pas d'accord","Pas d'accord",data$Preoccupation_environnement)
data$Preoccupation_environnement <- data$Preoccupation_environnement %>%
                                  fct_relevel("Pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Preoccupation_environnement) %>%
  tbl_summary(label=list(Preoccupation_environnement ~"Je suis inquiet concernant les enjeux et risques
environnementaux (pollutions, dérèglement climatique,
sauvegarde de la biodiversité, préservation des
ressources...)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
                            missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d <- describe(data$Preoccupation_environnement)
d$counts[1]/nrow(data)*100
t=table(data$Preoccupation_environnement)
barplot(t,cex.names = 0.8, col=2:6, main = "Je suis inquiet concernant les enjeux et risques
environnementaux (pollutions, dérèglement climatique,
sauvegarde de la biodiversité, préservation des
ressources...)")

# [4] "Preoccupation_secu_alim"
data$Preoccupation_secu_alim <- ifelse(Preoccupation_secu_alim=="Pas du tout d'accord" | Preoccupation_secu_alim=="Plutôt pas d'accord","Pas d'accord",Preoccupation_secu_alim)
data$Preoccupation_secu_alim <- data$Preoccupation_secu_alim %>%
  fct_relevel("Pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Preoccupation_secu_alim) %>%
  tbl_summary(label=list(Preoccupation_secu_alim ~"Je me sens préoccupé par les enjeux  de sécurité
alimentaire mondiale"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d <- describe(data$Preoccupation_secu_alim)
d$counts[1]/nrow(data)*100
t=table(data$Preoccupation_secu_alim)
barplot(t,cex.names = 0.8, col=2:6, main = "Je me sens préoccupé par les enjeux de sécurité
alimentaire mondiale")
# [5] "Preoccupation_impacts" 
data$Preoccupation_impacts <- ifelse(Preoccupation_impacts=="Pas du tout d'accord" | Preoccupation_impacts=="Plutôt pas d'accord","Pas d'accord",data$Preoccupation_impacts)
data$Preoccupation_impacts <- data$Preoccupation_impacts %>%
  fct_relevel("Pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Preoccupation_impacts) %>%
  tbl_summary(label=list(Preoccupation_impacts ~"Au quotidien, je fais attention à mes propres impacts
sur l'environnement et la société"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d <- describe(data$Preoccupation_impacts)
 d$counts[1]/nrow(data)*100
t=table(data$Preoccupation_impacts)
barplot(t,cex.names = 0.8, col=2:6, main = "Au quotidien, je fais attention à mes propres impacts
sur l'environnement et la société")

# [6] "Critere_prix" 
data$Critere_prix <- ifelse(Critere_prix=="Pas du tout important" | Critere_prix=="Plutôt pas important","Pas important",data$Critere_prix)
data$Critere_prix <- data$Critere_prix %>%
  fct_relevel("Pas important","Plutôt important","Très important")

BL2 <- data %>%
  select(Critere_prix) %>%
  tbl_summary(label=list(Critere_prix ~"Le prix des produits"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d <- describe(data$Critere_prix)
d$counts[1]/nrow(data)*100
t=table(data$Critere_prix)
barplot(t,cex.names = 0.8, col=2:6, main = "Le prix des produits")

# [7] "Critere_composition" 
data$Critere_composition <- ifelse(Critere_composition=="Pas du tout important" | Critere_composition=="Plutôt pas important","Pas important",data$Critere_composition)
data$Critere_composition <- data$Critere_composition %>%
  fct_relevel("Pas important","Plutôt important","Très important")
BL2 <- data %>%
  select(Critere_composition) %>%
  tbl_summary(label=list(Critere_composition  ~"La composition nutritionnelle des produits (glucides,
lipides, protéines, vitamines, minéraux...)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d <- describe(data$Critere_composition)
d$counts[1]/nrow(data)*100
t=table(data$Critere_composition)
barplot(t,cex.names = 0.8, col=2:6, main = "La composition nutritionnelle des produits (glucides, lipides, protéines, vitamines, minéraux...)")

# [8] "Critere_qualite" 
data$Critere_qualite <- ifelse(Critere_qualite=="Pas du tout important" | Critere_qualite=="Plutôt pas important","Pas important",data$Critere_qualite)
data$Critere_qualite <- data$Critere_qualite %>%
  fct_relevel("Pas important","Plutôt important","Très important")
BL2 <- data %>%
  select(Critere_qualite) %>%
  tbl_summary(label=list(Critere_qualite  ~"La qualité des produits"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d <- describe(data$Critere_qualite)
d$counts[1]/nrow(data)*100
t=table(data$Critere_qualite)
barplot(t,cex.names = 0.8, col=2:6, main = "La qualité des produits")

# [9] "Critere_informations"

data$Critere_informations <- ifelse(Critere_informations=="Pas du tout important" | Critere_informations=="Plutôt pas important","Pas important",data$Critere_informations)
data$Critere_informations <- data$Critere_informations %>%
  fct_relevel("Pas important","Plutôt important","Très important")
BL2 <- data %>%
  select(Critere_informations) %>%
  tbl_summary(label=list(Critere_informations ~"Les informations sur les produits (informations
disponibles sur l'étiquetage)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d=describe(data$Critere_informations)
d$counts[1]/nrow(data)*100
t=table(data$Critere_informations)
barplot(t,cex.names = 0.8, col=2:6, main = "Les informations sur les produits (informations disponibles sur l'étiquetage)")

# [10] "Critere_proximite"
data$Critere_proximite <- ifelse(Critere_proximite=="Pas du tout important" | Critere_proximite=="Plutôt pas important","Pas important",data$Critere_proximite)
data$Critere_proximite <- data$Critere_proximite %>%
  fct_relevel("Pas important","Plutôt important","Très important")
BL2 <- data %>%
  select(Critere_proximite) %>%
  tbl_summary(label=list(Critere_proximite ~"La proximité des producteurs (produits locaux)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d=describe(data$Critere_proximite)
d$counts[1]/nrow(data)*100
t=table(data$Critere_proximite)
barplot(t,cex.names = 0.8, col=2:6, main = "La proximité des producteurs (produits locaux)")

# [11] "Critere_impact_env" 
data$Critere_impact_env <- ifelse(Critere_impact_env=="Pas du tout important" | Critere_impact_env=="Plutôt pas important","Pas important",data$Critere_impact_env)
data$Critere_impact_env <- data$Critere_impact_env %>%
  fct_relevel("Pas important","Plutôt important","Très important")
BL2 <- data %>%
  select(Critere_impact_env) %>%
  tbl_summary(label=list(Critere_impact_env ~"L'impact sur l'environnement des produits"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
d=describe(data$Critere_impact_env)
d$counts[1]/nrow(data)*100
t=table(data$Critere_impact_env)
barplot(t,cex.names = 0.8, col=2:6, main = "L'impact sur l'environnement des produits")

# [12] "Critere_respect_animaux"
data$Critere_respect_animaux <- ifelse(Critere_respect_animaux=="Pas du tout important" | Critere_respect_animaux=="Plutôt pas important","Pas important",data$Critere_respect_animaux)
data$Critere_respect_animaux <- data$Critere_respect_animaux %>%
  fct_relevel("Pas important","Plutôt important","Très important")
BL2 <- data %>%
  select(Critere_respect_animaux) %>%
  tbl_summary(label=list(Critere_respect_animaux ~"Le respect du bien-être animal"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
t=table(data$Critere_respect_animaux)
barplot(t,cex.names = 0.8, col=2:6, main = "Le respect du bien-être animal")

# [13] "Critere_nouveaute" 

data$Critere_nouveaute <- data$Critere_nouveaute %>%
  fct_relevel("Pas du tout important","Plutôt pas important","Plutôt important","Très important")
BL2 <- data %>%
  select(Critere_nouveaute) %>%
  tbl_summary(label=list(Critere_nouveaute ~"La nouveauté des produits"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
t=table(data$Critere_nouveaute)
barplot(t,cex.names = 0.8, col=2:6, main = "La nouveauté des produits")

# [14] "Critere_facilite_prep"
data$Critere_facilite_prep <- data$Critere_facilite_prep %>%
  fct_relevel("Pas du tout important","Plutôt pas important","Plutôt important","Très important")
BL2 <- data %>%
  select(Critere_facilite_prep) %>%
  tbl_summary(label=list(Critere_facilite_prep ~"La facilité de préparation des produits"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
t=table(data$Critere_facilite_prep)
barplot(t,cex.names = 0.8, col=2:6, main = "La facilité de préparation des produits")

# [15] "Lieux_achats"

library(readxl)
inset <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_6-15-2023_13_47.xlsx")
inset <- inset%>%
  filter(inset$Situation_age>=15)
dim(inset)
str(inset$Situation_cp)

# récupérer les habitant de l'agglo PAU
inset <- inset%>%
  filter(inset$Situation_cp=="64000" | inset$Situation_cp== "64100" | inset$Situation_cp== "64110" | inset$Situation_cp== "64121" | inset$Situation_cp=="64140" | inset$Situation_cp=="64150" | inset$Situation_cp== "64160" | inset$Situation_cp== "64170" | inset$Situation_cp== "64230" | inset$Situation_cp== "64290" | inset$Situation_cp== "64320" | inset$Situation_cp=="64350" | inset$Situation_cp=="64360" | inset$Situation_cp=="64370" | inset$Situation_cp=="64400" | inset$Situation_cp=="64410" | inset$Situation_cp=="64420" | inset$Situation_cp=="64450" | inset$Situation_cp=="64460" | inset$Situation_cp=="64480" | inset$Situation_cp=="64510" | inset$Situation_cp=="64680" | inset$Situation_cp=="64800")
dim(inset)

inset$v1 <- ifelse(is.na(inset$v1), "Non", inset$v1)
inset$v2 <- ifelse(is.na(inset$v2), "Non", inset$v2)
inset$v3 <- ifelse(is.na(inset$v3), "Non", inset$v3)
inset$v4 <- ifelse(is.na(inset$v4), "Non", inset$v4)
inset$v5 <- ifelse(is.na(inset$v5), "Non", inset$v5)
inset$v6 <- ifelse(is.na(inset$v6), "Non", inset$v6)
inset$v7 <- ifelse(is.na(inset$v7), "Non", inset$v7)

 inset%>%
  select(v1,v2,v3,v4) %>%
  tbl_summary(label=list(v1  ~"Restaurants / Traiteurs",v2~"Hypermarchés / Supermarchés", v3~"Hard-discounts (ex. Lidl, Leader price, Aldi, Netto...)",v4~"Commerces de promixité"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
 
 inset%>%
   select(v5,v6,v7) %>%
   tbl_summary(label=list(v5~"Magasins bio", v6~"Marchés, producteurs",v7~"Sites sur internet"),
               statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
               digits=all_continuous()~1,
               missing = "no")%>%
   modify_header(update=list(
     label~"**Variables**"))
# [17] "Regime_alim"                           
#[18] "Regime_alim_Autre"
data$Regime_alim <- ifelse(Regime_alim=="Autre régime",Regime_alim_Autre,data$Regime_alim)
data$Regime_alim <- ifelse(data$Regime_alim=="Diabétique" | data$Regime_alim=="Régime omnivore (consommation de viande, de poisson et de fruits de mer dans votre alimentation)","Régime omnivore (consommation de viande, de poisson et de fruits de mer dans votre alimentation)",data$Regime_alim)
data$Regime_alim <- ifelse(data$Regime_alim=="Régime pesco-végétarien (exclusion de la viande de votre alimentation, tout en consommant du poisson et des fruits de mer)" | data$Regime_alim=="Régime vegan (exclusion de tout type de produit de source animale de votre alimentation mais aussi de toutes les sphères de votre vie)" | data$Regime_alim=="Régime végétalien (exclusion de tout type de produit de source animale de votre alimentation)" | data$Regime_alim=="Régime végétarien (exclusion de la viande, du poisson et des fruits de mer de votre alimentation)","Régime végétarien, pesco-végétarien, végétalien et vegan",data$Regime_alim)
BL2 <- data %>%
  select(Regime_alim) %>%
  tbl_summary(label=list(Regime_alim  ~"votre régime alimentaire"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [19] "Alim_saine" 
data$Alim_saine <- data$Alim_saine %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Alim_saine) %>%
  tbl_summary(label=list(Alim_saine  ~"J'ai une alimentation quotidienne saine et variée
(composée de fruits, légumes, céréales et légumineuses,
protéines animales ou équivalents...)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [20] "Alim_fait_maison" 
data$Alim_fait_maison <- ifelse(Alim_fait_maison=="Pas du tout d'accord" | Alim_fait_maison=="Plutôt pas d'accord","Pas d'accord",data$Alim_fait_maison)

BL2 <- data %>%
  select(Alim_fait_maison) %>%
  tbl_summary(label=list(Alim_fait_maison  ~"Je consomme quotidiennement des plats « faits maison »"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [21] "Alim_valeurs"
data$Alim_valeurs <- ifelse(Alim_valeurs=="Pas du tout d'accord" | Alim_valeurs=="Plutôt pas d'accord","Pas d'accord",data$Alim_valeurs)
BL2 <- data %>%
  select(Alim_valeurs) %>%
  tbl_summary(label=list(Alim_valeurs  ~"Mes valeurs et principes ont une grande importance dans
mon alimentation quotidienne"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

#[22] "Alim_croyances"
data$Alim_croyances <- data$Alim_croyances %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Alim_croyances) %>%
  tbl_summary(label=list(Alim_croyances  ~"Mes croyances (philosophie de vie, religion, science...)
influencent mon alimentation quotidienne"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [23] "Alim_ouverture" 
data$Alim_ouverture <- ifelse(Alim_ouverture=="Pas du tout d'accord" | Alim_ouverture=="Plutôt pas d'accord","Pas d'accord",data$Alim_ouverture)
BL2 <- data %>%
  select(Alim_ouverture) %>%
  tbl_summary(label=list(Alim_ouverture  ~"Je suis ouvert à de nouvelles habitudes alimentaires"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [24] "Infos_ins" 
BL2 <- data %>%
  select(Infos_ins) %>%
  tbl_summary(label=list(Infos_ins  ~"Êtes-vous informé concernant les produits alimentaires à base d'insectes ?"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [25] "Avis_ins_impacts_env"
data$Avis_ins_impacts_env <- ifelse(Avis_ins_impacts_env=="Pas du tout d'accord" | Avis_ins_impacts_env=="Plutôt pas d'accord","Pas d'accord",data$Avis_ins_impacts_env)
data$Avis_ins_impacts_env <- data$Avis_ins_impacts_env %>%
  fct_relevel("Ne sais pas","Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Avis_ins_impacts_env) %>%
  tbl_summary(label=list(Avis_ins_impacts_env ~"L'élevage d'insectes (ou entomoculture) a
de faibles impacts sur l'environnement"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [26] "Avis_ins_secu_alim" 
data$Avis_ins_secu_alim <- data$Avis_ins_secu_alim %>%
  fct_relevel("Ne sais pas","Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Avis_ins_secu_alim) %>%
  tbl_summary(label=list(Avis_ins_secu_alim ~"La production de produits alimentaires à
base d'insectes est une solution durable
qui peut permettre de garantir la sécurité
alimentaire mondiale"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [27] "Avis_ins_benefices_nutri"
data$Avis_ins_benefices_nutri <- ifelse(Avis_ins_benefices_nutri=="Pas du tout d'accord" |Avis_ins_benefices_nutri=="Plutôt pas d'accord","Pas d'accord",data$Avis_ins_benefices_nutri)
BL2 <- data %>%
  select(Avis_ins_benefices_nutri) %>%
  tbl_summary(label=list(Avis_ins_benefices_nutri ~"Les produits alimentaires à base d'insectes
présentent d'importants bénéfices
nutritionnels"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [28] "Avis_ins_sante" 
data$Avis_ins_sante <- data$Avis_ins_sante %>%
  fct_relevel("Ne sais pas","Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Avis_ins_sante) %>%
  tbl_summary(label=list(Avis_ins_sante~"Les produits alimentaires à base d'insectes
sont bénéfiques pour la santé"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [29] "Avis_ins_alternative" 
data$Avis_ins_alternative <- data$Avis_ins_alternative %>%
  fct_relevel("Ne sais pas","Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Avis_ins_alternative) %>%
  tbl_summary(label=list(Avis_ins_alternative~"Les produits alimentaires à base d'insectes
représentent une alternative satisfaisante
aux protéines animales"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [30] "Conso_ins" 
BL2 <- data %>%
  select(Conso_ins) %>%
  tbl_summary(label=list(Conso_ins~"Consommation d'insectes"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [31] "Conso_ins_freq"

d1 <- subset(data,Conso_ins=="Oui")
d1$Conso_ins_freq <- d1$Conso_ins_freq %>%
  fct_relevel("Une seule fois","A des occasions exceptionnelles (soit moins d'une fois par an)","Quelques fois par an (soit moins d'une fois par trimestre)","Quelques fois par trimestre (soit une à deux fois par trimestre)")
BL2 <- d1 %>%
  select(Conso_ins_freq) %>%
  tbl_summary(label=list(Conso_ins_freq~"Fréquence de consommation"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [32] "Dispo_conso_ins_ingredients"
 data$Dispo_conso_ins_ingredients <- ifelse(Dispo_conso_ins_ingredients=="Plutôt oui" | Dispo_conso_ins_ingredients=="Tout à fait", "Oui",data$Dispo_conso_ins_ingredients)
data$Dispo_conso_ins_ingredients <- data$Dispo_conso_ins_ingredients %>%
         fct_relevel("Pas du tout","Plutôt non","Oui")
 BL2 <- data %>%
  select(Dispo_conso_ins_ingredients) %>%
  tbl_summary(label=list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
# [33] "Dispo_conso_ins_entiers"
data$Dispo_conso_ins_entiers <- ifelse(Dispo_conso_ins_entiers=="Plutôt oui" | Dispo_conso_ins_entiers=="Tout à fait", "Oui",data$Dispo_conso_ins_entiers)
data$Dispo_conso_ins_entiers <- data$Dispo_conso_ins_entiers %>%
  fct_relevel("Pas du tout","Plutôt non","Oui")
BL2 <- data %>%
  select(Dispo_conso_ins_entiers) %>%
  tbl_summary(label=list(Dispo_conso_ins_entiers~"Disposition à consommer Des insectes comestibles préparés entiers"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [34] "Dispo_conso_ins_ingredients_type" 
library(readxl)
inset <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_6-15-2023_13_47.xlsx")
inset <- inset%>%
  filter(inset$Situation_age>=15)
dim(inset)
str(inset$Situation_cp)

# récupérer les habitant de l'agglo PAU
inset <- inset%>%
  filter(inset$Situation_cp=="64000" | inset$Situation_cp== "64100" | inset$Situation_cp== "64110" | inset$Situation_cp== "64121" | inset$Situation_cp=="64140" | inset$Situation_cp=="64150" | inset$Situation_cp== "64160" | inset$Situation_cp== "64170" | inset$Situation_cp== "64230" | inset$Situation_cp== "64290" | inset$Situation_cp== "64320" | inset$Situation_cp=="64350" | inset$Situation_cp=="64360" | inset$Situation_cp=="64370" | inset$Situation_cp=="64400" | inset$Situation_cp=="64410" | inset$Situation_cp=="64420" | inset$Situation_cp=="64450" | inset$Situation_cp=="64460" | inset$Situation_cp=="64480" | inset$Situation_cp=="64510" | inset$Situation_cp=="64680" | inset$Situation_cp=="64800")
dim(inset)
inset$d1 <- ifelse(is.na(inset$d1), "Non", inset$d1)
inset$d2 <- ifelse(is.na(inset$d2), "Non", inset$d2)
inset$d3 <- ifelse(is.na(inset$d3), "Non", inset$d3)
inset$d4 <- ifelse(is.na(inset$d4), "Non", inset$d4)
inset$d5 <- ifelse(is.na(inset$d5), "Non", inset$d5)
inset$d6 <- ifelse(is.na(inset$d6), "Non", inset$d6)
inset$d7 <- ifelse(is.na(inset$d7), "Non", inset$d7)
inset$d8 <- ifelse(is.na(inset$d8), "Non", inset$d8)
inset$d9 <- ifelse(is.na(inset$d9), "Non", inset$d9)

inset%>%
  select(d1,d2,d3) %>%
  tbl_summary(label=list(d1  ~"Produits sucrés (biscuits, barres sucrées,
confiseries, produits chocolatés...)",d2~"Produits apéritifs salés (biscuits, chips,
crackers, gressins...)", d3~"Plats préparés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
inset%>%
  select(d4,d5,d6) %>%
  tbl_summary(label=list(d4  ~"Sauces",d5~"Substituts de viande (steaks, saucisses...)", d6~"Produits laitiers (yaourts, fromages...)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
inset%>%
  select(d7,d8,d9) %>%
  tbl_summary(label=list(d7  ~"Pâtes",d8~"Farine", d9~"Suppléments sportifs (poudre, barres
protéinées, compléments alimentaires...)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))

# [36] "Dispo_conso_ins_conserv_entiers" 
library(readxl)
inset <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_6-15-2023_13_47.xlsx")
inset <- inset%>%
  filter(inset$Situation_age>=15)
dim(inset)
str(inset$Situation_cp)

# récupérer les habitant de l'agglo PAU
inset <- inset%>%
  filter(inset$Situation_cp=="64000" | inset$Situation_cp== "64100" | inset$Situation_cp== "64110" | inset$Situation_cp== "64121" | inset$Situation_cp=="64140" | inset$Situation_cp=="64150" | inset$Situation_cp== "64160" | inset$Situation_cp== "64170" | inset$Situation_cp== "64230" | inset$Situation_cp== "64290" | inset$Situation_cp== "64320" | inset$Situation_cp=="64350" | inset$Situation_cp=="64360" | inset$Situation_cp=="64370" | inset$Situation_cp=="64400" | inset$Situation_cp=="64410" | inset$Situation_cp=="64420" | inset$Situation_cp=="64450" | inset$Situation_cp=="64460" | inset$Situation_cp=="64480" | inset$Situation_cp=="64510" | inset$Situation_cp=="64680" | inset$Situation_cp=="64800")
dim(inset)
inset$p1 <- ifelse(is.na(inset$p1), "Non", inset$p1)
inset$p2 <- ifelse(is.na(inset$p2), "Non", inset$p2)
inset$p3 <- ifelse(is.na(inset$p3), "Non", inset$p3)
inset%>%
  select(p1,p2,p3) %>%
  tbl_summary(label=list(p1  ~"Produits secs",p2~"Produits frais", p3~"Produits surgelés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))

# [37] "Dispo_conso_ins_conserv_plats"
library(readxl)
inset <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_6-15-2023_13_47.xlsx")
inset <- inset%>%
  filter(inset$Situation_age>=15)
dim(inset)
str(inset$Situation_cp)

# récupérer les habitant de l'agglo PAU
inset <- inset%>%
  filter(inset$Situation_cp=="64000" | inset$Situation_cp== "64100" | inset$Situation_cp== "64110" | inset$Situation_cp== "64121" | inset$Situation_cp=="64140" | inset$Situation_cp=="64150" | inset$Situation_cp== "64160" | inset$Situation_cp== "64170" | inset$Situation_cp== "64230" | inset$Situation_cp== "64290" | inset$Situation_cp== "64320" | inset$Situation_cp=="64350" | inset$Situation_cp=="64360" | inset$Situation_cp=="64370" | inset$Situation_cp=="64400" | inset$Situation_cp=="64410" | inset$Situation_cp=="64420" | inset$Situation_cp=="64450" | inset$Situation_cp=="64460" | inset$Situation_cp=="64480" | inset$Situation_cp=="64510" | inset$Situation_cp=="64680" | inset$Situation_cp=="64800")
dim(inset)
inset$a1 <- ifelse(is.na(inset$a1), "Non", inset$a1)
inset$a2 <- ifelse(is.na(inset$a2), "Non", inset$a2)
inset$a3 <- ifelse(is.na(inset$a3), "Non", inset$a3)
inset%>%
  select(a1,a2,a3) %>%
  tbl_summary(label=list(a1  ~"Produits secs",a2~"Produits frais", a3~"Produits surgelés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))

# [38] "Dispo_conso_ins_conserv_sauces"
library(readxl)
inset <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_6-15-2023_13_47.xlsx")
inset <- inset%>%
  filter(inset$Situation_age>=15)
dim(inset)
str(inset$Situation_cp)

# récupérer les habitant de l'agglo PAU
inset <- inset%>%
  filter(inset$Situation_cp=="64000" | inset$Situation_cp== "64100" | inset$Situation_cp== "64110" | inset$Situation_cp== "64121" | inset$Situation_cp=="64140" | inset$Situation_cp=="64150" | inset$Situation_cp== "64160" | inset$Situation_cp== "64170" | inset$Situation_cp== "64230" | inset$Situation_cp== "64290" | inset$Situation_cp== "64320" | inset$Situation_cp=="64350" | inset$Situation_cp=="64360" | inset$Situation_cp=="64370" | inset$Situation_cp=="64400" | inset$Situation_cp=="64410" | inset$Situation_cp=="64420" | inset$Situation_cp=="64450" | inset$Situation_cp=="64460" | inset$Situation_cp=="64480" | inset$Situation_cp=="64510" | inset$Situation_cp=="64680" | inset$Situation_cp=="64800")
dim(inset)
inset$b1 <- ifelse(is.na(inset$b1), "Non", inset$b1)
inset$b2 <- ifelse(is.na(inset$b2), "Non", inset$b2)
inset$b3 <- ifelse(is.na(inset$b3), "Non", inset$b3)
inset%>%
  select(b1,b2,b3) %>%
  tbl_summary(label=list(b1  ~"Produits secs",b2~"Produits frais", b3~"Produits surgelés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))

# [39] "Dispo_conso_ins_conserv_viande" 
library(readxl)
inset <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_6-15-2023_13_47.xlsx")
inset <- inset%>%
  filter(inset$Situation_age>=15)
dim(inset)
str(inset$Situation_cp)

# récupérer les habitant de l'agglo PAU
inset <- inset%>%
  filter(inset$Situation_cp=="64000" | inset$Situation_cp== "64100" | inset$Situation_cp== "64110" | inset$Situation_cp== "64121" | inset$Situation_cp=="64140" | inset$Situation_cp=="64150" | inset$Situation_cp== "64160" | inset$Situation_cp== "64170" | inset$Situation_cp== "64230" | inset$Situation_cp== "64290" | inset$Situation_cp== "64320" | inset$Situation_cp=="64350" | inset$Situation_cp=="64360" | inset$Situation_cp=="64370" | inset$Situation_cp=="64400" | inset$Situation_cp=="64410" | inset$Situation_cp=="64420" | inset$Situation_cp=="64450" | inset$Situation_cp=="64460" | inset$Situation_cp=="64480" | inset$Situation_cp=="64510" | inset$Situation_cp=="64680" | inset$Situation_cp=="64800")
dim(inset)
inset$c1 <- ifelse(is.na(inset$c1), "Non", inset$c1)
inset$c2 <- ifelse(is.na(inset$c2), "Non", inset$c2)
inset$c3 <- ifelse(is.na(inset$c3), "Non", inset$c3)
inset%>%
  select(c1,c2,c3) %>%
  tbl_summary(label=list(c1  ~"Produits secs",c2~"Produits frais", c3~"Produits surgelés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))

# [40] "Motivations_dispo_conso_ins"           
    
# [42] "Freins_dispo_conso_ins"                
          
# [44] "Situation_genre"  
BL2 <- data %>%
  select(Situation_genre) %>%
  tbl_summary(label=list(Situation_genre~"Genre"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [45] "Situation_age"  
BL2 <- data %>%
  select(Situation_age) %>%
  tbl_summary(label=list(Situation_age~"Âge"),
              statistic = list(all_continuous()~"moyenne: {mean} (Maximum:
{max})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [46] "Situation_age_tranches" 
data$Situation_age_tranches <- ifelse(Situation_age_tranches=="60 à 74 ans" | Situation_age_tranches=="75 ans et plus","60 ans et plus",Situation_age_tranches)
BL2 <- data %>%
  select(Situation_age_tranches) %>%
  tbl_summary(label=list(Situation_age_tranches~"Tranches d'âge"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [47] "Situation_nationalite" 
data$Situation_nationalite <-ifelse(Situation_nationalite=="Française" |Situation_nationalite=="française" |Situation_nationalite=="Fr" | Situation_nationalite=="Fracaise" |  Situation_nationalite=="Francaise" |Situation_nationalite=="Français"| Situation_nationalite=="FrancaiE" | Situation_nationalite=="francaise"| Situation_nationalite=="Francais" | Situation_nationalite=="francais" | Situation_nationalite=="français" | Situation_nationalite=="Francaisd" | Situation_nationalite==" FRANCAISE" | Situation_nationalite=="france" | Situation_nationalite=="FRANCE"  ,"France", "Etranger")
BL2 <- data %>%
  select(Situation_nationalite) %>%
  tbl_summary(label=list(Situation_nationalite~"Nationalité"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))

# [48] "Situation_etudes"
data$Situation_etudes <- ifelse(Situation_etudes=="Aucun diplôme" | Situation_etudes=="Brevet des collèges","Brevet des collèges ou Aucun diplôme",Situation_etudes)
data$Situation_etudes <- ifelse(Situation_etudes=="Baccalauréat général" | Situation_etudes=="Baccalauréat technologique ou professionnel","Baccalauréat (général et technologique ou professionnel)",data$Situation_etudes)
data$Situation_etudes <- data$Situation_etudes %>%
  fct_relevel("Brevet des collèges ou Aucun diplôme","CAP ou BEP","Baccalauréat (général et technologique ou professionnel)","Diplôme niveau bac+2","Diplôme niveau bac+3","Diplôme niveau bac+5 et plus")
BL2 <- data %>%
  select(Situation_etudes) %>%
  tbl_summary(label=list(Situation_etudes~"Niveau d'études"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
#
data$Situation_etudes <- ifelse(Situation_etudes=="Aucun diplôme" | Situation_etudes=="Brevet des collèges"| Situation_etudes=="CAP ou BEP","Avant le Baccalauréat",Situation_etudes)
data$Situation_etudes <- ifelse(Situation_etudes=="Baccalauréat général" | Situation_etudes=="Baccalauréat technologique ou professionnel","Baccalauréat (général et technologique ou professionnel)",data$Situation_etudes)
data$Situation_etudes <- data$Situation_etudes %>%
  fct_relevel("Avant le Baccalauréat","Baccalauréat (général et technologique ou professionnel)","Diplôme niveau bac+2","Diplôme niveau bac+3","Diplôme niveau bac+5 et plus")
BL2 <- data %>%
  select(Situation_etudes) %>%
  tbl_summary(label=list(Situation_etudes~"Niveau d'études"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
# [49] "Situation_csp" 
data$Situation_csp <- data$Situation_csp %>%
  fct_relevel("Agriculteurs exploitants","Artisans, commerçants et chefs d'entreprise","Cadres et professions intellectuelles supérieures","Professions intermédiaires","Employés","Ouvriers","Retraités","Autres personnes sans activité professionnelle (dont étudiants)")
BL2 <- data %>%
  select(Situation_csp) %>%
  tbl_summary(label=list(Situation_csp~"CSP"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%as_kable_extra(format = "latex")

data1 <- data%>%filter(data$Situation_csp=="Artisans, commerçants et chefs d'entreprise" | data$Situation_csp=="Cadres et professions intellectuelles supérieures" | data$Situation_csp=="Professions intermédiaires" | data$Situation_csp=="Employés" | data$Situation_csp=="Autres personnes sans activité professionnelle (dont étudiants)")
data1$Situation_csp <- data1$Situation_csp %>%
  fct_relevel("Artisans, commerçants et chefs d'entreprise","Retraités","Cadres et professions intellectuelles supérieures","Professions intermédiaires","Employés","Retraités","Autres personnes sans activité professionnelle (dont étudiants)")
BL2 <- data1 %>%
  select(Situation_csp) %>%
  tbl_summary(label=list(Situation_csp~"CSP"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
# [50] "Situation_revenu" 

data$Situation_revenu <- ifelse(Situation_revenu=="De 2400 à moins de 3100 euros" | Situation_revenu=="De 3100 à moins de 3800 euros" | Situation_revenu=="De 3800 à moins de 4500 euros" | Situation_revenu=="De 4500 à moins de 6000 euros" | Situation_revenu=="De 7500 euros à plus","2400 euros à plus",Situation_revenu)
data$Situation_revenu <- data$Situation_revenu %>%
  fct_relevel("Vous ne souhaitez pas répondre","Moins de 800 euros","De 800 à moins de 1200 euros","De 1200 à moins de 1700 euros","De 1700 à moins de 2400 euros","2400 euros à plus")
BL2 <- data %>%
  select(Situation_revenu) %>%
  tbl_summary(label=list(Situation_revenu~"Niveau de revenu"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
# [51] "Siuation_situation" 
BL2 <- data %>%
  select(Siuation_situation) %>%
  tbl_summary(label=list(Siuation_situation~"Situation matrimoniale"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))
############### Analyses bi varié ######

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_nationalite)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte comme ingrédients",Situation_nationalite~"Nationalité"
                           ),
              include = c(Dispo_conso_ins_ingredients,Situation_nationalite),
              by=Dispo_conso_ins_ingredients,
              missing = "no",
              statistic = list(all_continuous()~"{mean}({sd})")
              ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer Des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_etudes)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte comme ingrédients",Situation_etudes~"Niveau d'études"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_etudes),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer Des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer Des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

data1$Dispo_conso_ins_ingredients <- ifelse(data1$Dispo_conso_ins_ingredients=="Plutôt oui" | data1$Dispo_conso_ins_ingredients=="Tout à fait", "Oui",data1$Dispo_conso_ins_ingredients)
data1$Dispo_conso_ins_ingredients <- data1$Dispo_conso_ins_ingredients %>%
  fct_relevel("Pas du tout","Plutôt non","Oui")
tbl <- data1%>%
  select(Dispo_conso_ins_ingredients,Situation_csp)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte comme ingrédients",Situation_csp~"CSP"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_csp),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_revenu)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte comme ingrédients",Situation_revenu~"CSP"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_revenu),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Siuation_situation)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte comme ingrédients",Siuation_situation~"Situation matrimoniale"
  ),
  include = c(Dispo_conso_ins_ingredients,Siuation_situation),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer Des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

##############################
tbl <- data%>%
  select(Dispo_conso_ins_entiers,Situation_etudes)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Disposition à consommer des insectes comestibles préparés entiers",Situation_etudes~"Age"
  ),
  include = c(Dispo_conso_ins_entiers,Situation_etudes),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_entiers,Siuation_situation)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Disposition à consommer des insectes comestibles préparés entiers",Siuation_situation~"Age"
  ),
  include = c(Dispo_conso_ins_entiers,Siuation_situation),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))



BL4 <- data %>%
  select(Conso_ins) %>%
  tbl_summary(label=list(Conso_ins~"Consommation d'insectes"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL4%>%
  modify_header(update=list(
    label~"**Variables**"))

BL5 <- data %>%
  select(Situation_nationalite) %>%
  tbl_summary(label=list(Situation_nationalite~"Nationalité"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL5%>%
  modify_header(update=list(
    label~"**Variables**"))

BL6 <- data %>%
  select(Situation_etudes) %>%
  tbl_summary(label=list(Situation_etudes~"Niveau d'études"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL6%>%
  modify_header(update=list(
    label~"**Variables**"))

BL7 <- data %>%
  select(Situation_revenu) %>%
  tbl_summary(label=list(Situation_revenu~"Niveau de revenu"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL7%>%
  modify_header(update=list(
    label~"**Variables**"))

library(labelled)
look_for(data)
describe(data)
boxplot.stats(data$Situation_age)$out
library(skimr)
skim(data)
library(DataExplorer)
create_report(data)
names(data)
###################"
d$qualif.reg[d$qualif == "Ouvrier specialise"] <- "Ouvrier"
d$qualif.reg[d$qualif == "Ouvrier qualifie"] <- "Ouvrier"
d$qualif.reg[d$qualif == "Employe"] <- "Employe"
d$qualif.reg[d$qualif == "Profession intermediaire"] <- "Intermediaire"
d$qualif.reg[d$qualif == "Technicien"] <- "Intermediaire"
d$qualif.reg[d$qualif == "Cadre"] <- "Cadre"
d$qualif.reg[d$qualif == "Autre"] <- "Autre"
table(d$qualif.reg)

