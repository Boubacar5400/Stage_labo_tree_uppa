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
library(knitr)
library(kableExtra)
theme_gtsummary_language("fr",decimal.mark = ".",big.mark = "")
# Objectif 1 --------------------------------------------------------------
library(readxl)
df <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_6-15-2023_13_47.xlsx")
dim(df)
attach(df)
df <- df%>%
  filter(df$Situation_age>=15)
dim(df)
df <- df%>%
  filter(df$Situation_cp=="64000" | df$Situation_cp== "64100" | df$Situation_cp== "64110" | df$Situation_cp== "64121" | df$Situation_cp=="64140" | df$Situation_cp=="64150" | df$Situation_cp== "64160" | df$Situation_cp== "64170" | df$Situation_cp== "64230" | df$Situation_cp== "64290" | df$Situation_cp== "64320" | df$Situation_cp=="64350" | df$Situation_cp=="64360" | df$Situation_cp=="64370" | df$Situation_cp=="64400" | df$Situation_cp=="64410" | df$Situation_cp=="64420" | df$Situation_cp=="64450" | df$Situation_cp=="64460" | df$Situation_cp=="64480" | df$Situation_cp=="64510" | df$Situation_cp=="64680" | df$Situation_cp=="64800")
dim(df)
data <- df
attach(data)
# 1)
# [31] "Infos_ins"
BL2 <- data %>%
  select(Infos_ins) %>%
  tbl_summary(label=list(Infos_ins  ~"Connaissance des produits alimentaires à base d'insectes "),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**")) %>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

#  [32] "Avis_ins_impacts_env"                  
data$Avis_ins_impacts_env <- data$Avis_ins_impacts_env %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord","Ne sais pas")
# [33] "Avis_ins_secu_alim" 
data$Avis_ins_secu_alim <- data$Avis_ins_secu_alim %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord","Ne sais pas")
# [34] "Avis_ins_benefices_nutri"
data$Avis_ins_benefices_nutri <- data$Avis_ins_benefices_nutri %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord","Ne sais pas")
# [35] "Avis_ins_sante"  
data$Avis_ins_sante <- data$Avis_ins_sante %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord","Ne sais pas")
# [36] "Avis_ins_alternative"
data$Avis_ins_alternative <- data$Avis_ins_alternative %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord","Ne sais pas")

BL2 <- data %>%
  select(Avis_ins_impacts_env,Avis_ins_secu_alim,Avis_ins_benefices_nutri,Avis_ins_sante,Avis_ins_alternative) %>%
  tbl_summary(label=list(Avis_ins_impacts_env~"L'élevage d'insectes (ou entomoculture) a de faibles impacts sur l'environnement",Avis_ins_secu_alim~"La production de produits alimentaires à base d'insectes est une solution durable qui peut permettre de garantir la sécurité alimentaire mondiale",Avis_ins_benefices_nutri~"Les produits alimentaires à base d'insectes présentent d'importants bénéfices nutritionnels",Avis_ins_sante~"Les produits alimentaires à base d'insectes sont bénéfiques pour la santé",Avis_ins_alternative~"Les produits alimentaires à base d'insectes représentent une alternative satisfaisante aux protéines animales"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "10cm")
# 2) 
# [30] "Alim_ouverture" 
data$Alim_ouverture <- data$Alim_ouverture %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Alim_ouverture) %>%
  tbl_summary(label=list(Alim_ouverture  ~"Je suis ouvert à de nouvelles habitudes alimentaires"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")
# [37] "Conso_ins"
BL2 <- data %>%
  select(Conso_ins) %>%
  tbl_summary(label=list(Conso_ins~"Consommation d'insectes par le passé"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")
##############
data1 <- subset(data,data$Conso_ins=="Oui")
BL2 <- data1 %>%
  select(Conso_ins_freq) %>%
  tbl_summary(label=list(Conso_ins_freq~"Consommation d'insectes par le passé"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")
# [39] "Dispo_conso_ins_ingredients"
data$Dispo_conso_ins_ingredients <- data$Dispo_conso_ins_ingredients %>%
  fct_relevel("Pas du tout",
              "Plutôt non",
              "Plutôt oui",
              "Tout à fait")
# [40] "Dispo_conso_ins_entiers"   
data$Dispo_conso_ins_entiers <- data$Dispo_conso_ins_entiers %>%
  fct_relevel("Pas du tout",
              "Plutôt non",
              "Plutôt oui",
              "Tout à fait")
BL2 <- data %>%
  select(Dispo_conso_ins_ingredients,Dispo_conso_ins_entiers) %>%
  tbl_summary(label=list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients",Dispo_conso_ins_entiers~"Disposition à consommer des insectes comestibles préparés entiers"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "10cm")
# [41] "Dispo_conso_ins_ingredients_type"
a <- subset(data,data$Dispo_conso_ins_ingredients=="Plutôt oui" | data$Dispo_conso_ins_ingredients=="Tout à fait")
# [51] "Dispo_conso_ins_ingredients_type_Autre"
a%>%
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
a%>%
  select(d4,d5,d6) %>%
  tbl_summary(label=list(d4  ~"Sauces",d5~"Substituts de viande (steaks, saucisses...)", d6~"Produits laitiers (yaourts, fromages...)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
a%>%
  select(d7,d8,d9) %>%
  tbl_summary(label=list(d7  ~"Pâtes",d8~"Farine", d9~"Suppléments sportifs (poudre, barres
protéinées, compléments alimentaires...)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
# [52] "Dispo_conso_ins_conserv_entiers"
b <- subset(data,data$Dispo_conso_ins_ingredients=="Plutôt oui" | data$Dispo_conso_ins_entiers=="Tout à fait" | data$Dispo_conso_ins_ingredients=="Plutôt oui" | data$Dispo_conso_ins_entiers=="Tout à fait")
b%>%
  select(p1,p2,p3) %>%
  tbl_summary(label=list(p1  ~"Produits secs",p2~"Produits frais", p3~"Produits surgelés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
# [56] "Dispo_conso_ins_conserv_plats"
b%>%
  select(a1,a2,a3) %>%
  tbl_summary(label=list(a1  ~"Produits secs",a2~"Produits frais", a3~"Produits surgelés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
# [60] "Dispo_conso_ins_conserv_sauces" 
b%>%
  select(b1,b2,b3) %>%
  tbl_summary(label=list(b1  ~"Produits secs",b2~"Produits frais", b3~"Produits surgelés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
# [64] "Dispo_conso_ins_conserv_viande"
b%>%
  select(c1,c2,c3) %>%
  tbl_summary(label=list(c1  ~"Produits secs",c2~"Produits frais", c3~"Produits surgelés"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))
# 3) 
# [68] "Motivations_dispo_conso_ins"
c <- subset(data,data$Dispo_conso_ins_ingredients=="Plutôt oui" | data$Dispo_conso_ins_entiers=="Plutôt oui" | data$Dispo_conso_ins_ingredients=="Tout à fait" | data$Dispo_conso_ins_entiers=="Tout à fait")
c%>%
  select(z1,z2,z3,z4,z5,z6,z7,z8,z9,Motivations_dispo_conso_ins_Autre) %>%
  tbl_summary(label=list(z1~"Propriétés sensorielles (goût, texture, odeur.)",
                         z2~"Bénéfices nutritionnels",
                         z3~"Faible impact environnemental",
                         z4~"Conditions de production (éthique, traitement des animaux.)",
                         z5~"Anticipation des évolutions futures de notre consommation (alternative durable aux protéines animales)",
                         z6~"Prix accessibles",
                         z7~"Goût pour la nouveauté / Curiosité",
                         z8~"Défi personnel",
                         z9~"Incitation de votre entourage",
                         Motivations_dispo_conso_ins_Autre~"Autre à préciser (Proximité à ma culture, s'il n'y a plus rien à manger)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

# [70] "Freins_dispo_conso_ins"
data%>%
  select(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,Freins_dispo_conso_ins_Autre) %>%
  tbl_summary(label=list(e1  ~"Propriétés sensorielles (goût, texture, odeur...)",
                         e2~"Composition nutritionnelle",
                         e3~"Potentiel danger pour la santé",
                         e4~"Raisons médicales (allergies, intolérances...)",
                         e5~"Incompatibilité avec vos convictions ou
croyances",
                         e6~"Manque de normes encadrant la production",
                         e7~"Manque d'information sur le mode de
production (transformation, abatage, récolte)",
                         e8~"Manque d'information sur la provenance",
                         e9~"Manque d'information sur le mode de
consommation / préparation",
                         e10~"Disponibilité limitée dans les lieux de
distribution",
                         e11~"Prix élevés",
                         e12~"Aucun frein",
                         Freins_dispo_conso_ins_Autre~"Autre à préciser"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

# Objectif 2 --------------------------------------------------------------

# 1)  # [37] "Conso_ins" , [39] "Dispo_conso_ins_ingredients" , [40] "Dispo_conso_ins_entiers" 
# [84] "Situation_genre" 
data$Dispo_conso_ins_entiers <- ifelse(Dispo_conso_ins_entiers=="Plutôt oui" | Dispo_conso_ins_entiers=="Tout à fait", "Oui",Dispo_conso_ins_entiers)
data$Dispo_conso_ins_entiers <- data$Dispo_conso_ins_entiers%>%
  fct_relevel("Pas du tout","Plutôt non","Oui")
data$Dispo_conso_ins_ingredients <- ifelse(Dispo_conso_ins_ingredients=="Plutôt oui" | Dispo_conso_ins_ingredients=="Tout à fait", "Oui",Dispo_conso_ins_ingredients)
data$Dispo_conso_ins_ingredients <- data$Dispo_conso_ins_ingredients%>%
  fct_relevel("Pas du tout","Plutôt non","Oui")
tbl <- data%>%
  select(Conso_ins,Situation_genre)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes  ",
                           Situation_genre~"Genre"
  ),
  include = c(Conso_ins,Situation_genre),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_genre)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte  comme ingrédients",
                           Situation_genre~"Genre"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_genre),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

tbl <- data%>%
  select(Dispo_conso_ins_entiers,Situation_genre)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consomme insecte  comme ingrédients",
                           Situation_genre~"Genre"
  ),
  include = c(Dispo_conso_ins_entiers,Situation_genre),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

# [85] "Situation_age"  
tbl <- data%>%
  select(Conso_ins,Situation_age)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes  ",
                           Situation_age~"Âge"
  ),
  include = c(Conso_ins,Situation_age),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_age)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte  comme ingrédients",
                           Situation_age~"Genre"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_age),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1)) 

tbl <- data%>%
  select(Dispo_conso_ins_entiers,Situation_age)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consomme insecte  comme ingrédients",
                           Situation_age~"Âge"
  ),
  include = c(Dispo_conso_ins_entiers,Situation_age),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

# [86] "Situation_age_tranches"  
data$Situation_age_tranches <- ifelse(Situation_age_tranches=="60 à 74 ans" | Situation_age_tranches=="75 ans et plus", "60 ans et plus", Situation_age_tranches)
tbl <- data%>%
  select(Conso_ins,Situation_age_tranches)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes  ",
                           Situation_age_tranches~"Âge"
  ),
  include = c(Conso_ins,Situation_age_tranches),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_age_tranches)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte  comme ingrédients",
                           Situation_age_tranches~"Genre"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_age_tranches),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))  

tbl <- data%>%
  select(Dispo_conso_ins_entiers,Situation_age_tranches)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consomme insecte  comme ingrédients",
                           Situation_age_tranches~"Âge"
  ),
  include = c(Dispo_conso_ins_entiers,Situation_age_tranches),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

# [87] "Situation_nationalite" 
data$Situation_nationalite <-ifelse(Situation_nationalite=="Française" |Situation_nationalite=="française" |Situation_nationalite=="Fr" | Situation_nationalite=="Fracaise" |  Situation_nationalite=="Francaise" |Situation_nationalite=="Français"| Situation_nationalite=="FrancaiE" | Situation_nationalite=="francaise"| Situation_nationalite=="Francais" | Situation_nationalite=="francais" | Situation_nationalite=="français" | Situation_nationalite=="Francaisd" | Situation_nationalite==" FRANCAISE" | Situation_nationalite=="france" | Situation_nationalite=="FRANCE"  ,"France", "Etranger")
tbl <- data%>%
  select(Conso_ins,Situation_nationalite)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes  ",
                           Situation_nationalite~"Âge"
  ),
  include = c(Conso_ins,Situation_nationalite),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_nationalite)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte  comme ingrédients",
                           Situation_nationalite~"Genre"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_nationalite),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))  

tbl <- data%>%
  select(Dispo_conso_ins_entiers,Situation_nationalite)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consomme insecte  comme ingrédients",
                           Situation_nationalite~"Âge"
  ),
  include = c(Dispo_conso_ins_entiers,Situation_nationalite),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

# [88] "Situation_etudes" 
data$Situation_etudes <- ifelse(Situation_etudes=="Aucun diplôme" | Situation_etudes=="Brevet des collèges"| Situation_etudes=="CAP ou BEP","Avant le baccalauréat",Situation_etudes)
data$Situation_etudes <- ifelse(Situation_etudes=="Baccalauréat général" | Situation_etudes=="Baccalauréat technologique ou professionnel","Baccalauréat (général et technologique ou professionnel)",data$Situation_etudes)
data$Situation_etudes <- data$Situation_etudes %>%
  fct_relevel("Avant le baccalauréat","Baccalauréat (général et technologique ou professionnel)","Diplôme niveau bac+2","Diplôme niveau bac+3","Diplôme niveau bac+5 et plus")
tbl <- data%>%
  select(Conso_ins,Situation_etudes)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes  ",
                           Situation_etudes~"Âge"
  ),
  include = c(Conso_ins,Situation_etudes),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_etudes)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte  comme ingrédients",
                           Situation_etudes~"Genre"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_etudes),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")  

tbl <- data%>%
  select(Dispo_conso_ins_entiers,Situation_etudes)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consomme insecte  comme ingrédients",
                           Situation_etudes~"Niveau d'études"
  ),
  include = c(Dispo_conso_ins_entiers,Situation_etudes),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

# [89] "Situation_csp" 
library(readxl)
df <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/UPPA-SOFT_Z_ALT_ALIM_7-5-2023_21_11.xlsx")
View(df)
data1 <- df%>%filter(df$Situation_csp=="Artisans, commerçants et chefs d'entreprise" | df$Situation_csp=="Cadres et professions intellectuelles supérieures" | df$Situation_csp=="Professions intermédiaires" | df$Situation_csp=="Employés" | df$Situation_csp=="Autres personnes sans activité professionnelle (dont étudiants)")
attach(data1)
data1$Dispo_conso_ins_ingredients <- ifelse(Dispo_conso_ins_ingredients=="Plutôt oui" | Dispo_conso_ins_ingredients=="Tout à fait", "Oui",Dispo_conso_ins_ingredients)
data1$Dispo_conso_ins_ingredients <- data1$Dispo_conso_ins_ingredients%>%
  fct_relevel("Pas du tout","Plutôt non","Oui")
data1$Dispo_conso_ins_entiers <- ifelse(Dispo_conso_ins_entiers=="Plutôt oui" | Dispo_conso_ins_entiers=="Tout à fait", "Oui",Dispo_conso_ins_entiers)
data1$Dispo_conso_ins_entiers <- data1$Dispo_conso_ins_entiers%>%
  fct_relevel("Pas du tout","Plutôt non","Oui")
tbl <- data1%>%
  select(Conso_ins,Situation_csp)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes dans le passé ",
                           Situation_csp~"CSP"
  ),
  include = c(Conso_ins,Situation_csp),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes dans le passé**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

tbl <- data1%>%
  select(Dispo_conso_ins_ingredients,Situation_csp)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte  comme ingrédients",
                           Situation_csp~"CSP"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_csp),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))  


tbl <- data1%>%
  select(Dispo_conso_ins_entiers,Situation_csp)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consomme insecte  comme ingrédients",
                           Situation_csp~"CSP"
  ),
  include = c(Dispo_conso_ins_entiers,Situation_csp),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))


# [90] "Situation_revenu"
attach(data)
data$Situation_revenu <- ifelse(Situation_revenu=="De 2400 à moins de 3100 euros" | Situation_revenu=="De 3100 à moins de 3800 euros" | Situation_revenu=="De 3800 à moins de 4500 euros" | Situation_revenu=="De 4500 à moins de 6000 euros" | Situation_revenu=="De 7500 euros à plus","2400 euros à plus",Situation_revenu)
data$Situation_revenu <- data$Situation_revenu %>%
  fct_relevel("Moins de 800 euros","De 800 à moins de 1200 euros","De 1200 à moins de 1700 euros","De 1700 à moins de 2400 euros","2400 euros à plus","Vous ne souhaitez pas répondre")
tbl <- data%>%
  select(Conso_ins,Situation_revenu)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes  ",
                           Situation_revenu~"Niveau de revenu"
  ),
  include = c(Conso_ins,Situation_revenu),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Situation_revenu)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte  comme ingrédients",
                           Situation_revenu~"CSP"
  ),
  include = c(Dispo_conso_ins_ingredients,Situation_revenu),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))  

tbl <- data%>%
  select(Dispo_conso_ins_entiers,Situation_revenu)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consomme insecte  comme ingrédients",
                           Situation_revenu~"CSP"
  ),
  include = c(Dispo_conso_ins_entiers,Situation_revenu),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

# [91] "Siuation_situation" 
tbl <- data%>%
  select(Conso_ins,Siuation_situation)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes",
                           Siuation_situation~"Situation"
  ),
  include = c(Conso_ins,Siuation_situation),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Siuation_situation)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consomme insecte  comme ingrédients",
                           Siuation_situation~"Situation maritale"
  ),
  include = c(Dispo_conso_ins_ingredients,Siuation_situation),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm") 


tbl <- data%>%
  select(Dispo_conso_ins_entiers,Siuation_situation)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consomme insecte  comme ingrédients",
                           Siuation_situation~"CSP"
  ),
  include = c(Dispo_conso_ins_entiers,Siuation_situation),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer  des produits contenant  des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

# 2)   # [37] "Conso_ins" , [39] "Dispo_conso_ins_ingredients" , [40] "Dispo_conso_ins_entiers" 
# [23] "Lieux_achats_Autre"                    
tbl <- data%>%
  select(Conso_ins,v1,v2,v5,v7)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes",
                           v1~"Restaurants / Traiteurs",
                           v2~"Hypermarchés / Supermarchés",
                           v5~"Magasins bio",
                           v7~"Sites sur internet"
  ),
  include = c(Conso_ins,v1,v2,v5,v7),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,v1)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients",
                           v1~"Restaurants / Traiteurs"
  ),
  include = c(Dispo_conso_ins_ingredients,v1),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

tbl <- data%>%
  select(Dispo_conso_ins_entiers,v1,v4)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Disposition à consommer des produits contenant des insectes comestibles préparés entiers",
                           v1~"Restaurants / Traiteurs",
                           v4~"Commerces de proximité"
  ),
  include = c(Dispo_conso_ins_entiers,v1,v4),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

# [24] "Regime_alim" 
data$Regime_alim <- ifelse(Regime_alim=="Autre régime",Regime_alim_Autre,data$Regime_alim)
data$Regime_alim <- ifelse(data$Regime_alim=="Diabétique" | data$Regime_alim=="Régime omnivore (consommation de viande, de poisson et de fruits de mer dans votre alimentation)","Régime omnivore (consommation de viande, de poisson et de fruits de mer dans votre alimentation)",data$Regime_alim)
data$Regime_alim <- ifelse(data$Regime_alim=="Régime pesco-végétarien (exclusion de la viande de votre alimentation, tout en consommant du poisson et des fruits de mer)" | data$Regime_alim=="Régime vegan (exclusion de tout type de produit de source animale de votre alimentation mais aussi de toutes les sphères de votre vie)" | data$Regime_alim=="Régime végétalien (exclusion de tout type de produit de source animale de votre alimentation)" | data$Regime_alim=="Régime végétarien (exclusion de la viande, du poisson et des fruits de mer de votre alimentation)","Régime végétarien, pesco-végétarien, végétalien et vegan",data$Regime_alim)
tbl <- data%>%
  select(Conso_ins,Regime_alim)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes",
                           Regime_alim~"Régime alimentaire"
  ),
  include = c(Conso_ins,Regime_alim),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")


tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Regime_alim)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients",
                           Regime_alim~"Régime alimentaire"
  ),
  include = c(Dispo_conso_ins_ingredients,Regime_alim),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))


tbl <- data%>%
  select(Dispo_conso_ins_entiers,Regime_alim)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Disposition à consommer des produits contenant des insectes comestibles préparés entiers",
                           Regime_alim~"Régime alimentaire"
                           
  ),
  include = c(Dispo_conso_ins_entiers,Regime_alim),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))
# [25] "Regime_alim_Autre"  

# [26] "Alim_saine"  
data$Dispo_conso_ins_entiers <- ifelse(Dispo_conso_ins_entiers=="Plutôt oui" | Dispo_conso_ins_entiers=="Tout à fait", "Oui",Dispo_conso_ins_entiers)
data$Dispo_conso_ins_entiers <- data$Dispo_conso_ins_entiers%>%
  fct_relevel("Pas du tout","Plutôt non","Oui")
data$Dispo_conso_ins_ingredients <- ifelse(Dispo_conso_ins_ingredients=="Plutôt oui" | Dispo_conso_ins_ingredients=="Tout à fait", "Oui",Dispo_conso_ins_ingredients)
data$Dispo_conso_ins_ingredients <- data$Dispo_conso_ins_ingredients%>%
  fct_relevel("Pas du tout","Plutôt non","Oui")
tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Alim_saine)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients",
                           Alim_saine~"Régime alimentaire"
  ),
  include = c(Dispo_conso_ins_ingredients,Alim_saine),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

# [27] "Alim_fait_maison"  
tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Alim_fait_maison)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients",
                           Alim_fait_maison~"Régime alimentaire"
  ),
  include = c(Dispo_conso_ins_ingredients,Alim_fait_maison),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))
# [28] "Alim_valeurs"
data$Alim_valeurs <- data$Alim_valeurs%>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
data$Alim_croyances <- data$Alim_croyances%>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
data$Alim_ouverture <- data$Alim_ouverture%>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")

tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Alim_valeurs,Alim_croyances)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients",
                           Alim_valeurs~"Mes valeurs et principes ont une grande importance dans mon alimentation quotidienne",
                           Alim_croyances~"Mes croyances (philosophie de vie, religion, science.) influencent mon alimentation quotidienne"
                           
  ),
  include = c(Dispo_conso_ins_ingredients,Alim_valeurs,Alim_croyances),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")


tbl <- data%>%
  select(Dispo_conso_ins_entiers,Alim_valeurs,Alim_croyances)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Disposition à consommer des produits contenant des insectes comme ingrédients",
                           Alim_valeurs~"Mes valeurs et principes ont une grande importance dans mon alimentation quotidienne",
                           Alim_croyances~"Mes croyances (philosophie de vie, religion, science.) influencent mon alimentation quotidienne"
                           
  ),
  include = c(Dispo_conso_ins_entiers,Alim_valeurs,Alim_croyances),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des  insectes comestible préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

# [29] "Alim_croyances"  
tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Alim_croyances)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients",
                           Alim_croyances~"Régime alimentaire"
  ),
  include = c(Dispo_conso_ins_ingredients,Alim_croyances),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))
# [30] "Alim_ouverture"
tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Alim_ouverture)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Disposition à consommer des produits contenant des insectes comme ingrédients",
                           Alim_ouverture~"Régime alimentaire"
  ),
  include = c(Dispo_conso_ins_ingredients,Alim_ouverture),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))
data$Alim_ouverture <- data$Alim_ouverture%>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")


#########

# [6] "Critere_prix"

# [7] "Critere_composition"   

# [8] "Critere_qualite"

# [9] "Critere_informations" 

# [10] "Critere_proximite"  

#[11] "Critere_impact_env"  

#[12] "Critere_respect_animaux" 

#[13] "Critere_nouveaute"

# [14] "Critere_facilite_prep"
data$Critere_prix <- data$Critere_prix%>%
  fct_relevel("Pas du tout important","Plutôt pas important","Plutôt important","Très important")
tbl <- data%>%
  select(Conso_ins,Critere_prix)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes",
                           Critere_prix~"Le prix des produits"
                           
                           
  ),
  include = c(Conso_ins,Critere_prix),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")


data$Critere_prix <- data$Critere_prix%>%
  fct_relevel("Pas du tout important","Plutôt pas important","Plutôt important","Très important")
data$Critere_qualite <- data$Critere_qualite%>%
  fct_relevel("Pas du tout important","Plutôt pas important","Plutôt important","Très important")
data$Critere_informations <- data$Critere_informations%>%
  fct_relevel("Pas du tout important","Plutôt pas important","Plutôt important","Très important")
data$Critere_proximite <- data$Critere_proximite%>%
  fct_relevel("Pas du tout important","Plutôt pas important","Plutôt important","Très important")
data$Critere_respect_animaux <- data$Critere_respect_animaux%>%
  fct_relevel("Pas du tout important","Plutôt pas important","Plutôt important","Très important")
tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Critere_prix,Critere_qualite,Critere_informations,Critere_proximite,Critere_respect_animaux)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consommation d'insectes",
                           Critere_prix~"Le prix des produits",
                           Critere_qualite~"La qualité des produits",
                           Critere_informations~"Les informations sur les produits (informations disponibles sur l'étiquetage)",
                           Critere_proximite~"La proximité des producteurs (produits locaux)",
                           Critere_respect_animaux~"Le respect du bien-être animal"
  ),
  include = c(Dispo_conso_ins_ingredients,Critere_prix,Critere_qualite,Critere_informations,Critere_proximite,Critere_respect_animaux),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contents des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")


tbl <- data%>%
  select(Dispo_conso_ins_entiers,Critere_informations)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consommation d'insectes",
                           Critere_informations~"Les informations sur les produits (informations disponibles sur l'étiquetage)"
  ),
  include = c(Dispo_conso_ins_entiers,Critere_informations),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

# 3) # [37] "Conso_ins" , [39] "Dispo_conso_ins_ingredients" , [40] "Dispo_conso_ins_entiers" 
# [3] "Preoccupation_environnement" 

# [4] "Preoccupation_secu_alim"  

# [5] "Preoccupation_impacts"
tbl <- data%>%
  select(Conso_ins,Preoccupation_environnement,Preoccupation_secu_alim,Preoccupation_impacts)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes"
  ),
  include = c(Conso_ins,Preoccupation_environnement,Preoccupation_secu_alim,Preoccupation_impacts),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

data$Preoccupation_secu_alim <- data$Preoccupation_secu_alim%>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
tbl <- data%>%
  select(Dispo_conso_ins_ingredients,Preoccupation_secu_alim)%>%
  tbl_summary(label = list(Dispo_conso_ins_ingredients~"Consommation d'insectes",
                           Preoccupation_secu_alim~"Je me sens préoccupé par les enjeux de sécurité alimentaire mondiale"
  ),
  include = c(Dispo_conso_ins_ingredients,Preoccupation_secu_alim),
  by=Dispo_conso_ins_ingredients,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des produits contenant des insectes comme ingrédients**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")

tbl <- data%>%
  select(Dispo_conso_ins_entiers,Preoccupation_secu_alim)%>%
  tbl_summary(label = list(Dispo_conso_ins_entiers~"Consommation d'insectes",
                           Preoccupation_secu_alim~"Je me sens préoccupé par les enjeux de sécurité alimentaire mondiale"
  ),
  include = c(Dispo_conso_ins_entiers,Preoccupation_secu_alim),
  by=Dispo_conso_ins_entiers,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Disposition à consommer des insectes comestibles préparés entiers**")%>%add_p()%>%modify_header(update=list(label~"**Variables**"))%>%add_significance_stars(thresholds = c( 0.01, 0.05,0.1))

# [32] "Avis_ins_impacts_env"  

tbl <- data%>%
  select(Conso_ins,Avis_ins_impacts_env)%>%
  tbl_summary(label = list(Conso_ins~"Consommation d'insectes",
                           Avis_ins_impacts_env~"L'élevage d'insectes (ou entomoculture) a de faibles impacts sur l'environnement"
  ),
  include = c(Conso_ins,Avis_ins_impacts_env),
  by=Conso_ins,
  missing = "no",
  statistic = list(all_continuous()~"{mean}({sd})")
  ,percent="row")
tbl%>%
  modify_spanning_header(all_stat_cols()~"**Consommation d'insectes**")%>%add_p()%>%modify_header(update=list(label~"**Va