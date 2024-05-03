# genre
BL2 <- data %>%
  select(Situation_genre) %>%
  tbl_summary(label=list(Situation_genre~"Genre"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center")


# [45] "Situation_age"  
BL2 <- data %>%
  select(Situation_age) %>%
  tbl_summary(label=list(Situation_age~"Âge"),
              statistic = list(all_continuous()~"(Min: {min}) Moy: {mean} (Max:
{max})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~0,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center")

# [46] "Situation_age_tranches" 
BL2 <- data %>%
  select(Situation_age_tranches) %>%
  tbl_summary(label=list(Situation_age_tranches~"Tranches d'âge"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center")

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
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center")

# [48] "Situation_etudes"
data$Situation_etudes <- data$Situation_etudes%>%
  fct_relevel("Aucun diplôme","Brevet des collèges","CAP ou BEP","Baccalauréat général","Baccalauréat technologique ou professionnel","Diplôme niveau bac+2","Diplôme niveau bac+3","Diplôme niveau bac+5 et plus")
BL2 <- data %>%
  select(Situation_etudes) %>%
  tbl_summary(label=list(Situation_etudes~"Niveau d'études"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center")

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
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")
# [50] "Situation_revenu" 

data$Situation_revenu <- data$Situation_revenu %>%
  fct_relevel("Moins de 800 euros","De 800 à moins de 1200 euros","De 1200 à moins de 1700 euros","De 1700 à moins de 2400 euros","De 2400 à moins de 3100 euros","De 3100 à moins de 3800 euros","De 3800 à moins de 4500 euros","De 4500 à moins de 6000 euros","De 7500 euros à plus","Vous ne souhaitez pas répondre")
BL2 <- data %>%
  select(Situation_revenu) %>%
  tbl_summary(label=list(Situation_revenu~"Niveau de revenu"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center")
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
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center")


######
data$Preoccupation_impacts <- data$Preoccupation_impacts %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
data$Preoccupation_environnement <- data$Preoccupation_environnement %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
data$Preoccupation_secu_alim <- data$Preoccupation_secu_alim %>%
  fct_relevel("Pas du tout d'accord","Plutôt pas d'accord","Plutôt d'accord","Tout à fait d'accord")
BL2 <- data %>%
  select(Preoccupation_environnement,Preoccupation_secu_alim,Preoccupation_impacts) %>%
  tbl_summary(label=list(Preoccupation_environnement ~"Je suis inquiet concernant les enjeux et risques
environnementaux (pollutions, dérèglement climatique,
sauvegarde de la biodiversité, préservation des
ressources...)",
                         Preoccupation_secu_alim~"Je me sens préoccupé par les enjeux  de sécurité
alimentaire mondiale",
                         Preoccupation_impacts~"Au quotidien, je fais attention à mes propres impacts
sur l'environnement et la société"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
BL2%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  as_kable_extra(format="latex" ,
                 longtable=T)%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center") %>%
  column_spec(1, width = "6cm")
####

BL2 <- data %>%
  select(Critere_prix,Critere_composition,Critere_qualite,Critere_informations,Critere_proximite,Critere_impact_env,Critere_respect_animaux,Critere_nouveaute,Critere_facilite_prep) %>%
  tbl_summary(label=list(Critere_prix ~"Le prix des produits",
                         Critere_composition  ~"La composition nutritionnelle des produits (glucides,
lipides, protéines, vitamines, minéraux...)",
                         Critere_qualite  ~"La qualité des produits",
                         Critere_informations ~"Les informations sur les produits (informations
disponibles sur l'étiquetage)",
                         Critere_proximite ~"La proximité des producteurs (produits locaux)",
                         Critere_impact_env ~"L'impact sur l'environnement des produits",
                         Critere_respect_animaux ~"Le respect du bien-être animal",
                         Critere_nouveaute ~"La nouveauté des produits",
                         Critere_facilite_prep ~"La facilité de préparation des produits"),
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
