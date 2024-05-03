# [3] "Preoccupation_environnement" 

# [4] "Preoccupation_secu_alim"  

# [5] "Preoccupation_impacts"


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
  as_kable_extra(format="latex",
                 
  )%>%
  kableExtra::kable_styling(full_width=F,
                            position = "center"
  )%>%
  column_spec(c(1,2,3,4), width = c("6cm","3cm","4cm","5cm"))
