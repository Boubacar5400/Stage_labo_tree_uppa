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
  as_kable_extra(format="latex",
                 booktabs = TRUE,
                # longtable = TRUE,
                 linesep = ""
  ) %>%
  kableExtra::kable_styling(full_width=F,
                            position = "center",
                            latex_options = c("striped", "repeat_header"),
                            stripe_color = "gray!15"
  ) %>%
  column_spec(1, width = "6cm")

BL2%>%
  modify_header(update=list(
    label~"**Variables**")) %>%
  as_kable_extra(format="latex",
                 
  ) %>%
  kableExtra::kable_styling(full_width=F,
                            position = "center"
  ) %>%
  column_spec(1, width = "6cm")
