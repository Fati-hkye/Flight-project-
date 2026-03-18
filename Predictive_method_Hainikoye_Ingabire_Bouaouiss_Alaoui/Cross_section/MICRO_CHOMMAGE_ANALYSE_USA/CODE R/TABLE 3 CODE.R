library(dplyr)
library(readr)
library(tidyr)

# Variables explicatives à croiser avec EMPSTATUS
vars <- c("SEX_F",
          "EDUC_LEVEL",
          "MARITAL",
          "RESIDENCE",
          "AGE_GROUP",
          "MINORITY",
          "DISABLED")

# Tableau long
tab_status_long <- bind_rows(
  lapply(vars, make_block_status, data = df_multi)
)

# Tableau large : colonnes Employed / Unemployed / Inactive
tab_status_wide <- tab_status_long %>%
  pivot_wider(
    id_cols    = c(Variable, Category),
    names_from  = EMPSTATUS,
    values_from = Share_pct
  ) %>%
  arrange(Variable, Category)

tab_status_wide

# Sauvegarde pour export
write_csv(tab_status_wide, "OUTPUTS/table_status_by_group.csv")

 
