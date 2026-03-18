# ===============================
# Table 1 – Descriptive statistics by labour-market status
# ===============================
library(dplyr)
library(readr)
library(tidyr)

# 1) Charger la base multinomiale
df_multi <- read_csv("DATA_CLEAN/cps_2023_multinomial_base.csv")

# 2) Fonction générique pour faire un bloc de lignes
make_block <- function(df,
                       var_name,
                       group_label,
                       ref_level = NULL,
                       label_map = NULL) {
  
  v_raw <- df[[var_name]]
  
  # Remappage éventuel des labels (ex : 0/1 -> Majority/Minority)
  if (!is.null(label_map)) {
    v_char <- as.character(v_raw)
    v_lab  <- dplyr::recode(v_char, !!!label_map)
    v <- factor(v_lab)
  } else {
    v <- if (is.factor(v_raw)) v_raw else factor(v_raw)
  }
  
  # Mettre la catégorie de référence en premier + étoile dans le texte
  if (!is.null(ref_level) && ref_level %in% levels(v)) {
    v <- stats::relevel(v, ref = ref_level)
  }
  
  df_tmp <- df %>%
    mutate(.v = v) %>%
    group_by(.v, EMPSTATUS) %>%
    summarise(w = sum(WTFINL, na.rm = TRUE), .groups = "drop") %>%
    group_by(EMPSTATUS) %>%
    mutate(perc = 100 * w / sum(w)) %>%
    select(.v, EMPSTATUS, perc) %>%
    tidyr::pivot_wider(
      names_from  = EMPSTATUS,
      values_from = perc
    ) %>%
    ungroup() %>%
    # remettre l'ordre des niveaux tel qu'il est dans v
    mutate(.v = factor(.v, levels = levels(v))) %>%
    arrange(.v) %>%
    # ajouter les colonnes Group/Category comme dans le papier
    mutate(
      Group    = group_label,
      Category = as.character(.v),
      Category = ifelse(
        !is.null(ref_level) & Category == ref_level,
        paste0(Category, "*"),
        Category
      )
    ) %>%
    select(Group, Category, Employed, Unemployed, Inactive) %>%
    # remplacer les NA par 0 si une case est vide
    replace_na(list(Employed = 0, Unemployed = 0, Inactive = 0))
  
  df_tmp
}

# 3) Construire chacun des blocs (comme dans Stijepic)

block_sex <- make_block(
  df_multi,
  var_name   = "SEX_F",
  group_label = "Sex",
  ref_level   = "Male"
)

block_educ <- make_block(
  df_multi,
  var_name   = "EDUC_LEVEL",
  group_label = "Education level",
  ref_level   = "None"
)

block_marital <- make_block(
  df_multi,
  var_name   = "MARITAL",
  group_label = "Marital status",
  ref_level   = "Single"
)

block_residence <- make_block(
  df_multi,
  var_name   = "RESIDENCE",
  group_label = "Place of residence",
  ref_level   = "Urban"
)

block_age <- make_block(
  df_multi,
  var_name   = "AGE_GROUP",
  group_label = "Age",
  ref_level   = "16-24"
)

block_disabled <- make_block(
  df_multi,
  var_name   = "DISABLED",
  group_label = "Validity status",
  # on renomme les labels cps -> labels du papier
  ref_level   = "Valid",
  label_map   = c(
    "No difficulty"  = "Valid",
    "Has difficulty" = "Disabled"
  )
)

block_minority <- make_block(
  df_multi,
  var_name   = "MINORITY",
  group_label = "Minority status",
  ref_level   = "Majority",
  label_map   = c(
    "0" = "Majority",
    "1" = "Minority"
  )
)

# 4) Assembler la table finale et arrondir
tab1_like <- bind_rows(
  block_sex,
  block_educ,
  block_marital,
  block_residence,
  block_age,
  block_disabled,
  block_minority
) %>%
  mutate(
    Employed   = round(Employed,   0),
    Unemployed = round(Unemployed, 0),
    Inactive   = round(Inactive,   0)
  )

tab1_like

# 5) Export éventuel pour LaTeX / Excel / Word
write_csv(tab1_like, "OUTPUTS/table1_descriptive_status.csv")

