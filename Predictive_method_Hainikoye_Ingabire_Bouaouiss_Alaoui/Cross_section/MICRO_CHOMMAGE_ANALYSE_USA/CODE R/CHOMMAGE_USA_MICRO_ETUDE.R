library(dplyr)
library(tidyr)
library(readr)

# 1) Charger la base multinomiale
df_multi <- read_csv("DATA_CLEAN/cps_2023_multinomial_base.csv")

# S’assurer de l’ordre Employed / Unemployed / Inactive
df_multi$EMPSTATUS <- factor(df_multi$EMPSTATUS,
                             levels = c("Employed","Unemployed","Inactive"))

# 2) Fonction qui construit un bloc "à la Table 1"
make_block <- function(data, var, group_label,
                       ref_level = NULL,      # catégorie de référence (à marquer *)
                       label_map = NULL) {    # éventuel recodage (ex: 0→Majority)
  
  var <- rlang::as_string(rlang::enquo(var))
  
  tmp <- data %>%
    filter(!is.na(.data[[var]]), !is.na(EMPSTATUS)) %>%
    group_by(EMPSTATUS, .data[[var]]) %>%
    summarise(w = sum(WTFINL, na.rm = TRUE), .groups = "drop") %>%
    group_by(EMPSTATUS) %>%
    mutate(share = 100 * w / sum(w)) %>%   # % dans chaque statut
    ungroup() %>%
    mutate(Category = as.character(.data[[var]])) %>%
    select(-all_of(var))
  
  # Recode des labels si besoin (MINORITY, etc.)
  if (!is.null(label_map)) {
    tmp$Category <- dplyr::recode(tmp$Category, !!!label_map)
  }
  
  # Marquer la catégorie de référence par une étoile
  if (!is.null(ref_level)) {
    tmp$Category <- ifelse(tmp$Category == ref_level,
                           paste0(ref_level, "*"),
                           tmp$Category)
  }
  
  # Ordre des lignes : référence d’abord, puis le reste
  ref_cat   <- if (!is.null(ref_level)) paste0(ref_level, "*") else NULL
  other_cat <- setdiff(unique(tmp$Category), ref_cat)
  tmp$Category <- factor(tmp$Category,
                         levels = c(ref_cat, sort(other_cat)))
  
  tmp %>%
    select(Category, EMPSTATUS, share) %>%
    mutate(share = round(share)) %>%  # arrondi à l’entier pour coller au papier
    pivot_wider(names_from = EMPSTATUS,
                values_from = share) %>%
    arrange(Category) %>%
    mutate(Group = group_label, .before = Category)
}

# 3) Construire le tableau complet (comme la Figure/Table 1 de l’article)

tab1_like <- bind_rows(
  make_block(df_multi, SEX_F,
             group_label = "Sex",
             ref_level   = "Male"),
  
  make_block(df_multi, EDUC_LEVEL,
             group_label = "Education level",
             ref_level   = "None"),
  
  make_block(df_multi, MARITAL,
             group_label = "Marital status",
             ref_level   = "Single"),
  
  make_block(df_multi, RESIDENCE,
             group_label = "Place of residence",
             ref_level   = "Urban"),
  
  make_block(df_multi, AGE_GROUP,
             group_label = "Age",
             ref_level   = "16-24"),
  
  make_block(df_multi, DISABLED,
             group_label = "Validity status",
             ref_level   = "No difficulty"),
  
  # Bloc optionnel pour le statut minoritaire
  make_block(df_multi, MINORITY,
             group_label = "Minority status",
             ref_level   = "Majority",
             label_map   = c("0" = "Majority",
                             "1" = "Minority"))
)

tab1_like
# A tibble avec colonnes :
# Group | Category | Employed | Unemployed | Inactive

# 4) Sauvegarde pour LaTeX / Word
write_csv(tab1_like, "OUTPUTS/table1_style_descriptive_status.csv")

