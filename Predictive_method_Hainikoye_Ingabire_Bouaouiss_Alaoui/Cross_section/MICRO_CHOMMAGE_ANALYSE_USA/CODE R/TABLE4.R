# ===============================
# 0) Packages + données
# ===============================
library(dplyr)
library(readr)
library(forcats)
library(broom)   # pour un joli tableau (optionnel)

df_multi <- read_csv("DATA_CLEAN/cps_2023_multinomial_base.csv")

# ===============================
# 1) Restreindre à actifs : Employed / Unemployed
#    et créer la variable binaire UNEMP
# ===============================
df_logit <- df_multi %>%
  filter(EMPSTATUS %in% c("Employed", "Unemployed")) %>%
  mutate(
    UNEMP = ifelse(EMPSTATUS == "Unemployed", 1L, 0L),
    
    # remettre proprement les niveaux (avec références choisies)
    SEX_F      = factor(SEX_F,
                        levels = c("Male", "Female")),                # ref = Male
    EDUC_LEVEL = factor(EDUC_LEVEL,
                        levels = c("None", "Primary",
                                   "Secondary", "Higher")),          # ref = None
    MARITAL    = factor(MARITAL,
                        levels = c("Single", "Married")),            # ref = Single
    RESIDENCE  = factor(RESIDENCE,
                        levels = c("Urban", "Rural")),               # ref = Urban
    AGE_GROUP  = fct_relevel(factor(AGE_GROUP), "41-64"),            # ref = 41–64
    MINORITY   = factor(MINORITY,
                        levels = c(0, 1),
                        labels = c("Majority", "Minority")),         # ref = Majority
    DISABLED   = factor(DISABLED,
                        levels = c("No difficulty", "Disabled"))     # ref = No difficulty
  )

# (optionnel) enlever les lignes avec NA sur X ou UNEMP
df_logit <- df_logit %>%
  filter(complete.cases(UNEMP, SEX_F, EDUC_LEVEL, MARITAL,
                        RESIDENCE, AGE_GROUP, MINORITY, DISABLED, WTFINL))

# ===============================
# 2) Modèle (I) : logit binaire  
# ===============================
model_I <- glm(
  UNEMP ~ SEX_F + EDUC_LEVEL + MARITAL +
    RESIDENCE + AGE_GROUP + MINORITY + DISABLED,
  data   = df_logit,
  family = binomial(link = "logit"),
 
)

summary(model_I)

# ===============================
# 3) Coefficients + RRR = exp(beta)
# ===============================
res_I <- tidy(model_I) %>%
  mutate(
    RRR = exp(estimate)   # Relative Risk Ratio (odds ratio)
  )

res_I


# ===============================# ===============================# ===============================# ===============================# ===============================
# 4) Modèle (II) : hommes seulement
#     UNEMP ~ EDUC_LEVEL + MARITAL +
#             RESIDENCE + AGE_GROUP + MINORITY + DISABLED
# ===============================

# Sous-échantillon : uniquement les hommes
df_logit_male <- df_logit %>%
  filter(SEX_F == "Male")

# Vérification rapide (optionnel)
# table(df_logit_male$SEX_F)

model_II <- glm(
  UNEMP ~ EDUC_LEVEL + MARITAL +
    RESIDENCE + AGE_GROUP + MINORITY + DISABLED,
  data   = df_logit_male,
  family = binomial(link = "logit")
)

summary(model_II)

# Coefficients + RRR = exp(beta)
res_II <- tidy(model_II) %>%
  mutate(
    RRR = exp(estimate)   # odds ratio / "RRR" dans ton tableau
  )

res_II

# (optionnel) sauvegarde pour faire les tableaux LaTeX
# write_csv(res_I,  "OUTPUTS/logit_model_I_fullsample.csv")
# write_csv(res_II, "OUTPUTS/logit_model_II_males.csv")
# ===============================# ===============================# ===============================# ===============================# ===============================

# ===============================
# 5) Modèle (III) : femmes seulement
#     UNEMP ~ EDUC_LEVEL + MARITAL +
#             RESIDENCE + AGE_GROUP + MINORITY + DISABLED
# ===============================

# Sous-échantillon : uniquement les femmes
df_logit_female <- df_logit %>%
  filter(SEX_F == "Female")

# Vérification rapide (optionnel)
# table(df_logit_female$SEX_F)

model_III <- glm(
  UNEMP ~ EDUC_LEVEL + MARITAL +
    RESIDENCE + AGE_GROUP + MINORITY + DISABLED,
  data   = df_logit_female,
  family = binomial(link = "logit")
)

summary(model_III)

# Coefficients + RRR = exp(beta)
res_III <- tidy(model_III) %>%
  mutate(
    RRR = exp(estimate)   # odds ratio / "RRR" dans le tableau
  )

res_III

# (optionnel) sauvegarde
# write_csv(res_III, "OUTPUTS/logit_model_III_females.csv")
# ===============================# ===============================# ===============================# ===============================# ===============================

# ===============================
# Modèle (IV) : Majority only
# ===============================

# 1) Sous-échantillon : majorité (MINORITY == "Majority")
df_logit_maj <- df_logit %>%
  filter(MINORITY == "Majority")

# (optionnel) vérifier la taille de l'échantillon
nrow(df_logit_maj)

# 2) Logit binaire sur cet échantillon
#    UNEMP = 1 si chômeur, 0 si employé
#    On garde les mêmes explicatives, sauf MINORITY (constante ici)
model_IV <- glm(
  UNEMP ~ SEX_F + EDUC_LEVEL + MARITAL +
    RESIDENCE + AGE_GROUP + DISABLED,
  data   = df_logit_maj,
  family = binomial(link = "logit")
)

summary(model_IV)

# 3) Tableau coefficients + RRR = exp(beta)
library(broom)

res_IV <- tidy(model_IV) %>%
  mutate(
    RRR = exp(estimate)   # odds ratio
  )

res_IV
# ===============================# ===============================# ===============================# ===============================# ===============================
# ===============================# ===============================# ===============================# ===============================# ===============================
# ===============================
# Modèle (V) : Minority only
# ===============================

# 1) Sous-échantillon : minorités (MINORITY == "Minority")
df_logit_min <- df_logit %>%
  filter(MINORITY == "Minority")

# (optionnel) vérifier la taille de l'échantillon
nrow(df_logit_min)

# 2) Logit binaire sur cet échantillon
#    UNEMP = 1 si chômeur, 0 si employé
#    On garde les mêmes explicatives que pour (IV), sans MINORITY
model_V <- glm(
  UNEMP ~ SEX_F + EDUC_LEVEL + MARITAL +
    RESIDENCE + AGE_GROUP + DISABLED,
  data   = df_logit_min,
  family = binomial(link = "logit")
)

summary(model_V)

# 3) Coefficients + RRR = exp(beta)
library(broom)

res_V <- tidy(model_V) %>%
  mutate(
    RRR = exp(estimate)   # odds ratio
  )

res_V

