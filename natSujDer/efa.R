library(psych)
library(caret)
library(EFAtools)

dbEfa <- bd %>%
  select(-c(q1_bf_ant, q3_al_ant, q15_al_ant, q17_td_ant, q25_al_bio, q28_rep_bio, q16_bf_bio)) %>%
  select(-(id:Condition))

N_FACTORS(dbEfa, criteria = c("PARALLEL", "MAP", "K1", "EIGEN", "VSS", "GAP", "SABCA"),
          eigen_type_other = c("SMC", "PCA", "ML"), show_progress = TRUE)
EFAtools::PARALLEL(dbEfa)

modEfa <- psych::fa(
  r = dbEfa,
    nfactors = 2,
  cor = "poly",
  fm = "pa",
    rotate = "oblimin"
)
print(modEfa, cut = 0.4, sort = TRUE)

# efa por grupos
bio <- dbEfa %>%
  select(contains("bio"))
ant <- dbEfa %>%
    select(contains("ant"))

EFAtools::PARALLEL(bio)
EFAtools::PARALLEL(ant)
# Cargar la librería

# AFE para la subescala Biocéntrica (confirmando 1 factor)
modelo_bio_final <- fa(
  r = bio, # Tu dataframe con los ítems de la escala Biocéntrica
  nfactors = 1,
  cor = "poly",
  fm = "pa"
)
print(modelo_bio_final)

# AFE para la subescala Antropocéntrica (confirmando 1 factor)
modelo_ant_final <- fa(
  r = ant, # Tu dataframe con los ítems de la escala Antropocéntrica
  nfactors = 1,
  cor = "poly",
  fm = "pa"
)
print(modelo_ant_final)