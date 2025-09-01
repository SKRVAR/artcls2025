library(lavaan)

# Suponiendo que 'dbEfa_final' es tu dataframe con los ítems finales y depurados.

# 1. Definir la estructura del modelo en un texto
modelo_cfa <- '
  # Definición de los factores latentes (variables latentes)
  # La variable latente "biocentrismo" es medida por los siguientes ítems:
  biocentrismo =~ q4_td_bio + q5_al_bio + q6_bf_bio + q9_td_bio +
                   q10_bf_bio + q11_td_bio + q12_bf_bio + q13_al_bio +
                   q14_rep_bio + q19_td_bio + q21_bf_bio

  # La variable latente "antropocentrismo" es medida por los siguientes ítems:
  antropocentrismo =~ q2_bf_ant + q7_al_ant + q8_rep_ant + q18_rep_ant +
                       q20_bf_ant + q22_bf_ant + q23_td_ant + q24_td_ant +
                       q26_td_ant + q27_bf_ant

  # Por defecto, lavaan calculará la correlación entre biocentrismo y antropocentrismo.
'
# OJO: Es muy probable que esta línea arroje una advertencia o un error
# debido al problema de la matriz no positiva definida.
fit_cfa <- cfa(modelo_cfa, data = dbEfa, std.lv = TRUE)

# Pedir un resumen completo con índices de ajuste y estimaciones estandarizadas
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)