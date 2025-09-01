libs <- c("tidyverse", "datapasta", "clipr", "rio", "labelled")
for (lib in libs) {
    library(lib, character.only = TRUE)
}
setwd("D:/IntellijIDEA_Projects/artcls2025/natSujDer")
nams <- import("bdNat.xlsx", sheet = "names")
varsNames <- setNames(as.list(nams$label), nams$new)
nivs <- c("Totalmente en desacuerdo", "En desacuerdo", "Indiferente",
          "De acuerdo", "Totalmente de acuerdo")
labls <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree")
temp <- import("bdNat.xlsx")

bd <- temp %>%
    mutate(edad = as.numeric(edad),
           sex = factor(sex, levels = c("Femenino", "Masculino"),
                        labels = c("Female", "Male"))) %>%
    mutate(across(q1:q28, ~ factor(.x, levels = nivs, labels = labls))) %>%
    mutate(across(q1:q28, ~ as.integer(.x))) %>%
    as_tibble() %>%
    rename(all_of(setNames(nams$var, nams$new))) %>%
    set_variable_labels(.labels = varsNames)


# new after efa
itmElim <- c("q1_bf_ant", "q3_al_ant", "q15_al_ant", "q17_td_ant", "q25_al_bio", "q28_rep_bio", "q16_bf_bio")
bdWefa <- bd %>%
    select(-all_of(itmElim))
export(bd, "bcNat.sav")
save(bd, bdWefa, file = "bdNat.Rdata")
load("bdNat.Rdata")
