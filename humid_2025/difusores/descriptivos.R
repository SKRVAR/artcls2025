load("difDatos.RData")
libs <- c("tidyverse", "datapasta", "clipr")
for (lib in libs) {
    library(lib, character.only = TRUE)
}

difDatos %>%
    drop_na() %>%
    group_by(cat) %>%
    summarise(
        maxMean = max(do)
    ) %>%
    summarise(mean = mean(maxMean),
              sd = sd(maxMean))

difDatos <- difDatos %>%
    rename("ln_cs-cl" = "kla") %>%
    select(-capOxigen)

temp <- difDatos %>%
    drop_na() %>%
    mutate(kla = case_when(
        cat == "Extra-coarse bubble" ~ 0.056,
        cat == "Coarse bubble" ~ 0.049,
        cat == "Fine bubble" ~ 0.25)) %>%
    mutate(oxCap2 = (6.36 - do)*kla)
