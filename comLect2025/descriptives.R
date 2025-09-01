

bd %>%
  select(grade, pv1Pisa:pv5Pisa) %>%
    pivot_longer(cols = -grade,
                 names_to = "pv",
                 values_to = "puntPisa") %>%
  group_by(grade, pv) %>%
  summarise(
    meanPisa = mean(puntPisa, na.rm = TRUE),
    sdPisa = sd(puntPisa, na.rm = TRUE),
    minPisa = min(puntPisa, na.rm = TRUE),
    maxPisa = max(puntPisa, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(grade) %>%
  summarise(MeanPisa = mean(meanPisa, na.rm = TRUE),
            SdPisa = mean(sdPisa, na.rm = TRUE),
            MinPisa = min(minPisa, na.rm = TRUE),
            MaxPisa = max(maxPisa, na.rm = TRUE))

competencies %>%
  group_by(comp) %>%
    summarise(
        meanPisa = round(mean(puntPisa, na.rm = TRUE), 2),
        medianaPisa = round(median(puntPisa, na.rm = TRUE),2),
        sdPisa = round(sd(puntPisa, na.rm = TRUE), 2),
        minPisa = round(min(puntPisa, na.rm = TRUE), 2),
        maxPisa = round(max(puntPisa, na.rm = TRUE), 2)
    )


load("competencies.Rdata")