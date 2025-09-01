libs <- c("tidyverse", "datapasta", "clipr", "rio", "ggpubr", "ggsci", "TAM")

for (lib in libs) {
  library(lib, character.only = TRUE)
}
setwd("D:/IntellijIDEA_Projects/artcls2025/comLect2025")
load("bdCompLect.Rdata")
temp <- import("bd_Povis.sav")

items <- c("DE1.1", "DE2.1", "DI17.1", "DE18.1", "RE4.1", "DI13.1", "RE15.1", "RE3.1", "RE8.1", "RE14.1", "DE19.1", "RE5.1", "RE6.1", "DE3.4", "DE4.4", "DE14.4", "DE19.4", "DE1.4", "DE16.4", "DE2.4", "DI13.4", "DI15.4", "DE17.4", "RE20.4", "DE18.4")
newNames <- c("q1_n1-1", "q1_n1-2", "q1_n1-13", "q1_n1-14", "q1_n2-4", "q1_n2-9", "q1_n2-11", "q1_n3-3", "q1_n3-8", "q1_n3-10", "q1_n3-15", "q1_n4-5", "q1_n5-6", "q4_n2-19", "q4_n2-20", "q4_n2-22", "q4_n2-27", "q4_n3-17", "q4_n3-24", "q4_n4-18", "q4_n4-21", "q4_n4-23", "q4_n4-25", "q4_n4-28", "q4_n5-26")

bd <- temp %>%
  select(all_of(items)) %>%
  rename( !!!setNames(items, newNames)) %>%
  mutate(est = row_number())
temp2 <- bd %>%
  pivot_longer(-est, names_to = c("grade", "level"), names_sep = "_") %>%
  pivot_wider( names_from = level, values_from = value) %>%
  mutate(grade = factor(grade, levels = c("q1", "q4"),
                         labels = c("First grade", "Fourth grade")))
bd <- temp2
bd <- bd %>%
  mutate(lvl1 = rowMeans(select(., starts_with("n1-")), na.rm = TRUE),
         lvl2 = rowMeans(select(., starts_with("n2-")), na.rm = TRUE),
         lvl3 = rowMeans(select(., starts_with("n3-")), na.rm = TRUE),
         lvl4 = rowMeans(select(., starts_with("n4-")), na.rm = TRUE),
         lvl5 = rowMeans(select(., starts_with("n5-")), na.rm = TRUE)) %>%
  mutate(lvl1 = replace_na(lvl1,0)) %>%
  mutate(lvl1_median = apply(select(., starts_with("n1-")), 1, median, na.rm = TRUE),
         lvl2_median = apply(select(., starts_with("n2-")), 1, median, na.rm = TRUE),
         lvl3_median = apply(select(., starts_with("n3-")), 1, median, na.rm = TRUE),
         lvl4_median = apply(select(., starts_with("n4-")), 1, median, na.rm = TRUE),
         lvl5_median = apply(select(., starts_with("n5-")), 1, median, na.rm = TRUE)) %>%
  mutate(lvl1_median = replace_na(lvl1_median,0))

temp2 <- bd %>%
  rowwise() %>%
  mutate(maxLevel = paste0("lvl", which.max(c(lvl1, lvl2, lvl3, lvl4, lvl5)))) %>%
  ungroup() %>%
  mutate(maxLevel = factor(maxLevel, levels = c("lvl1", "lvl2", "lvl3", "lvl4", "lvl5"),
                         labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")))
bd <- temp2

bd <- bd %>%
  mutate(niv1 = rowSums(select(., starts_with("n1-")), na.rm = TRUE),
         niv2 = rowSums(select(., starts_with("n2-")), na.rm = TRUE),
         niv3 = rowSums(select(., starts_with("n3-")), na.rm = TRUE),
         niv4 = rowSums(select(., starts_with("n4-")), na.rm = TRUE),
         niv5 = rowSums(select(., starts_with("n5-")), na.rm = TRUE))

scoreLec <- function(n1, n2, n3, n4, n5) {
  # Calculate weighted sum
  weighted_sum <- (1^2 * n1) + (2^2 * n2) + (3^2 * n3) + (4^2 * n4) + (5^2 * n5)
  N <- n1 + n2 + n3 + n4 + n5
  # Return the score
  return(weighted_sum/(N*5))
}

bd <- bd %>%
  mutate(score = scoreLec(niv1, niv2, niv3, niv4, niv5))
changeNames <- tibble::tribble(
  ~old,       ~new,
  "n1-1_ar",  "n1-1_ar",
  "n1-2_ar",  "n1-2_ar",
  "n1-13_ar", "n1-13_ar",
  "n1-14_ar", "n1-14_ar",
  "n2-4_ii",  "n2-4_ii",
  "n2-9_ar",  "n2-9_ar",
  "n2-11_ar", "n2-11_ar",
  "n3-3_ii",  "n3-3_ii",
  "n3-8_ii",  "n3-8_ii",
  "n3-10_ii", "n3-10_ii",
  "n3-15_re", "n3-15_re",
  "n4-5_re",  "n4-5_re",
  "n5-6_re",  "n5-6_re",
  "n2-19_ar", "n2-19_ar",
  "n2-20_ii", "n2-20_ii",
  "n2-22_ar", "n2-22_ar",
  "n2-27_ar", "n2-27_ar",
  "n3-17_ii", "n3-17_ar",
  "n3-24_ii", "n3-24_re",
  "n4-18_ii", "n4-18_ii",
  "n4-21_ii", "n4-21_ii",
  "n4-23_re", "n4-23_re",
  "n4-25_ii", "n4-25_re",
  "n4-28_ii", "n4-28_ii",
  "n5-26_re", "n5-26_re"
)


bd <- bd %>%
  rename(!!!setNames(changeNames$old, changeNames$new))
# Make sure your data is properly prepared
bd_items1 <- bd %>%
  filter(grade == "First grade") %>%
  select(est, "n1-1_ar":"n5-6_re")
bd_items4 <- bd %>%
  filter(grade == "Fourth grade") %>%
  select(est,"n2-19_ar":"n5-26_re")

# 2. Ajustar modelo Rasch CORREGIDO (fijar dificultades para 5 ítems)
modelo_rasch1 <- tam(bd_items1[,-1])
modelo_rasch4 <- tam(bd_items4[,-1])

# 3. Generar 5 valores plausibles
pv1 <- tam.pv(modelo_rasch1, nplausible = 5)
pv4 <- tam.pv(modelo_rasch4, nplausible = 5)

pvT <- bind_rows(
  pv1$pv %>% mutate(grade = "First grade"),
  pv4$pv %>% mutate(grade = "Fourth grade")
)
breaks <- c(-Inf, 335, 407, 480, 553, 626, 698, Inf)
labls <- c("< Level 1", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Level 6")
eapT <- bind_rows(
  as_tibble(modelo_rasch1$person$EAP) %>%
    mutate(grade = "First grade",
           est = row_number()) %>%
    rename(EAP = value),
  as_tibble(modelo_rasch4$person$EAP) %>%
    mutate(grade = "Fourth grade",
           est = row_number()) %>%
    rename(EAP = value)) %>%
  mutate(puntPisa = EAP*100+500) %>%
  mutate(lvlPisa = cut(puntPisa, breaks = breaks, labels = labls, right = FALSE)) %>%
  mutate(lvlPisa = factor(lvlPisa, levels = labls))

temp <- full_join(bd, pvT, by = c("est"="pid", "grade"="grade")) %>%
  full_join(eapT, by = c("est"="est", "grade"="grade"))
bd <- temp

itemsAr <- bd %>%
  select(est, grade, contains("_ar"))
itemsIi <- bd %>%
  select(est, grade, contains("_ii"))
itemsRe <- bd %>%
    select(est, grade, contains("_re"))

modelo1Ar <- itemsAr %>%
  filter(grade == "First grade") %>%
  select(where(~!any(is.na(.)))) %>%
  select(-est, -grade) %>%
  {
    resp_data <- .
    item_names <- names(resp_data)
    fixedParams <- dificult1 %>%
      filter(items %in% item_names)
    fixedParams <- cbind(
      match(fixedParams$items, item_names),
      fixedParams$xsi
    )

    tam(., xsi.fixed = fixedParams)
  }


modelo1Ii <- itemsIi %>%
  filter(grade == "First grade") %>%
  select(where(~!any(is.na(.)))) %>%
  select(-est, -grade) %>%
{
    resp_data <- .
    item_names <- names(resp_data)
    fixedParams <- dificult1 %>%
      filter(items %in% item_names)
    fixedParams <- cbind(
      match(fixedParams$items, item_names),
      fixedParams$xsi
    )

    tam(., xsi.fixed = fixedParams)
}
modelo1Re <- itemsRe %>%
    filter(grade == "First grade") %>%
    select(where(~!any(is.na(.)))) %>%
    select(-est, -grade) %>%
    {
        resp_data <- .
        item_names <- names(resp_data)
        fixedParams <- dificult1 %>%
            filter(items %in% item_names)
        fixedParams <- cbind(
            match(fixedParams$items, item_names),
            fixedParams$xsi
        )

        tam(., xsi.fixed = fixedParams)
    }
modelo4Ar <- itemsAr %>%
    filter(grade == "Fourth grade") %>%
    select(where(~!any(is.na(.)))) %>%
    select(-est, -grade) %>%
    {
        resp_data <- .
        item_names <- names(resp_data)
        fixedParams <- dificult4 %>%
            filter(items %in% item_names)
        fixedParams <- cbind(
            match(fixedParams$items, item_names),
            fixedParams$xsi
        )

        tam(., xsi.fixed = fixedParams)
    }
modelo4Ii <- itemsIi %>%
    filter(grade == "Fourth grade") %>%
    select(where(~!any(is.na(.)))) %>%
    select(-est, -grade) %>%
    {
        resp_data <- .
        item_names <- names(resp_data)
        fixedParams <- dificult4 %>%
            filter(items %in% item_names)
        fixedParams <- cbind(
            match(fixedParams$items, item_names),
            fixedParams$xsi
        )

        tam(., xsi.fixed = fixedParams)
    }
modelo4Re <- itemsRe %>%
    filter(grade == "Fourth grade") %>%
    select(where(~!any(is.na(.)))) %>%
    select(-est, -grade) %>%
    {
        resp_data <- .
        item_names <- names(resp_data)
        fixedParams <- dificult4 %>%
            filter(items %in% item_names)
        fixedParams <- cbind(
            match(fixedParams$items, item_names),
            fixedParams$xsi
        )

        tam(., xsi.fixed = fixedParams)
    }



eapTAr <- bind_rows(
  as_tibble(modelo1Ar$person$EAP) %>%
  rename(EAP = value) %>%
  mutate(grade = "First grade",
             est = row_number()) %>%
    mutate(puntPisa = EAP*100+500) %>%
    mutate(lvlPisa = cut(puntPisa, breaks = breaks, labels = labls, right = FALSE)) %>%
    mutate(lvlPisa = factor(lvlPisa, levels = labls)),
    as_tibble(modelo4Ar$person$EAP) %>%
    mutate(grade = "Fourth grade",
             est = row_number()) %>%
    rename(EAP = value) %>%
    mutate(puntPisa = EAP*100+500) %>%
    mutate(lvlPisa = cut(puntPisa, breaks = breaks, labels = labls, right = FALSE)) %>%
    mutate(lvlPisa = factor(lvlPisa, levels = labls))) %>%
  mutate(comp = "AR")

eapTIi <- bind_rows(
  as_tibble(modelo1Ii$person$EAP) %>%
  rename(EAP = value) %>%
  mutate(grade = "First grade",
             est = row_number()) %>%
    mutate(puntPisa = EAP*100+500) %>%
    mutate(lvlPisa = cut(puntPisa, breaks = breaks, labels = labls, right = FALSE)) %>%
    mutate(lvlPisa = factor(lvlPisa, levels = labls)),
    as_tibble(modelo4Ii$person$EAP) %>%
    mutate(grade = "Fourth grade",
             est = row_number()) %>%
    rename(EAP = value) %>%
    mutate(puntPisa = EAP*100+500) %>%
    mutate(lvlPisa = cut(puntPisa, breaks = breaks, labels = labls, right = FALSE)) %>%
    mutate(lvlPisa = factor(lvlPisa, levels = labls))) %>%
    mutate(comp = "II")
eapTRe <- bind_rows(
    as_tibble(modelo1Re$person$EAP) %>%
    rename(EAP = value) %>%
    mutate(grade = "First grade",
                 est = row_number()) %>%
        mutate(puntPisa = EAP*100+500) %>%
        mutate(lvlPisa = cut(puntPisa, breaks = breaks, labels = labls, right = FALSE)) %>%
        mutate(lvlPisa = factor(lvlPisa, levels = labls)),
        as_tibble(modelo4Re$person$EAP) %>%
        mutate(grade = "Fourth grade",
                 est = row_number()) %>%
        rename(EAP = value) %>%
        mutate(puntPisa = EAP*100+500) %>%
        mutate(lvlPisa = cut(puntPisa, breaks = breaks, labels = labls, right = FALSE)) %>%
        mutate(lvlPisa = factor(lvlPisa, levels = labls))) %>%
  mutate(comp = "RE")
competencies <- bind_rows(eapTAr, eapTIi, eapTRe) %>%
  mutate(grade = factor(grade, levels = c("First grade", "Fourth grade")))

bd <- bd %>%
  mutate(pv1Pisa = 500+100*PV1.Dim1,
         pv2Pisa = 500+100*PV2.Dim1,
         pv3Pisa = 500+100*PV3.Dim1,
         pv4Pisa = 500+100*PV4.Dim1,
         pv5Pisa = 500+100*PV5.Dim1)

dificult1 <- as_tibble(modelo_rasch1$xsi, rownames = "items")
dificult4 <- as_tibble(modelo_rasch4$xsi, rownames = "items")

load("bdCompLect.Rdata")
load("competencies.Rdata")

save(bd, file = "bdCompLect.Rdata")
save(competencies, file = "competencies.Rdata")