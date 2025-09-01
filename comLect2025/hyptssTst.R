library(rio)
#chi square test for independence
library(DescTools)
install.packages("MESS")
library(MESS)
bd %>%
{chisq.test(x = bd$grade,
            y = bd$lvlPisa,
            correct = TRUE,
            simulate.p.value = TRUE,)}
bd %>%
{GoodmanKruskalGamma(x = bd$lvlPisa,
                      y = bd$grade)}
bd %>%
    select(grade, lvlPisa) %>%
    group_by(grade, lvlPisa) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = grade, values_from = n, values_fill = 0) %>%
  column_to_rownames("lvlPisa") %>%
    as.matrix() %>%
chisq.test(., correct = TRUE, simulate.p.value = TRUE)
# Hypothesis testing for competencies in competencies data

safe_chisq_test <- function(data, var1, var2) {
  tbl <- data %>%
    count({{ var1 }}, {{ var2 }}) %>%
    pivot_wider(names_from = {{ var2 }}, values_from = n, values_fill = 0) %>%
    select(-{{ var1 }}) %>%
    as.matrix()

  chisq.test(tbl, correct = TRUE, simulate.p.value = TRUE)$p.value
}

competencies %>%
  group_by(grade) %>%
  summarise(
    pvalue = safe_chisq_test(data = pick(everything()), var1 = lvlPisa, var2 = comp)
  )


competencies %>%
    group_by(grade) %>%
    summarise(
        pvalue = safe_chisq_test(data = competencies, var2 = comp, var1 = lvlPisa)
    )
competencies %>%
  count(lvlPisa, comp) %>%
    pivot_wider(names_from = comp, values_from = n, values_fill = 0) %>%
    select(-lvlPisa) %>%
    chisq.test(., correct = TRUE, simulate.p.value = TRUE)
competencies %>%
{chisq.test(x =.$comp,
            y = .$lvlPisa,
            correct = TRUE,
            simulate.p.value = TRUE)}
#normality test

competencies %>%
  group_by(grade) %>%
    summarise(
        shapiro_p_value = shapiro.test(puntPisa)$p.value,
        normality = ifelse(shapiro_p_value > 0.05, "Normal", "Not Normal")
    )
# non parametric anova test

competencies %>%
    group_by(grade) %>%
        summarise(
            kruskal_p_value = kruskal.test(puntPisa ~ comp)$p.value,
            anova_result = ifelse(kruskal_p_value > 0.05, "No significant difference", "Significant difference")
        )
competencies %>%
  summarise(kpv = kruskal.test(puntPisa ~ comp)$p.value,
            anova_result = ifelse(kpv > 0.05, "No significant difference", "Significant difference"))
#non parametric two-way anova test
install.packages("ARTool")
library(ARTool)
competencies <- competencies %>%
    mutate(comp = factor(comp, levels = c("AR", "II", "RE")))
artModel <- art(data = competencies,
    formula = puntPisa ~ comp * grade)
anova(artModel)
anovaModel <- aov(puntPisa ~ comp * grade, data = competencies)
summary(anovaModel)

export(competencies, "competencies.sav")