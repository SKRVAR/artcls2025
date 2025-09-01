libs <- c("readxl", "plotly", "tidyverse", "ggpmisc", "ggpubr")
install.packages("rJava", type = "source")  # Fuerza la compilación
install.packages("glmulti")

library(glmulti)


for (lib in libs) {
    library(lib, character.only = TRUE)
}
setwd(find_dir("difusores"))
load("difDatos.RData")

difDatos <- difDatos %>%
    mutate(cat = factor(cat,
                        levels = c("ecb", "cb", "fb"),
                        labels = c("Extra-coarse bubble", "Coarse bubble", "Fine bubble")))

kla <- difDatos %>%
    drop_na() %>%
    ggplot(aes(x = time, y = kla, color = cat)) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5, show.legend = FALSE, alpha = 0.2) +
    geom_point(show.legend = FALSE, size = 2.7, alpha = 0.4)+
    facet_wrap(
            vars(cat),
            nrow = 1,
            scales = "free"
    )+
    labs(y = expression(ln(C[s] - C[t])),
         x = "Time (min)") +
    stat_regline_equation(label.y.npc = 0.28,
                          show.legend = FALSE,
                          size = 6)+
    stat_cor(
        label.y.npc = 0.20,
        show.legend = FALSE,
        size = 6
    )
kla

ggsave(
    filename = "kla.svg",
    plot = kla,
    device = "svg")
ggsave(
    filename = "kla.png",
    plot = kla,
    device = "png",
    dpi = 300
)


resultado <- glmulti(
    oxCap ~ poly(time, 8),  # Hasta grado 3
    data = difDatos,
    level = 1,
    crit = "aicc",
    fitfunction = "lm"
)

resultado_bic <- glmulti(
    oxCap ~ poly(time, 8), 
    data = difDatos, 
    crit = "bic",  # ¡Más estricto!
    level = 1
)

summary(resultado)
