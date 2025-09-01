libs <- c("datapasta", "clipr", "readxl", "tidyverse", "geomtextpath", "ggbreak", "ggrepel")

# Cargar todos los paquetes
for (i in libs) {
    library(i, character.only = TRUE)
}

setwd("D:/IntellijIDEA_Projects/R_Projects/untitled/LozanoSedano/difusores")
# Cargar datos
temp <- read_xlsx("dif_dats.xlsx", sheet = "dats")

dats <- temp %>%
    pivot_longer(
        cols = -time,
        names_to = "variable",
        values_to = "Value"
    ) %>%
    separate(variable, into = c("cat", "var"), sep = "_") %>%
    mutate(cat = case_when(
        cat == "fB" ~ "Fine bubble",
        cat == "coB" ~ "Coarse bubble",
        cat == "ecB" ~ "Extra-coarse bubble",
        TRUE ~ cat
    )) %>%
    mutate(var = case_when(
        var == "do" ~ "Disolved oxigen",
        var == "temp" ~ "Temperature",
        TRUE ~ var
    )) %>%
    mutate(cat = factor(cat, levels = c("Fine bubble", "Coarse bubble", "Extra-coarse bubble")))

dats %>%
    #filter(var == "Disolved oxigen") %>%
    drop_na() %>%
    ggplot(aes(x = time, y = Value, color = var, group = var)) +
    geom_line(aes(linetype = var), linewidth = 1)+
    facet_grid(
        rows = vars(var),
        cols = vars(cat),
        scales = "free"
    )+
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        strip.text = element_text(size = 18),
        axis.text = element_text(color = "grey40", size = 11),
        legend.text = element_text(size = 14, color = "grey40"),
        panel.background = element_rect(fill = "grey95", color = "grey98"),
        axis.line = element_line(colour = "gray20", linewidth = 0.2),
        axis.ticks = element_line(colour = "gray20", linewidth = 0.2)
    )
    #scale_y_continuous(labels = scales::number_format(accuracy = 0.1))

dats2 <- dats %>%
    pivot_wider(
        names_from = var,
        values_from = Value
    ) %>%
    rename(
        "DO" = "Disolved oxigen"

    ) %>%
    drop_na() %>%
    group_by(cat) %>%
    mutate(labDo = if_else(time == max(time), round(DO,2),NA_real_),
           labTemp = if_else(time == max(time), round(Temperature,1),NA_real_))

dats_avg <- dats2 %>%
    group_by(cat) %>%
    summarise(
        meanTemp = round(mean(Temperature, na.rm = TRUE), 2),
        sdTemp = round(sd(Temperature, na.rm = TRUE), 2)
    ) %>%
    mutate(
        meanTemp = paste0("Temp. mean = ",meanTemp, " °C, ±", sdTemp)
    ) %>%
    select(-sdTemp)

dats2 %>%
    left_join(dats_avg, by = "cat") %>%
    ggplot(aes(x = time, group = cat, color = cat)) +
    geom_textline(aes(y = DO),
                  label = "Disolved oxigen",
                  show.legend = FALSE,
                  hjust = 0.1,
                  linewidth = 1.2)+
    geom_textline(aes(y = Temperature/4, label = meanTemp),
                  linetype = "dashed",
                  show.legend = FALSE,
                  hjust = 0.8,
                  linewidth = 1.2)+
    scale_y_continuous(
        sec.axis = sec_axis(~.*4, name = "Temperature (°C)")
    ) +
    geom_label_repel(
        aes(y = labDo, label = labDo),
        size = 4,
        nudge_x = -1,
        nudge_y = 0.2,
        show.legend = FALSE,
        hjust = 0.5
    ) +
    facet_wrap(
        vars(cat),
        nrow = 1,
        scales = "free"
    )+
    labs(y = "Disolved oxigen (mg/L)",
         x = "Time in minutes") +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        strip.text = element_text(size = 18),
        axis.text = element_text(color = "grey40", size = 11),
        legend.text = element_text(size = 14, color = "grey40"),
        panel.background = element_rect(fill = "grey95", color = "grey98"),
        axis.line = element_line(colour = "gray20", linewidth = 0.2),
        axis.ticks = element_line(colour = "gray20", linewidth = 0.2)
    )





ggsave(
    filename = "ox.svg",
    plot = last_plot(),
    device = "svg"
)
ggsave(
    filename = "ox.png",
    plot = last_plot(),
    device = "png",
    dpi = 300
)

tempo <- dats %>%
    mutate(cat = case_when(
        cat == "Fine bubble" ~ "fb",
        cat == "Coarse bubble" ~ "cb",
        cat == "Extra-coarse bubble" ~ "ecb",
        TRUE ~ cat
    )) %>%
    mutate(var = case_when(
        var == "Disolved oxigen" ~ "do",
        var == "Temperature" ~ "temp",
        TRUE ~ var
    ))
tempo <- tempo %>%
    pivot_wider(
        names_from = var,
        values_from = Value
    )

temp2 <- datos %>%
    select(-c(Y1, Y2, Y3))

bindT <- left_join(
    tempo,
    temp2,
    by = c("time" = "time", "cat" = "bubble")
)

temp <- left_join(
    difDatos,
    datos,
    by = c("time" = "time", "cat" = "bubble")
)
difDatos <- temp

save(
    dats,
    datos,
    bindT,
    file = "difusores_dats.RData")

save(difDatos, file = "difDatos.RData")

load("difusores_dats.RData")