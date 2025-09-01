libs <- c("datapasta", "clipr", "tidyverse", "readxl")
library("haven")
library("ggpubr")
for (lib in libs) {
    library(lib, character.only = TRUE)
}

setwd("D:/IntellijIDEA_Projects/R_Projects/untitled/LozanoSedano")
#datos

dats <- read_xlsx("amyHum.xlsx")


temp <- dats %>%
    pivot_longer(cols = c(hum, temp), names_to = "cat", values_to = "value") %>%
    mutate(cat = case_when(cat == "hum" ~ "Humidity",
                           cat == "temp" ~ "Temperature")) %>%
    mutate(cat = factor(cat, levels = c("Temperature", "Humidity")))
new_labels <- c("día 1" = "Day 1", "día 2" = "Day 2", "día 3" = "Day 3", "día 4" = "Day 4", "día 5" = "Day 5", "día 6" = "Day 6", "día 7" = "Day 7", "día 8" = "Day 8", "día 9" = "Day 9")
temp %>%
    ggplot(aes(x = h, y = value, group = cat, color = cat)) +
    geom_line(linewidth = 0.7)+
    facet_wrap(~day, scales = "free_x", labeller = as_labeller(new_labels))+
    labs(x = "Hour",
         y = "Measurement")+
    scale_color_brewer(palette = "Set1")+
    theme(strip.text = element_text(size = 15),
          panel.background = element_rect(fill = "grey94", color = "grey90"),
          legend.title = element_blank(),
          legend.position = "top")



corGraph <- dats %>%
    ggplot(aes(x = hum, y = temp)) +
    geom_point(aes(color = day), alpha = 0.4, size = 3.5, show.legend = FALSE)+
    geom_smooth(aes(color = day),method = "lm", se = FALSE, size = 0.5, show.legend = FALSE)+
    facet_wrap(~day, scales = "free", labeller = as_labeller(new_labels))+
    labs(x = "Humidity",
         y = "Temperature")+
    theme(strip.text = element_text(size = 15),
          panel.background = element_rect(fill = "grey94", color = "grey90"))
corGraph <- corGraph+
    stat_cor(method = "spearman",
             size = 4,
             cor.coef.name = "rho",
             label.y.npc = 0.2)
# dfa

dats %>%
    group_by(day) %>%
    summarise(r = cor(hum, temp, method = "pearson"),
              sig_r = cor.test(hum, temp, method = "pearson")$p.value,
              rho = cor(hum, temp, method = "spearman"),
              sig_rho = cor.test(hum, temp, method = "spearman")$p.value) %>%
    write_clip()

ggsave("cor.png", dpi = 300)
ggsave("cor.svg", device = "svg")
ggsave("temHum.png", dpi = 300)
ggsave("temHum.svg", device = "svg")
write_sav(dats, "amyHum.sav")
