libs <- c("tidyverse", "datapasta", "clipr", "rio", "ggpubr", "ggsci", "TAM")
library(ggradar)
for (lib in libs) {
  library(lib, character.only = TRUE)
}
library(ggridges)
library(patchwork)
setwd("D:/IntellijIDEA_Projects/artcls2025/comLect2025")
bd %>%
  count(grade, maxLevel) %>%
  group_by(grade) %>%
  mutate(Per = round(n/sum(n),2)) %>%
  ggplot(aes(x = maxLevel, y = Per, group = grade, colour = grade)) +
  geom_col(aes(fill = grade),show.legend = FALSE) +
  geom_density(data = bd,
               aes(x = maxLevel, y = ..density..),
               alpha = 0.3,
               show.legend = FALSE,
               colour = "gray40") +
  geom_text(aes(label = scales::percent(Per)), size = 6, vjust = -0.5, show.legend = FALSE) +
  labs(x = "Max Level", y = "Students", fill = "Grade") +
  facet_wrap(~grade) +
  scale_fill_npg()+
  scale_y_continuous(limits = c(0, 0.6), labels = scales::percent_format())



ggsave(filename = "maxLvl.svg",
       plot = last_plot(),
       device = "svg")

bd %>%
count(grade, lvlPisa) %>%
group_by(grade) %>%
mutate(percentage = round(n/sum(n), 2)) %>%
  ggplot(aes(x = lvlPisa, y = percentage, group = grade))+
  geom_col(aes(fill = grade,
               color = grade),
           alpha = 0.7,
           show.legend = FALSE)+
  geom_text(aes(label = scales::percent(percentage),
                color = grade),
            vjust = -0.5, size = 6,
            show.legend = FALSE)+
  geom_density(data = bd,
               aes(x=lvlPisa,
                   y = after_stat(density),
                   group = grade
                   ),
               colour = "gray20",
               alpha = 0.3,
               linetype = "dashed",
               show.legend = FALSE)+
  facet_wrap(~grade, scales = "free_y")+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(x = "PISA Level", y = "Students") +
  scale_fill_jco()+
  scale_color_jco()

competencies %>%
    count(grade, comp, lvlPisa) %>%
    group_by(grade, comp) %>%
    mutate(percentage = round(n/sum(n), 2)) %>%
    ggplot(aes(x = lvlPisa, y = percentage, group = comp)) +
    geom_col(aes(fill = comp), show.legend = FALSE) +
    geom_text(aes(label = scales::percent(percentage),
                  color = comp),
              vjust = -0.5, size = 6,
              show.legend = FALSE) +
    geom_density(data = competencies,
                 aes(x = lvlPisa,
                     y = after_stat(density),
                     group = comp),
                 colour = "gray10",
                 linetype = "dashed",
                 show.legend = FALSE) +
    facet_grid(comp~grade) +
    scale_y_continuous(limits = c(0, 1.2), labels = scales::percent_format()) +
    labs(x = "PISA Level", y = "Students") +
    scale_fill_jco() +
    scale_color_jco() +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    theme_minimal(base_size = 15)



competencies %>%
  ggplot(aes(x = lvlPisa, group = comp)) +
  geom_density(aes(colour = comp,
                   linetype = comp,
                   fill = comp),
               alpha = 0.2)+
  facet_wrap(~grade, scales = "free_y")+
  scale_color_jco()+
  scale_fill_lancet()

competencies %>%
    count(grade, comp, lvlPisa) %>%
    group_by(grade, comp) %>%
    mutate(percentage = round(n/sum(n), 2))
    ggplot(aes(x = lvlPisa, y = percentage, group = comp)) +
    geom_col(aes(fill = comp), show.legend = FALSE) +
    geom_text(aes(label = scales::percent(percentage),
                  color = comp),
              vjust = -0.5, size = 6,
              show.legend = FALSE) +
    geom_density(data = competencies,
                 aes(x = lvlPisa,
                     y = after_stat(density),
                     group = comp),
                 colour = "gray10",
                 linetype = "dashed",
                 show.legend = FALSE) +
    facet_grid(comp~grade) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    labs(x = "PISA Level", y = "Students") +
    scale_fill_jco() +
    scale_color_jco() +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    theme_minimal(base_size = 15)

# Radar plot for competencies


## ggridges
estad <- competencies %>%
  group_by(comp) %>%
  summarise(meanPunt = mean(puntPisa, na.rm = TRUE),
            medianaPunt = median(puntPisa, na.rm = TRUE),
            sdPunt = sd(puntPisa, na.rm = TRUE),
            minPunt = min(puntPisa, na.rm = TRUE),
            maxPunt = max(puntPisa, na.rm = TRUE))
competencies %>%  
    ggplot(aes(x = puntPisa, y = comp, fill = comp))+
    geom_density_ridges(mapping = aes(colour = comp),
                        alpha = 0.5,
                        show.legend = FALSE,
                        bandwidth = 10)+
  scale_fill_jco()+
  scale_color_jco()+
  labs(x = "PISA Score", y = "Competency")+
  facet_wrap(~grade, scales = "free_y")

dificult1 <- as_tibble(modelo_rasch1$xsi, rownames = "items")
dificult4 <- as_tibble(modelo_rasch4$xsi, rownames = "items")

dif4 <- dificult4 %>%
    mutate(items = str_replace(items, "[a-z][0-9]-", "q")) %>%
    ggplot(aes(x = reorder(items, xsi), y = xsi)) +
    geom_point(show.legend = FALSE, size = 5,
               shape = 23, fill = pal_jco()(3)[3])+
    labs(x = "Items", y = "Difficulty (Logits)")+
    scale_y_continuous(limits = c(-3.5, 2)) +
  geom_text(aes(label = round(xsi, 2)),
            vjust = 2.5, size = 5,
            show.legend = FALSE,
            color = pal_jco()(3)[3]) +
    scale_size_continuous(range = c(1, 5))+
  theme(axis.text.x.bottom = element_text(angle = 90, size = 12))
dif1 <- dificult1 %>%
    mutate(items = str_replace(items, "[a-z][0-9]-", "q")) %>%
    ggplot(aes(x = reorder(items, xsi), y = xsi)) +
    geom_point(show.legend = FALSE, size = 5,
               shape = 23, fill = pal_jco()(3)[3])+
    labs(x = "Items", y = "Difficulty (Logits)")+
    scale_y_continuous(limits = c(-3.5, 2)) +
    geom_text(aes(label = round(xsi, 2)),
                vjust = 2.5, size = 5,
                show.legend = FALSE,
                color = pal_jco()(3)[3]) +
    scale_size_continuous(range = c(1, 5))+
  theme(axis.text.x.bottom = element_text(angle = 90, size = 12))
hist4 <- person4 %>%
    ggplot(aes(y = theta)) +
    geom_histogram(binwidth = 0.4,
                   show.legend = FALSE,
                   fill = pal_jco()(2)[2],
                   color = "white") +
  stat_bin(aes(x = after_stat(count),
               label = scales::percent(after_stat(count/sum(after_stat(count))),
                                       accuracy = 1)),
           binwidth = 0.4,
           geom = "text",
           hjust = -0.2, size = 6,
           show.legend = FALSE,
           color = pal_jco()(2)[2]) +
    labs(y = NULL, x = "Frequency")+
  scale_y_continuous(sec.axis = dup_axis(name = "Student Ability (Logits)")) +
  scale_x_continuous(limits = c(0,20))+
  coord_cartesian(ylim = c(-3.5, 2)) +
  scale_size_continuous(range = c(1, 5))+
  theme(axis.title.y.right = element_text(angle = 90, vjust = 0.5))
hist1 <- person1 %>%
    ggplot(aes(y = theta)) +
    geom_histogram(binwidth = 0.4,
                   show.legend = FALSE,
                   fill = pal_jco()(2)[2],
                   color = "white") +
  stat_bin(aes(x = after_stat(count),
               label = scales::percent(after_stat(count/sum(after_stat(count))),
                                       accuracy = 1)),
           binwidth = 0.4,
           geom = "text",
           hjust = -0.2, size = 6,
           show.legend = FALSE,
           color = pal_jco()(2)[2]) +
    labs(y = NULL, x = "Frequency")+
  scale_y_continuous(sec.axis = dup_axis(name = "Student Ability (Logits)")) +
  scale_x_continuous(limits = c(0,20))+
  coord_cartesian(ylim = c(-3.5, 2)) +
  scale_size_continuous(range = c(1, 5))+
  theme(axis.title.y.right = element_text(angle = 90, vjust = 0.5))
freq4 <- bd_items4 %>%
  select(-est) %>%
    pivot_longer(cols = everything(),
                 names_to = "items",
                 values_to = "response") %>%
  group_by(items) %>%
  summarise(n = sum(response, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percentage = round(n/sum(n), 2)) %>%
  left_join(dificult4, by = "items") %>%
  mutate(items = str_replace(items, "[a-z][0-9]-", "q")) %>%
    ggplot(aes(y = reorder(items, xsi), x = percentage)) +
    geom_col(show.legend = FALSE,
             fill = pal_jco()(2)[1]
             ) +
    geom_text(aes(label = scales::percent(percentage)),
              hjust = 1, size = 5,
              show.legend = FALSE,
              color = pal_jco()(2)[1])+
  scale_x_reverse(label = scales::percent_format(accuracy = 1),
                  limits = c(.2,0))+
  scale_y_discrete(position = "right") +
  theme(axis.title.y.right = element_text(angle = 90, vjust = 0.5),
        axis.text.y.right = element_text(size = 12)) +
  labs(x = "Correct responses", y = NULL)
freq1 <- bd_items1 %>%
    select(-est) %>%
        pivot_longer(cols = everything(),
                     names_to = "items",
                     values_to = "response") %>%
    group_by(items) %>%
    summarise(n = sum(response, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(percentage = round(n/sum(n), 2)) %>%
    left_join(dificult1, by = "items") %>%
    mutate(items = str_replace(items, "[a-z][0-9]-", "q")) %>%
        ggplot(aes(y = reorder(items, xsi), x = percentage)) +
        geom_col(show.legend = FALSE,
                 fill = pal_jco()(2)[1]
                 ) +
        geom_text(aes(label = scales::percent(percentage)),
                hjust = 1, size = 5,
                show.legend = FALSE,
                color = pal_jco()(2)[1])+
    scale_x_reverse(label = scales::percent_format(accuracy = 1),
                    limits = c(.2,0))+
    scale_y_discrete(position = "right") +
    theme(axis.title.y.right = element_text(angle = 90, vjust = 0.5),
            axis.text.y.right = element_text(size = 12)) +
    labs(x = "Correct responses", y = NULL)




layout <- "ABBC"
freq4+dif4+hist4 +
  plot_layout(design = layout)
freq1+dif1+hist1 +
  plot_layout(design = layout)



person1 <- tam.wle(modelo_rasch1)
person4 <- tam.wle(modelo_rasch4)
estimate1 <- person1$theta
thershold1 <- tam.threshold(modelo_rasch1)
estimate4 <- tam.threshold(modelo_rasch4)
library(WrightMap)
TAM::IRT.WrightMap(modelo_rasch1)
wrightMap(estimate1, thershold1)
ggsave(filename = "wraigthPlot4.svg",
       plot = last_plot(),
       device = "svg")
ggsave(filename = "wraigthPlot4.png",
       plot = last_plot(),
       device = "png",
       dpi = 300)
ggsave(filename = "wraigthPlot1.svg",
       plot = last_plot(),
       device = "svg")
ggsave(filename = "wraigthPlot1.png",
         plot = last_plot(),
         device = "png",
         dpi = 300)
ggsave(filename = "pisaComp.svg",
       plot = last_plot(),
       device = "svg")
load("competencies.Rdata")
load("bdCompLect.Rdata")