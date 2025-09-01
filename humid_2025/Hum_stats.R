temp %>%
    group_by(day, cat) %>%
    summarise(Minimo = min(value),
              Máximo = max(value),
              Promedio = mean(value),
              Normalidad = shapiro.test(value)$p.value,
              Desviacion = sd(value)) %>%
    pivot_longer(cols = c(Minimo, Máximo, Promedio, Normalidad, Desviacion), names_to = "Estadístico", values_to = "Valor") %>%
    pivot_wider(names_from = c(cat,Estadístico), values_from = Valor) %>%
    write_clip()
temp %>%
    group_by(day) %>%
    summarise(transc = difftime(max(h), min(h), units = "mins"))
    write_clip()
