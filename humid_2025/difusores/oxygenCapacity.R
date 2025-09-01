lib <- c("readxl", "dplyr", "plotly", "tidyverse")
for (lib in libs) {
    library(lib, character.only = TRUE)
}

setwd(find_dir("difusores"))
load("difDatos.RData")

read_xlsx("oxCap.xlsx")
sheets <- readxl::excel_sheets("oxCap.xlsx")
oxCapBind <- tibble()
for (i in sheets) {
    temp <- read_xlsx("oxCap.xlsx", sheet = i) %>%
        mutate(bubble = i)
    oxCapBind <- bind_rows(oxCapBind, temp)
}

oxCap <- oxCapBind %>%
    mutate(oxCap = rowMeans(across(-c(time, bubble)), na.rm = TRUE)) %>%
    mutate(bubble = case_when(
        bubble == "gruesa" ~ "cb",
        bubble == "extragruesa" ~ "ecb",
        bubble == "fina" ~ "fb",
        TRUE ~ bubble
    ))
bindT <- bindT %>%
    rename("kla" = "capOxigen")
difDatos <- left_join(bindT, oxCap, by = c("time" = "time", "cat" = "bubble"))

ecb <- difDatos %>%
    dplyr::filter(cat == "ecb") %>%
    drop_na() %>%
    dplyr::select(time, kla, oxCap)
cb <- difDatos %>%
    dplyr::filter(cat == "cb") %>%
    drop_na() %>%
    dplyr::select(time, kla, oxCap)
fb <- difDatos %>%
    dplyr::filter(cat == "fb") %>%
    drop_na() %>%
    dplyr::select(time, kla, oxCap)

ecb3d <- plot_ly(data = ecb, x = ~time, y = ~kla, z = ~oxCap,
                 type = "scatter3d",
                 mode = "lines",
                 line = list(
                     color = ~oxCap,
                     colorscale = "Viridis",
                     width = 9,
                     showscale = TRUE,
                     colorbar = list(
                         title = list(
                             text = "Extra-coarse bubble",
                             side = "right"
                         ),
                         x = -0.2,
                         y = 0.5,
                         len = 0.5
                     )
                 ),
                 showlegend = FALSE) %>%
    layout(
        scene = list(
            xaxis = list(title = "Time"),
            yaxis = list(title = expression(ln(C[s] - C[t]))),
            zaxis = list(title = "Oxygenation capacity")
        )
    )
ecb3d
cb3d <- cb %>%
    plot_ly(data = ., x = ~time, y = ~kla, z = ~oxCap,
            type = "scatter3d",
            mode = "lines",
            line = list(
                color = ~oxCap,
                colorscale = "Cividis",
                width = 9,
                showscale = TRUE,
                colorbar = list(
                    orientation = "h",
                    x = 0.5,
                    y = 1,
                    len = 0.5,
                    title = list(
                        text = "Coarse bubble",
                        side = "top"
                    )
                )
            ),
            showlegend = FALSE) %>%
    layout(
        scene = list(
            xaxis = list(title = "Time"),
            yaxis = list(title = expression(ln(C[s] - C[t]))),
            zaxis = list(title = "Oxygenation capacity")
        )
    )
cb3d
fb3d <- plot_ly(data = fb, x = ~time, y = ~kla, z = ~oxCap,
                 type = "scatter3d",
                 mode = "lines",
                 line = list(
                     color = ~oxCap,
                     colorscale = "YlOrRd",
                     width = 9,
                     showscale = TRUE,
                     colorbar = list(
                         title = list(
                             text = "Fine bubble",
                             side = "right"
                         ),
                         len = 0.5
                     )
                 ),
                 showlegend = FALSE) %>%
    layout(
        scene = list(
            xaxis = list(title = "Time"),
            yaxis = list(title = expression(ln(C[s] - C[t]))),
            zaxis = list(title = "Oxygenation capacity")
        )
    )
fb3d

combined_plot <- subplot(
    ecb3d, cb3d, fb3d,
    nrows = 3,  # Una fila para que estén uno al lado del otro
    margin = 0.02,  # Reducir el margen entre gráficos
    shareX = FALSE,  # No compartir ejes X
    shareY = FALSE   # No compartir ejes Y
)


combined_plot
save(difDatos, file = "difDatos.RData")