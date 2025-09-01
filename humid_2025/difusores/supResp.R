libs <- c( "readxl", "tidyverse", "datapasta", "clipr", "this.path", "plotly")
for (lib in libs) {
    library(lib, character.only = TRUE)
}


setwd(find_dir("difusores"))
load("difusores_dats.RData")


archivo <- "dif_supResp.xlsx"
hojas <- excel_sheets(archivo)

datos <- tibble()
for (i in hojas) {
    temp <- read_xlsx(archivo, sheet = i) %>%
        mutate(bubble = i)
    datos <- bind_rows(datos, temp)
}

datos <-  datos %>%
    mutate(capOxigen = rowMeans(across(-c(time, bubble)), na.rm = TRUE))

datos %>%
    pivot_longer(
        cols = -c(time, bubble),
        names_to = "variable",
        values_to = "value"
    ) %>%
    mutate(bubble = case_when(
        bubble == "cb" ~ "Coarse bubble",
        bubble == "ecb" ~ "Extra-coarse bubble",
        bubble == "fb" ~ "Fine bubble",
        TRUE ~ variable
    )) %>%
    ggplot(aes(x = time, y = value, color = variable, group = variable)) +
    geom_line(aes(linetype = variable), linewidth = 1.5)+
    facet_wrap(
        vars(bubble),
        scales = "free_y"
    )

ecb <- bindT %>%
    dplyr::filter(cat == "ecb") %>%
    drop_na() %>%
    dplyr::select(time, capOxigen, do)
cb <- bindT %>%
    dplyr::filter(cat == "cb") %>%
    drop_na() %>%
    dplyr::select(time, capOxigen, do)

fb <- bindT %>%
    dplyr::filter(cat == "fb") %>%
    drop_na() %>%
    dplyr::select(time, capOxigen, do)

ecb3d <- plot_ly(data = ecb, x = ~time, y = ~do, z = ~capOxigen,
        type = "scatter3d",
        mode = "lines",
        line = list(
            color = ~capOxigen,
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
            yaxis = list(title = "Dissolved oxygen"),
            zaxis = list(title = "Oxygenation capacity")
        )
    )
ecb3d
cb3d <- plot_ly(data = cb, x = ~time, y = ~do, z = ~capOxigen,
        type = "scatter3d",
        mode = "lines",
        line = list(
            color = ~capOxigen,
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
            yaxis = list(title = "Dissolved oxygen"),
            zaxis = list(title = "Oxygenation capacity")
        )
    )
cb3d

fb3d <- plot_ly(data = fb, x = ~time, y = ~do, z = ~capOxigen,
        type = "scatter3d",
        mode = "lines",
        line = list(
            color = ~capOxigen,
            colorscale = "Hot",
            width = 9,
            showscale = TRUE,
            colorbar = list(
                len = 0.5,
                title = list(
                    text = "Fine bubble",
                    side = "right"
                )
            )
        ),
        showlegend = FALSE) %>%
    layout(
        scene = list(
            xaxis = list(title = "Time"),
            yaxis = list(title = "Dissolved oxygen"),
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

subplot(ecb3d, cb3d)


# Instalar kaleido
install.packages("reticulate")
reticulate::install_miniconda()
reticulate::conda_install("r-reticulate", "python-kaleido")

# Exportar con save_image
plotly::save_image(ecb3d, "ecb3d_plot.png", width = 800, height = 600)