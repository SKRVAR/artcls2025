library(showtext)
font_add(family = "Calibri", regular = "Calibri.ttf")
theme_set(theme_minimal()) # fija el tema minimalista comom predeterminado
theme_update(
    plot.title = element_blank(), # Quitar el título
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(color = "grey40", size = 13),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14, color = "grey40"),
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    panel.background = element_rect(fill = "grey98", color = "grey98"),
    legend.key.size = unit(0.7, "cm"),
    legend.key.spacing.y = unit(0.15, "cm"),
    axis.line = element_line(colour = "gray20", linewidth = 0.2),
    axis.ticks = element_line(colour = "gray20", linewidth = 0.2),
    strip.text = element_text(size = 15, color = "grey40", face = "bold")
)

find_dir <- function(dir_name, start_path = getwd(), max_depth = 10) {
    # Verificar y cargar fs
    if (!requireNamespace("fs", quietly = TRUE)) {
        install.packages("fs")
    }
    library(fs)

    # PASO 1: Subir un nivel desde el directorio actual
    parent_dir <- fs::path_dir(start_path)

    # PASO 2: Encontrar el directorio más profundo desde ese nivel superior
    deepest_dir <- parent_dir
    max_depth_reached <- -1

    explore_dirs <- function(path, depth = 0) {
        if (depth >= max_depth) return()
        if (depth > max_depth_reached) {
            deepest_dir <<- path
            max_depth_reached <<- depth
        }

        # Intentar listar subdirectorios
        tryCatch({
            subdirs <- fs::dir_ls(path, type = "directory", recurse = FALSE)
            for (subdir in subdirs) {
                explore_dirs(subdir, depth + 1)
            }
        }, error = function(e) {})
    }

    # Iniciar exploración desde el nivel superior
    explore_dirs(parent_dir)

    # PASO 3: Buscar hacia arriba desde el directorio más profundo
    current <- deepest_dir
    for (i in 1:max_depth) {
        # Verificar si este directorio es el buscado
        if (fs::path_file(current) == dir_name) {
            return(current)
        }

        # Verificar si contiene el directorio buscado
        target_path <- fs::path(current, dir_name)
        if (fs::dir_exists(target_path)) {
            return(target_path)
        }

        # Subir un nivel
        parent <- fs::path_dir(current)
        if (parent == current) break  # Ya llegamos a la raíz
        current <- parent
    }

    return(NULL)  # No se encontró
}

