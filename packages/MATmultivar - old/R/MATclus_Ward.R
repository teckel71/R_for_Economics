# Archivo: R/MATclus_Ward.R

#' MATclus_Ward: Clustering jerárquico con método de Ward
#'
#' Realiza un análisis de clúster jerárquico usando el método de Ward y permite determinar el número óptimo de grupos.
#'
#' @param data Dataframe con los datos.
#' @param ... Variables específicas (sin comillas) a incluir en el clustering.
#' @param k Número de clusters a formar.
#' @param silueta Booleano, si TRUE, calcula el número óptimo de clusters.
#' @return Lista con dendrograma, resumen de grupos y gráficos.
#' @import factoextra ggplot2 dplyr cluster knitr kableExtra patchwork
#' @export
MATclus_Ward <- function(data, ..., k = 0, silueta = FALSE) {
  library(factoextra)
  library(ggplot2)
  library(dplyr)
  library(cluster)
  library(knitr)
  library(kableExtra)
  library(patchwork)
  
  # Seleccionar variables métricas
  vars <- rlang::quos(...)
  if (length(vars) == 0) {
    data_selected <- data %>% select(where(is.numeric))
  } else {
    data_selected <- data %>% select(!!!vars)
  }
  
  # Tipificar las variables
  data_scaled <- scale(data_selected)
  
  # Calcular la matriz de distancias euclídeas
  dist_matrix <- dist(data_scaled, method = "euclidean")
  
  # Crear lista para almacenar resultados
  cluster_Ward_info <- list()
  
  # Visualizar la matriz de distancias con gráfico de temperatura
  cluster_Ward_info$heatmap <- fviz_dist(dist_matrix)
  
  # Aplicar el método de clustering jerárquico de Ward
  hclust_result <- hclust(dist_matrix, method = "ward.D2")
  cluster_Ward_info$hclust <- hclust_result
  
  # Determinar el número óptimo de clusters con el método de la silueta si es necesario
  if (silueta) {
    sil_result <- fviz_nbclust(data_scaled, FUN = hcut, method = "silhouette")
    sil_result <- sil_result + geom_vline(xintercept = which.max(sil_result$data$y), linetype = "dashed", color = "red")
    cluster_Ward_info$silhouette <- sil_result
  }
  
  # Función para crear composiciones de gráficos con patchwork
  create_patchwork <- function(plot_list) {
    n <- length(plot_list)
    if (n == 0) return(NULL)
    full_rows <- n %/% 4
    remaining <- n %% 4
    patchworks <- list()
    
    if (full_rows > 0) {
      for (i in seq(1, full_rows * 4, by = 4)) {
        patchworks <- c(patchworks, list((plot_list[[i]] + plot_list[[i+1]]) / 
                                           (plot_list[[i+2]] + plot_list[[i+3]])))
      }
    }
    
    if (remaining > 0) {
      last_plots <- plot_list[(full_rows * 4 + 1):n]
      empty_plots <- lapply(1:(4 - remaining), function(x) ggplot() + theme_void())
      last_patchwork <- do.call(patchwork::wrap_plots, c(last_plots, empty_plots))
      patchworks <- c(patchworks, list(last_patchwork))
    }
    return(patchworks)
  }
  
  # Visualizar dendrograma dependiendo del valor de k
  if (k == 0) {
    cluster_Ward_info$dendrogram <- fviz_dend(hclust_result, cex = 0.6, rect = FALSE, 
                                              labels_track_height = 5.5, 
                                              theme = theme_gray())
  } else {
    cluster_Ward_info$dendrogram <- fviz_dend(hclust_result, cex = 0.6, k = k, 
                                              k_colors = "black", labels_track_height = 5.5, 
                                              rect = TRUE, rect_border = "npg", rect_fill = TRUE, 
                                              theme = theme_gray())
    
    # Asignar cada observación a un grupo
    group_assignments <- cutree(hclust_result, k = k)
    
    # Crear dataframe con los datos originales más el grupo asignado
    data_with_groups <- data %>% 
      select(names(data_selected)) %>% 
      mutate(NOMBRE = rownames(data), GRUPO = factor(paste("GRUPO", group_assignments)))
    
    cluster_Ward_info$data_groups <- data_with_groups
    
    # Crear tabla resumen de los grupos con medias
    summary_table <- data_with_groups %>% 
      group_by(GRUPO) %>% 
      summarise(Obs = n(), across(where(is.numeric), ~ round(mean(.), 3)))
    
    cluster_Ward_info$group_summary <- kable(summary_table, format = "html", 
                                             caption = "GRUPOS Y CENTROIDES") %>% 
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "bordered", "condensed"), 
                    position = "center", font_size = 11)
    
    # Crear tablas individuales por grupo
    group_tables <- list()
    for (grupo in unique(data_with_groups$GRUPO)) {
      group_data <- data_with_groups %>% filter(GRUPO == grupo) %>% select(-GRUPO, -NOMBRE) %>% 
        mutate(across(where(is.numeric), ~ round(., 3)))
      group_tables[[as.character(grupo)]] <- kable(group_data, format = "html", 
                                                   caption = paste("Casos en", grupo)) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = c("striped", "bordered", "condensed"), 
                      position = "center", font_size = 11)
    }
    cluster_Ward_info$group_tables <- group_tables
    
    # Crear gráficos de barras comparativos de medias
    bar_plots <- list()
    variables <- colnames(data_selected)
    for (var in variables) {
      grafico <- ggplot(summary_table, aes_string(x = "GRUPO", y = var)) +
        geom_bar(stat = "identity", colour = "red", fill = "orange", alpha = 0.7) +
        ggtitle(paste0(var, ". Media por grupos."), subtitle = "Análisis de Clúster") +
        xlab("Grupo") +
        ylab(var)
      bar_plots[[paste0("barplot_", var)]] <- grafico
    }
    
    cluster_Ward_info$bar_plots <- bar_plots
    cluster_Ward_info$bar_patchworks <- create_patchwork(bar_plots)
    
    # Crear gráficos de dispersión con los centros de cada grupo
    scatter_plots <- list()
    variable_combinations <- combn(colnames(data_selected), 2, simplify = FALSE)
    
    for (comb in variable_combinations) {
      var1 <- comb[1]
      var2 <- comb[2]
      scatter_plot <- ggplot(data_with_groups, aes_string(x = var1, y = var2, color = "GRUPO")) +
        geom_point(alpha = 0.7) +
        geom_point(data = summary_table, aes_string(x = var1, y = var2, color = "GRUPO"), size = 2, shape = 16) +
        labs(title = paste("GRÁFICO", var1, "-", var2), subtitle = "Función: MATclus_Ward") +
        xlab(var1) +
        ylab(var2) +
        scale_color_brewer(palette = "Set1")
      scatter_plots[[paste0("scatter_", var1, "_", var2)]] <- scatter_plot
    }
    
    cluster_Ward_info$scatter_plots <- scatter_plots
    cluster_Ward_info$scatter_patchworks <- create_patchwork(scatter_plots)
  }
  
  return(cluster_Ward_info)
}
