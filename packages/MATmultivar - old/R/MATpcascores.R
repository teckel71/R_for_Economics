#' MATpcascores: Obtención de puntuaciones de componentes principales
#'
#' Calcula y devuelve puntuaciones de componentes principales de un PCA previamente realizado.
#'
#' @param data Dataframe de entrada (puede ser diferente al original).
#' @param pca_object Objeto resultante de prcomp().
#' @param Nscores Número de componentes principales a extraer (por defecto todas).
#' @param Rank Número de casos a mostrar en tablas y gráficos (por defecto 10).
#' @return Lista con el dataframe de puntuaciones, tabla formateada y gráfico.
#' @import dplyr ggplot2 knitr kableExtra
#' @export
MATpcascores <- function(data, pca_object, Nscores = NULL, Rank = 10) {
  if (!"prcomp" %in% class(pca_object)) stop("El objeto PCA no es válido.")
  
  # Extraer nombres de variables usadas en el PCA
  pca_vars <- rownames(pca_object$rotation)
  
  # Verificar que las variables estén en el nuevo dataframe
  missing_vars <- setdiff(pca_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(paste("Faltan variables en el nuevo dataframe:", paste(missing_vars, collapse = ", ")))
  }
  
  # Calcular puntuaciones de los nuevos casos
  scores <- predict(pca_object, newdata = data[pca_vars])
  
  # Definir número de componentes a extraer
  if (is.null(Nscores)) {
    Nscores <- ncol(scores)  # Usar todas las componentes disponibles
  } else {
    Nscores <- min(Nscores, ncol(scores))  # Asegurar que no exceda el máximo
  }
  
  scores <- as.data.frame(scores[, 1:Nscores, drop = FALSE])
  data_with_scores <- cbind(data, scores)
  
  # Manejo de casos con valores faltantes en las puntuaciones
  na_cases <- sum(apply(scores, 1, function(x) any(is.na(x))))
  if (na_cases > 0) {
    message(paste("No se pudieron calcular las puntuaciones para", na_cases, "casos."))
  }
  
  # Generar nombre de la lista de salida
  data_name <- deparse(substitute(data))
  pca_name <- deparse(substitute(pca_object))
  pca_name <- sub("\\$.*", "", pca_name)  # Eliminar cualquier signo de dólar
  output_name <- paste0("scores_", data_name, "_", pca_name, "_info")
  
  # Generar tablas y gráficos para cada componente
  resultados <- list()
  
  for (i in 1:Nscores) {
    comp_name <- colnames(scores)[i]
    sorted_data <- data_with_scores %>% arrange(desc(.data[[comp_name]])) %>% slice(1:Rank)
    
    # Tabla con kable
    tabla <- sorted_data %>%
      select(all_of(c(comp_name, pca_vars))) %>%
      kable(caption = paste("Top", Rank, "casos para", comp_name), format.args = list(decimal.mark = ".", digits = 4)) %>%
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "bordered"))
    
    # Gráfico de barras
    grafico <- ggplot(sorted_data, aes(x = reorder(rownames(sorted_data), .data[[comp_name]]), y = .data[[comp_name]])) +
      geom_bar(stat = "identity", color = "red", fill = "orange", alpha = 0.6) +
      geom_text(aes(label = sprintf("%.3f", .data[[comp_name]])), hjust = 1.2, size = 3, color = "black") +
      coord_flip() +
      labs(title = paste("Top", Rank, "casos en", comp_name), x = "Casos", y = paste("Valor en", comp_name)) +
      theme_minimal()
    
    resultados[[comp_name]] <- list(tabla = tabla, grafico = grafico)
  }
  
  output_list <- list(scores_df = data_with_scores, resultados = resultados)
  
  assign(output_name, output_list, envir = .GlobalEnv)
  return(output_list)
}
