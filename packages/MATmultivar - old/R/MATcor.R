# Archivo: R/MATcor.R

#' MATcor: Matriz de correlación gráfica
#'
#' Esta función crea una matriz de correlación gráfica entre las variables métricas de un dataframe.
#' Permite seleccionar un subconjunto específico de variables.
#'
#' @param data Dataframe con los datos.
#' @param ... Variables específicas (sin comillas) a incluir en la matriz de correlación.
#' @return Un gráfico de correlación guardado en el entorno global.
#' @import GGally dplyr
#' @export
MATcor <- function(data, ...) {
  if (!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  
  library(GGally)
  library(dplyr)
  
  df_name <- deparse(substitute(data))
  variables <- quos(...)
  
  if (length(variables) > 0) {
    data <- data %>% select(!!!variables)
  } else {
    data <- data %>% select(where(is.numeric))
  }
  
  if (ncol(data) < 2) {
    stop("El dataframe debe contener al menos dos variables métricas para calcular la correlación.")
  }
  
  corr_plot <- ggpairs(
    data,
    lower = list(continuous = wrap("cor", size = 4.5, method = "pearson", stars = TRUE)),
    title = "Matriz de Correlación"
  )
  
  nombre_lista <- paste0(df_name, "_correlaciones_info")
  assign(nombre_lista, list(correlaciones = corr_plot), envir = .GlobalEnv)
  message(paste("La matriz de correlación gráfica se guardó como:", nombre_lista))
}
