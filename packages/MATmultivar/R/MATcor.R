# Archivo: R/MATcor.R

#' MATcor: Matriz de correlacion grafica
#'
#' Esta funcion crea una matriz de correlacion grafica entre variables metricas de un dataframe.
#' Permite seleccionar un subconjunto especifico de variables.
#'
#' @param data Dataframe con los datos.
#' @param ... Variables especificas (sin comillas) a incluir en la matriz de correlacion.
#' @return Devuelve (y ademas guarda en el entorno global) una lista con el grafico de correlaciones.
#' @importFrom rlang quos
#' @export
MATcor <- function(data, ...) {
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Falta el paquete 'GGally'. Inst\u00e1lalo con install.packages('GGally').", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Falta el paquete 'dplyr'. Inst\u00e1lalo con install.packages('dplyr').", call. = FALSE)
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Falta el paquete 'rlang'. Inst\u00e1lalo con install.packages('rlang').", call. = FALSE)
  }

  variables <- rlang::quos(...)

  data_used <- if (length(variables) > 0) {
    dplyr::select(data, !!!variables)
  } else {
    dplyr::select(data, dplyr::where(is.numeric))
  }

  if (ncol(data_used) < 2) {
    stop("El dataframe debe contener al menos dos variables num\u00e9ricas para calcular la correlaci\u00f3n.", call. = FALSE)
  }

  corr_plot <- GGally::ggpairs(
    data_used,
    lower = list(
      continuous = GGally::wrap("cor", size = 4.5, method = "pearson", stars = TRUE)
    ),
    title = "Matriz de Correlaci\u00f3n"
  )

  # --- Compatibilidad con gu\u00eda/pr\u00e1cticas ---
  # La gu\u00eda y los PDFs del curso asumen que la funci\u00f3n crea un objeto en el
  # Global Environment con el nombre: <data>_correlaciones_info
  df_name <- deparse(substitute(data))
  out <- list(correlaciones = corr_plot, data = data_used)
  assign(paste0(df_name, "_correlaciones_info"), out, envir = .GlobalEnv)

  out
}
