#' Identificación de Outliers Univariados
#'
#' Esta función analiza una variable continua en un dataframe y detecta outliers utilizando el rango intercuartílico (IQR).
#' Devuelve un dataframe sin los outliers y genera un gráfico de caja y una tabla con los casos identificados como outliers.
#'
#' @param data Dataframe que contiene los datos a analizar.
#' @param variable Variable continua a analizar (sin comillas).
#'
#' @return
#' La función crea dos objetos en el entorno global:
#' \itemize{
#'   \item Un nuevo dataframe que contiene los datos sin outliers, nombrado como \code{<nombre_original>_so}.
#'   \item Una lista de información nombrada como \code{<nombre_original>_so_info}, que incluye:
#'     \itemize{
#'       \item \code{Outliers_Table}: Una tabla en formato HTML con los outliers detectados (o un texto si no hay).
#'       \item \code{Boxplot}: Un gráfico de caja de la variable analizada.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' datos <- data.frame(altura = c(160, 165, 170, 200, 180, 175, 400))
#' MATout(datos, altura)
#' # Resultados: datos_so y datos_so_info en el entorno global
#' }
#'
#' @export
MATout <- function(data, variable) {

  # Validaciones
  if (!is.data.frame(data)) {
    stop("El primer argumento debe ser un dataframe.")
  }

  variable <- deparse(substitute(variable))

  if (!variable %in% colnames(data)) {
    stop("La variable especificada no existe en el dataframe.")
  }

  # Verificar NA en la variable (mantenemos el comportamiento original)
  if (any(is.na(data[[variable]]))) {
    stop("El análisis no se puede realizar porque existen valores NA en la variable.")
  }

  # Q1, Q3 e IQR
  Q1 <- stats::quantile(data[[variable]], 0.25, names = FALSE)
  Q3 <- stats::quantile(data[[variable]], 0.75, names = FALSE)
  IQR_value <- Q3 - Q1

  # Límites outliers
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value

  # Outliers y datos limpios
  outliers <- dplyr::filter(data, .data[[variable]] < lower_bound | .data[[variable]] > upper_bound)
  data_clean <- dplyr::filter(data, .data[[variable]] >= lower_bound & .data[[variable]] <= upper_bound)

  # Nombres de salida
  original_name <- deparse(substitute(data))
  new_data_name <- paste0(original_name, "_so")
  info_list_name <- paste0(new_data_name, "_info")

  # Boxplot
  boxplot <- ggplot2::ggplot(data, ggplot2::aes(y = .data[[variable]])) +
    ggplot2::geom_boxplot(fill = "orange") +
    ggplot2::ggtitle(
      paste0("Box-Plot de la variable ", variable),
      subtitle = "Generado con la función MATout"
    ) +
    ggplot2::ylab(variable)

  # Tabla outliers (HTML)
  if (nrow(outliers) > 0) {
    outliers_table <- outliers |>
      dplyr::select(dplyr::all_of(variable)) |>
      knitr::kable(format = "html", caption = "Casos considerados outliers") |>
      kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)
  } else {
    outliers_table <- "No se identificaron outliers."
  }

  # Guardar resultados en el entorno global
  assign(new_data_name, data_clean, envir = .GlobalEnv)
  assign(info_list_name, list(Outliers_Table = outliers_table, Boxplot = boxplot), envir = .GlobalEnv)

  message(paste0("Análisis completado. Los resultados se guardaron como: ", new_data_name, " y ", info_list_name))

  invisible(new_data_name)
}
