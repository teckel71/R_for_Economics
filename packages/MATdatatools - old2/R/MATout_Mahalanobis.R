#' Identificación de Outliers Multivariados mediante la Distancia de Mahalanobis
#'
#' Esta función identifica outliers multivariados utilizando la distancia de Mahalanobis.
#' Genera un dataframe sin los outliers y proporciona un gráfico de caja y una tabla con los casos detectados.
#'
#' @param data Dataframe que contiene los datos a analizar.
#' @param ... Variables a incluir en el análisis (sin comillas, separadas por comas).
#'
#' @return
#' La función crea dos objetos en el entorno global:
#' \itemize{
#'   \item Un nuevo dataframe sin los outliers, nombrado como \code{<nombre_original>_so}.
#'   \item Una lista de información nombrada como \code{<nombre_original>_so_info}, que incluye:
#'     \itemize{
#'       \item \code{Outliers_Table}: Una tabla en formato HTML con los outliers detectados.
#'       \item \code{Boxplot}: Un gráfico de caja de las distancias de Mahalanobis.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' datos <- data.frame(x = rnorm(100), y = rnorm(100))
#' MATout_Mahalanobis(datos, x, y)
#' # Resultados: datos_so y datos_so_info en el entorno global
#' }
#'
#' @export
MATout_Mahalanobis <- function(data, ...) {

  # Validaciones básicas
  if (!is.data.frame(data)) {
    stop("El primer argumento (`data`) debe ser un dataframe.")
  }

  # Capturar variables desde ...
  variables <- as.character(substitute(list(...)))[-1]
  if (length(variables) == 0) {
    stop("Debes indicar al menos una variable, por ejemplo: MATout_Mahalanobis(df, x, y).")
  }

  original_name <- deparse(substitute(data))

  # Verificar que las variables existen
  if (!all(variables %in% colnames(data))) {
    faltan <- setdiff(variables, colnames(data))
    stop(
      paste0(
        "Algunas variables no existen en el dataframe: ",
        paste(faltan, collapse = ", ")
      )
    )
  }

  # Verificar que no hay NA en las variables seleccionadas
  if (any(is.na(data[variables]))) {
    stop("El análisis no se puede realizar porque existen valores NA en las variables seleccionadas.")
  }

  # Calcular matriz de covarianza y comprobar singularidad
  cov_matrix <- stats::cov(data[variables])
  if (det(cov_matrix) == 0) {
    stop("La matriz de covarianza es singular. No se puede calcular la distancia de Mahalanobis.")
  }

  # Distancias de Mahalanobis
  mahal_dist <- stats::mahalanobis(
    x = data[variables],
    center = colMeans(data[variables]),
    cov = cov_matrix
  )

  # Añadir distancia a una copia temporal
  data2 <- dplyr::mutate(data, Mahalanobis_Distance = mahal_dist)

  # Criterio IQR sobre la distancia (manteniendo tu enfoque original)
  Q1 <- stats::quantile(data2$Mahalanobis_Distance, 0.25)
  Q3 <- stats::quantile(data2$Mahalanobis_Distance, 0.75)
  IQR_value <- Q3 - Q1

  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value

  # Outliers
  outliers <- dplyr::filter(
    data2,
    Mahalanobis_Distance < lower_bound | Mahalanobis_Distance > upper_bound
  )

  # Datos sin outliers (con todas las variables originales; eliminando la columna temporal)
  data_clean <- dplyr::filter(
    data2,
    Mahalanobis_Distance >= lower_bound & Mahalanobis_Distance <= upper_bound
  ) |>
    dplyr::select(-Mahalanobis_Distance)

  # Nombres de salida en el entorno global (como en tu guía)
  new_data_name <- paste0(original_name, "_so")
  info_list_name <- paste0(new_data_name, "_info")

  # Boxplot de distancias
  selected_vars <- paste(variables, collapse = ", ")
  boxplot <- ggplot2::ggplot(data2, ggplot2::aes(y = Mahalanobis_Distance)) +
    ggplot2::geom_boxplot(fill = "orange") +
    ggplot2::ggtitle(
      paste0("Box-Plot de la distancia de Mahalanobis: ", selected_vars),
      subtitle = "Generado con la función MATout_Mahalanobis"
    ) +
    ggplot2::ylab("Distancia de Mahalanobis")

  # Tabla HTML de outliers
  if (nrow(outliers) > 0) {
    outliers_table <- outliers |>
      dplyr::select(dplyr::all_of(c("Mahalanobis_Distance", variables))) |>
      knitr::kable(format = "html", caption = "Casos considerados outliers") |>
      kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)
  } else {
    outliers_table <- "No se identificaron outliers."
  }

  # Guardar en .GlobalEnv
  assign(new_data_name, data_clean, envir = .GlobalEnv)
  assign(info_list_name, list(Outliers_Table = outliers_table, Boxplot = boxplot), envir = .GlobalEnv)

  # Mensaje
  message(
    paste0(
      "Análisis completado. Los resultados se guardaron como: ",
      new_data_name, " y ", info_list_name
    )
  )

  invisible(new_data_name)
}