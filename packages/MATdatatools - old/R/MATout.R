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
#'       \item \code{Outliers_Table}: Una tabla en formato HTML con los outliers detectados.
#'       \item \code{Boxplot}: Un gráfico de caja de la variable analizada.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Crear un dataframe de ejemplo
#' datos <- data.frame(altura = c(160, 165, 170, 200, 180, 175, 400))
#'
#' # Detectar outliers en la variable altura
#' MATout(datos, altura)
#' }
#'
#' @import dplyr ggplot2 knitr kableExtra
#' @export
MATout <- function(data, variable) {
  # Verificar si los paquetes necesarios están instalados y cargarlos
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
  if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")

  library(dplyr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)

  # Convertir la variable a texto si no lo es
  variable <- deparse(substitute(variable))

  # Comprobar que el dataframe y la variable existen
  if (!is.data.frame(data)) stop("El primer argumento debe ser un dataframe.")
  if (!variable %in% colnames(data)) stop("La variable especificada no existe en el dataframe.")

  # Verificar si hay valores NA en la variable
  if (any(is.na(data[[variable]]))) {
    stop("El análisis no se puede realizar porque existen valores NA en la variable.")
  }

  # Calcular Q1, Q3 e IQR
  Q1 <- quantile(data[[variable]], 0.25)
  Q3 <- quantile(data[[variable]], 0.75)
  IQR_value <- Q3 - Q1

  # Determinar los límites para los outliers
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value

  # Filtrar outliers
  outliers <- data %>%
    filter(data[[variable]] < lower_bound | data[[variable]] > upper_bound)

  # Filtrar datos sin outliers
  data_clean <- data %>%
    filter(data[[variable]] >= lower_bound & data[[variable]] <= upper_bound)

  # Crear nombre del nuevo dataframe y lista
  new_data_name <- paste0(deparse(substitute(data)), "_so")
  info_list_name <- paste0(new_data_name, "_info")

  # Generar gráfico de caja
  boxplot <- ggplot(data, aes(y = .data[[variable]])) +
    geom_boxplot(fill = "orange") +
    ggtitle(paste0("Box-Plot de la variable ", variable),
            subtitle = "Generado con la función MATout") +
    ylab(variable)

  # Crear tabla con los outliers
  if (nrow(outliers) > 0) {
    outliers_table <- outliers %>%
      select(all_of(variable)) %>%
      kable(format = "html", caption = "Casos considerados outliers") %>%
      kable_styling(bootstrap_options = "striped", full_width = F)
  } else {
    outliers_table <- "No se identificaron outliers."
  }

  # Guardar resultados en la lista
  assign(new_data_name, data_clean, envir = .GlobalEnv)
  assign(info_list_name, list(Outliers_Table = outliers_table, Boxplot = boxplot), envir = .GlobalEnv)

  message(paste0("Análisis completado. Los resultados se guardaron como: ", new_data_name, " y ", info_list_name))
}
