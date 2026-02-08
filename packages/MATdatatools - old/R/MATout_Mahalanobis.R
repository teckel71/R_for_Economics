#' Identificación de Outliers Multivariados mediante la Distancia de Mahalanobis
#'
#' Esta función identifica outliers multivariados utilizando la distancia de Mahalanobis.
#' Genera un dataframe sin los outliers y proporciona un gráfico de caja y una tabla con los casos detectados como outliers.
#'
#' @param data Dataframe que contiene los datos a analizar.
#' @param ... Variables a incluir en el análisis (sin comillas, separadas por comas). Si no se especifican, se procesarán todas las variables numéricas del dataframe.
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
#' # Crear un dataframe de ejemplo
#' datos <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
#'
#' # Detectar outliers multivariados considerando todas las variables numéricas
#' MATout_Mahalanobis(datos)
#'
#' # Detectar outliers multivariados considerando solo x e y
#' MATout_Mahalanobis(datos, x, y)
#' }
#'
#' @import dplyr ggplot2 knitr kableExtra
#' @export
MATout_Mahalanobis <- function(data, ...) {
  # Verificar si los paquetes necesarios están instalados y cargarlos
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
  if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")

  library(dplyr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)

  # Obtener las variables
  variables <- sapply(substitute(list(...))[-1], deparse)
  
  # Si no se especifican variables, usar todas las numéricas
  if (length(variables) == 0) {
    variables <- names(select(data, where(is.numeric)))
  }
  
  # Obtener el nombre del dataframe de entrada
  original_name <- deparse(substitute(data))

  # Comprobar que las variables existen en el dataframe
  if (!all(variables %in% colnames(data))) {
    stop("Algunas de las variables seleccionadas no existen en el dataframe.")
  }

  # Verificar si hay valores NA en las variables seleccionadas
  if (any(is.na(data[variables]))) {
    stop("El análisis no se puede realizar porque existen valores NA en las variables seleccionadas.")
  }

  # Calcular la distancia de Mahalanobis
  cov_matrix <- cov(data[variables])
  if (det(cov_matrix) == 0) stop("La matriz de covarianza es singular. No se puede calcular la distancia de Mahalanobis.")
  mahal_dist <- mahalanobis(data[variables], colMeans(data[variables]), cov_matrix)
  data <- data %>% mutate(Mahalanobis_Distance = mahal_dist)

  # Calcular Q1, Q3 e IQR de la distancia de Mahalanobis
  Q1 <- quantile(data$Mahalanobis_Distance, 0.25)
  Q3 <- quantile(data$Mahalanobis_Distance, 0.75)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value

  # Filtrar outliers
  outliers <- data %>%
    filter(Mahalanobis_Distance < lower_bound | Mahalanobis_Distance > upper_bound)

  # Filtrar datos sin outliers (con todas las variables originales)
  data_clean <- data %>%
    filter(Mahalanobis_Distance >= lower_bound & Mahalanobis_Distance <= upper_bound) %>%
    select(-Mahalanobis_Distance)  # Eliminar la columna de Mahalanobis

  # Crear nombres para el nuevo dataframe y lista
  new_data_name <- paste0(original_name, "_so")
  info_list_name <- paste0(new_data_name, "_info")

  # Generar gráfico de caja
  selected_vars <- paste(variables, collapse = ", ")
  boxplot <- ggplot(data, aes(y = Mahalanobis_Distance)) +
    geom_boxplot(fill = "orange") +
    ggtitle(paste0("Box-Plot de la distancia de Mahalanobis de las variables: ", selected_vars),
            subtitle = "Generado con la función MATout_Mahalanobis") +
    ylab("Distancia de Mahalanobis")

  # Crear tabla con los outliers
  if (nrow(outliers) > 0) {
    outliers_table <- outliers %>%
      select(c("Mahalanobis_Distance", all_of(variables))) %>%
      kable(format = "html", caption = "Casos considerados outliers") %>%
      kable_styling(bootstrap_options = "striped", full_width = F)
  } else {
    outliers_table <- "No se identificaron outliers."
  }

  # Guardar el nuevo dataframe y la lista en el Global Environment
  assign(new_data_name, data_clean, envir = .GlobalEnv)
  assign(info_list_name, list(Outliers_Table = outliers_table, Boxplot = boxplot), envir = .GlobalEnv)

   # Eliminar objetos temporales para mantener limpio el entorno global
  rm(list = ls()[!ls() %in% c(new_data_name, info_list_name)], envir = .GlobalEnv)
  
  # Mensaje de confirmación
  message(paste0("Análisis completado. Los resultados se guardaron como: ", new_data_name, " y ", info_list_name))
}
