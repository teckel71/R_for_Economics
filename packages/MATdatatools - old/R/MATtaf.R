#' MATtaf: Tabla de frecuencias agrupadas en intervalos
#'
#' Esta función crea una tabla con la distribución de frecuencias agrupadas en intervalos
#' para una variable numérica de un dataframe. Si no se especifica el número de intervalos,
#' se utiliza la regla de Freedman-Diaconis para determinarlo automáticamente. Además, genera un histograma
#' cuyos intervalos coinciden con los de la tabla.
#'
#' @param data Dataframe que contiene los datos.
#' @param variable Variable numérica para la que se calculará la distribución de frecuencias.
#' @param breaks (Opcional) Número de intervalos deseados. Si no se especifica, se calcula automáticamente con Freedman-Diaconis.
#' @return Una lista con la tabla de frecuencias y un histograma, guardada en el entorno global
#'         con el nombre "<variable>_intervalos_frecuencia".
#' @import dplyr knitr kableExtra ggplot2
#' @examples
#' \dontrun{
#' # Crear una tabla de frecuencias para la variable mpg en mtcars
#' MATtaf(data = mtcars, variable = mpg)
#' }
#' @export
MATtaf <- function(data, variable, breaks = NULL) {
  # Verificar e instalar paquetes necesarios
  packages <- c("dplyr", "knitr", "kableExtra", "ggplot2")
  lapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  })
  
  # Cargar paquetes
  library(dplyr)
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  
  # Asegurarse de que la variable sea tratada como nombre
  variable <- deparse(substitute(variable))
  
  # Verificar que la variable existe en el dataframe
  if (!variable %in% colnames(data)) {
    stop(paste("La variable", variable, "no existe en el dataframe."))
  }
  
  # Ordenar el dataframe por la variable
  data <- data %>% arrange(.data[[variable]])
  
  # Determinar el número de intervalos si no se especifica
  if (is.null(breaks)) {
    iqr <- IQR(data[[variable]], na.rm = TRUE)
    bin_width <- 2 * iqr / (nrow(data)^(1/3))
    breaks <- ceiling((max(data[[variable]], na.rm = TRUE) - min(data[[variable]], na.rm = TRUE)) / bin_width)
  }
  
  # Crear los intervalos
  data$intervalos <- cut(data[[variable]], breaks = breaks, include.lowest = TRUE)
  
  # Calcular las frecuencias
  conteo_intervalos <- table(data$intervalos)
  
  # Convertir a dataframe
  conteo_intervalos_df <- as.data.frame(conteo_intervalos)
  colnames(conteo_intervalos_df) <- c("Intervalo", "Frecuencia")
  
  # Calcular la frecuencia total
  N_agre <- sum(conteo_intervalos_df$Frecuencia)
  
  # Calcular frecuencias acumuladas y relativas
  conteo_intervalos_df <- conteo_intervalos_df %>%
    mutate(
      Frecuencia_acum = cumsum(Frecuencia),
      Frecuencia_R = Frecuencia / N_agre,
      Frecuencia_R_acum = cumsum(Frecuencia_R)
    )
  
  # Crear la tabla en formato kable
  tabla <- conteo_intervalos_df %>%
    kable(
      caption = paste("Distribución de frecuencias agrupadas en intervalos de", variable),
      col.names = c(
        "Intervalo",
        "Frecuencia absoluta n(i)",
        "Frecuencia absoluta acum. N(i)",
        "Frecuencia relativa f(i)",
        "Frecuencia relativa acum. F(i)"
      ),
      format.args = list(decimal.mark = ".", digits = 2)
    ) %>%
    kable_styling(
      full_width = F,
      bootstrap_options = c("striped", "bordered", "condensed"),
      position = "center",
      font_size = 11
    ) %>%
    row_spec(0, bold = TRUE, align = "c") %>%
    row_spec(1:(nrow(conteo_intervalos_df)), bold = FALSE, align = "c")
  
  # Crear el histograma
  histograma <- ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(bins = breaks, colour = "red", fill = "orange", alpha = 0.7) +
    geom_vline(xintercept = mean(data[[variable]], na.rm = TRUE), color = "darkblue", size = 1.2, alpha = 0.8) +
    ggtitle(paste("Histograma de", variable)) +
    xlab(variable) +
    ylab("Frecuencia")
  
  # Guardar la tabla y el histograma en el entorno global
  nombre_lista <- paste0(variable, "_intervalos_frecuencia")
  assign(nombre_lista, list(tabla = tabla, histograma = histograma), envir = .GlobalEnv)
  
  # Mensaje de confirmación
  message(paste("La tabla de frecuencias agrupadas y el histograma se guardaron en el entorno global con el nombre:", nombre_lista))
}