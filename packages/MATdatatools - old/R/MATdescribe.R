#' MATdescribe: Análisis descriptivo y visual de una variable
#'
#' Esta función realiza un análisis descriptivo y visual detallado para una variable
#' de un dataframe, generando histogramas, densidad, boxplots y evaluando la normalidad con tres pruebas estadísticas.
#'
#' @param data Dataframe que contiene los datos.
#' @param variable Variable numérica a analizar (sin comillas).
#' @param bins Número de bins para el histograma. Si es 0, se usa el método Freedman-Diaconis.
#' @return Una lista con los resultados del análisis descriptivo, guardada en el entorno global con el nombre "<variable>_describe_info".
#' @import ggplot2 dplyr knitr kableExtra moments patchwork nortest
#' @examples
#' \dontrun{
#' # Ejemplo de uso
#' MATdescribe(data = mtcars, variable = mpg, bins = 10)
#' }
#' @export
MATdescribe <- function(data, variable, bins = 0) {
  # Verificar e instalar paquetes necesarios
  packages <- c("ggplot2", "dplyr", "knitr", "kableExtra", "moments", "patchwork", "nortest")
  lapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  })
  
  # Cargar paquetes
  library(ggplot2)
  library(dplyr)
  library(knitr)
  library(kableExtra)
  library(moments)
  library(patchwork)
  library(nortest)
  
  # Asegurarse de que la variable sea tratada como nombre
  variable <- deparse(substitute(variable))
  
  # Verificar que la variable existe en el dataframe
  if (!variable %in% colnames(data)) {
    stop(paste("La variable", variable, "no existe en el dataframe."))
  }
  
  # Determinar el número de bins
  if (bins == 0) {
    bins <- nclass.FD(data[[variable]])  # Método Freedman-Diaconis
  }
  
  # Histogram
  g1 <- ggplot(data = data, aes(x = .data[[variable]])) +
    geom_histogram(bins = bins, colour = "red", fill = "orange", alpha = 0.7) +
    geom_vline(xintercept = mean(data[[variable]], na.rm = TRUE), color = "darkblue", size = 1.2, alpha = 0.8) +
    ggtitle("Histograma") +
    xlab(variable) +
    ylab("Frecuencias")
  
  # Densidad
  g2 <- ggplot(data = data, aes(x = .data[[variable]])) +
    geom_density(colour = "red", fill = "orange", alpha = 0.7) +
    geom_vline(xintercept = mean(data[[variable]], na.rm = TRUE), color = "darkblue", size = 0.8, alpha = 0.8) +
    stat_function(fun = dnorm, args = list(mean = mean(data[[variable]], na.rm = TRUE), sd = sd(data[[variable]], na.rm = TRUE)), geom = "area", color = "darkblue", fill = "yellow", alpha = 0.2) +
    ggtitle("Gráfico de densidad vs curva normal") +
    xlab(variable) +
    ylab("Densidad")
  
  # Boxplot
  g3 <- ggplot(data = data, aes(x = "", y = .data[[variable]])) +
    geom_boxplot(color = "red", fill = "orange", outlier.shape = NA) +
    stat_summary(fun = "mean", geom = "point", size = 3, col = "darkblue") +
    geom_jitter(width = 0.1, size = 1, col = "darkred", alpha = 0.50) +
    ggtitle("Box-Plot") +
    ylab(variable)
  
  # QQ-Plot
  g4 <- ggplot(data = data, aes(sample = .data[[variable]])) +
    stat_qq(colour = "red") +
    stat_qq_line(colour = "darkblue") +
    ggtitle("QQ-Plot")
  
  # Resumen de gráficos
  resumen_graficos <- (g1 | g2) / (g3 | g4) +
    plot_annotation(
      title = paste("Análisis gráfico de", variable),
      subtitle = "Construido con MATdatatools"
    )
  
  # Estadísticos descriptivos
  estadisticos <- data %>% summarise(
    Media = mean(.data[[variable]], na.rm = TRUE),
    DT = abs(sd(.data[[variable]], na.rm = TRUE)),
    Mediana = median(.data[[variable]], na.rm = TRUE),
    Mínimo = min(.data[[variable]], na.rm = TRUE),
    Máximo = max(.data[[variable]], na.rm = TRUE),
    Asimetría = skewness(.data[[variable]], na.rm = TRUE),
    Curtosis = kurtosis(.data[[variable]], na.rm = TRUE) - 3
  )
  
  tabla_estadisticos <- estadisticos %>%
    kable(caption = paste("Principales Estadísticos de", variable),
          col.names = c("Media", "Desviación Típica", "Mediana", "Valor mínimo", "Valor máximo", "C. Asimetría Fisher", "C. Curtosis Fisher")) %>%
    kable_styling(full_width = FALSE, position = "center")
  
  # Pruebas de normalidad
  shapiro_test <- shapiro.test(data[[variable]])
  ks_test <- lillie.test(data[[variable]])
  ad_test <- ad.test(data[[variable]])
  
  normalidad <- data.frame(
    Prueba = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Anderson-Darling"),
    `p-valor` = round(c(shapiro_test$p.value, ks_test$p.value, ad_test$p.value), 3),
    Conclusión = ifelse(c(shapiro_test$p.value, ks_test$p.value, ad_test$p.value) > 0.05, "NORMALIDAD", "NO-NORMALIDAD")
  )
  
  tabla_normalidad <- normalidad %>%
    kable(caption = paste(variable, "- Pruebas de Normalidad"),
          col.names = c("Prueba", "p-valor", "Conclusión")) %>%
    kable_styling(full_width = FALSE, position = "center")
  
  # Crear la lista de resultados
  resultados <- list(
    grafico_resumen = resumen_graficos,
    estadisticos = tabla_estadisticos,
    normalidad = tabla_normalidad
  )
  
  # Guardar en el entorno global
  nombre_lista <- paste0(variable, "_describe_info")
  assign(nombre_lista, resultados, envir = .GlobalEnv)
  
  # Mensaje de confirmación
  message(paste("Los resultados del análisis descriptivo se guardaron en el entorno global con el nombre:", nombre_lista))
}