#' MATdescribe: Análisis descriptivo y visual de una variable
#'
#' Esta función realiza un análisis descriptivo y visual detallado para una variable
#' de un dataframe, generando histogramas, densidad, boxplots y evaluando la normalidad con tres pruebas estadísticas.
#'
#' @param data Dataframe que contiene los datos.
#' @param variable Variable numérica a analizar (sin comillas).
#' @param bins Número de bins para el histograma. Si es 0, se usa el método Freedman-Diaconis.
#'
#' @return
#' Una lista con los resultados del análisis descriptivo, guardada en el entorno global con el nombre
#' "<variable>_describe_info".
#'
#' @examples
#' \dontrun{
#' MATdescribe(data = mtcars, variable = mpg, bins = 10)
#' # Resultados: mpg_describe_info en el entorno global
#' }
#'
#' @export
MATdescribe <- function(data, variable, bins = 0) {

  if (!is.data.frame(data)) {
    stop("`data` debe ser un dataframe.")
  }

  variable <- deparse(substitute(variable))

  if (!variable %in% colnames(data)) {
    stop(paste0("La variable ", variable, " no existe en el dataframe."))
  }

  x <- data[[variable]]

  # Determinar bins
  if (!is.numeric(bins) || length(bins) != 1 || is.na(bins) || bins < 0) {
    stop("`bins` debe ser un número >= 0.")
  }

  if (bins == 0) {
    # Freedman-Diaconis (si x tiene NA, grDevices::nclass.FD los ignora mal, así que limpiamos)
    x_no_na <- x[!is.na(x)]
    if (length(x_no_na) < 2) {
      bins <- 1
    } else {
      bins <- grDevices::nclass.FD(x_no_na)
      if (is.na(bins) || bins < 1) bins <- 1
    }
  }

  # Estadísticos base para líneas y normal
  mu <- mean(x, na.rm = TRUE)
  sig <- stats::sd(x, na.rm = TRUE)

  # Histogram
  g1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[variable]])) +
    ggplot2::geom_histogram(bins = bins, colour = "red", fill = "orange", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = mu, color = "darkblue", linewidth = 1.2, alpha = 0.8) +
    ggplot2::ggtitle("Histograma") +
    ggplot2::xlab(variable) +
    ggplot2::ylab("Frecuencias")

  # Densidad + curva normal (solo si sd > 0)
  g2 <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[variable]])) +
    ggplot2::geom_density(colour = "red", fill = "orange", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = mu, color = "darkblue", linewidth = 0.8, alpha = 0.8) +
    ggplot2::ggtitle("Gráfico de densidad vs curva normal") +
    ggplot2::xlab(variable) +
    ggplot2::ylab("Densidad")

  if (is.finite(sig) && sig > 0) {
    g2 <- g2 +
      ggplot2::stat_function(
        fun = stats::dnorm,
        args = list(mean = mu, sd = sig),
        geom = "area",
        color = "darkblue",
        fill = "yellow",
        alpha = 0.2
      )
  }

  # Boxplot
  g3 <- ggplot2::ggplot(data = data, ggplot2::aes(x = "", y = .data[[variable]])) +
    ggplot2::geom_boxplot(color = "red", fill = "orange", outlier.shape = NA) +
    ggplot2::stat_summary(fun = "mean", geom = "point", size = 3, col = "darkblue") +
    ggplot2::geom_jitter(width = 0.1, size = 1, col = "darkred", alpha = 0.50) +
    ggplot2::ggtitle("Box-Plot") +
    ggplot2::ylab(variable)

  # QQ-Plot (con NA se maneja ok)
  g4 <- ggplot2::ggplot(data = data, ggplot2::aes(sample = .data[[variable]])) +
    ggplot2::stat_qq(colour = "red") +
    ggplot2::stat_qq_line(colour = "darkblue") +
    ggplot2::ggtitle("QQ-Plot")

  # Resumen de gráficos con patchwork
  resumen_graficos <- (g1 | g2) / (g3 | g4) +
    patchwork::plot_annotation(
      title = paste("Análisis gráfico de", variable),
      subtitle = "Construido con MATdatatools"
    )

  # Estadísticos descriptivos
  # (mantenemos tu estructura y etiquetas)
  estadisticos <- dplyr::summarise(
    data,
    Media = mean(.data[[variable]], na.rm = TRUE),
    DT = stats::sd(.data[[variable]], na.rm = TRUE),
    Mediana = stats::median(.data[[variable]], na.rm = TRUE),
    Mínimo = min(.data[[variable]], na.rm = TRUE),
    Máximo = max(.data[[variable]], na.rm = TRUE),
    Asimetría = moments::skewness(.data[[variable]], na.rm = TRUE),
    Curtosis = moments::kurtosis(.data[[variable]], na.rm = TRUE) - 3
  )

  tabla_estadisticos <- knitr::kable(
    estadisticos,
    caption = paste("Principales Estadísticos de", variable),
    col.names = c(
      "Media",
      "Desviación Típica",
      "Mediana",
      "Valor mínimo",
      "Valor máximo",
      "C. Asimetría Fisher",
      "C. Curtosis Fisher"
    )
  ) |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")

  # Pruebas de normalidad (robustas ante NA y tamaños pequeños)
  x_test <- x[!is.na(x)]

  # Shapiro: requiere 3..5000
  shapiro_p <- NA_real_
  if (length(x_test) >= 3) {
    if (length(x_test) > 5000) {
      # Shapiro no admite >5000: muestreamos 5000
      set.seed(123)
      x_sh <- sample(x_test, 5000)
    } else {
      x_sh <- x_test
    }
    shapiro_p <- stats::shapiro.test(x_sh)$p.value
  }

  # Lilliefors y Anderson-Darling (nortest) aceptan tamaños más grandes
  ks_p <- NA_real_
  ad_p <- NA_real_
  if (length(x_test) >= 3) {
    ks_p <- nortest::lillie.test(x_test)$p.value
    ad_p <- nortest::ad.test(x_test)$p.value
  }

  normalidad <- data.frame(
    Prueba = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Anderson-Darling"),
    `p-valor` = round(c(shapiro_p, ks_p, ad_p), 3),
    Conclusión = ifelse(
      c(shapiro_p, ks_p, ad_p) > 0.05,
      "NORMALIDAD",
      "NO-NORMALIDAD"
    )
  )

  # Si p-valor es NA, ponemos conclusión NA (para no engañar)
  normalidad$Conclusión[is.na(normalidad$`p-valor`)] <- NA

  tabla_normalidad <- knitr::kable(
    normalidad,
    caption = paste(variable, "- Pruebas de Normalidad"),
    col.names = c("Prueba", "p-valor", "Conclusión")
  ) |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")

  # Lista de resultados
  resultados <- list(
    grafico_resumen = resumen_graficos,
    estadisticos = tabla_estadisticos,
    normalidad = tabla_normalidad
  )

  # Guardar en el entorno global
  nombre_lista <- paste0(variable, "_describe_info")
  assign(nombre_lista, resultados, envir = .GlobalEnv)

  message(
    paste(
      "Los resultados del análisis descriptivo se guardaron en el entorno global con el nombre:",
      nombre_lista
    )
  )

  invisible(nombre_lista)
}
