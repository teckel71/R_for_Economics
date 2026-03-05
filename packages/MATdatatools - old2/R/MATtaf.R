#' MATtaf: Tabla de frecuencias agrupadas en intervalos
#'
#' Esta función crea una tabla con la distribución de frecuencias agrupadas en intervalos
#' para una variable numérica de un dataframe. Si no se especifica el número de intervalos,
#' se utiliza la regla de Freedman-Diaconis para determinarlo automáticamente. Además, genera un histograma
#' cuyos intervalos coinciden con los de la tabla.
#'
#' @param data Dataframe que contiene los datos.
#' @param variable Variable numérica para la que se calculará la distribución de frecuencias (sin comillas).
#' @param breaks (Opcional) Número de intervalos deseados. Si no se especifica, se calcula automáticamente con Freedman-Diaconis.
#'
#' @return
#' Una lista con la tabla de frecuencias y un histograma, guardada en el entorno global
#' con el nombre "<variable>_intervalos_frecuencia". Invisiblemente devuelve ese nombre.
#'
#' @examples
#' \dontrun{
#' MATtaf(data = mtcars, variable = mpg)
#' # Resultados: mpg_intervalos_frecuencia en el entorno global
#' }
#'
#' @export
MATtaf <- function(data, variable, breaks = NULL) {

  if (!is.data.frame(data)) {
    stop("`data` debe ser un dataframe.")
  }

  variable <- deparse(substitute(variable))

  if (!variable %in% colnames(data)) {
    stop(paste0("La variable ", variable, " no existe en el dataframe."))
  }

  # Ordenar el dataframe por la variable (mantiene tu lógica original)
  data_ord <- dplyr::arrange(data, .data[[variable]])

  # Cálculo del número de intervalos si no se especifica
  if (is.null(breaks)) {
    x <- data_ord[[variable]]

    # Si todos los valores iguales o hay muy pocos datos, evitamos errores en FD
    if (length(x) < 2 || stats::sd(x, na.rm = TRUE) == 0) {
      breaks <- 1
    } else {
      iqr <- stats::IQR(x, na.rm = TRUE)
      # Freedman–Diaconis: ancho bin = 2*IQR / n^(1/3)
      bin_width <- 2 * iqr / (nrow(data_ord)^(1/3))

      # Si bin_width sale 0 (p.ej. iqr=0), caemos a Sturges como alternativa segura
      if (is.na(bin_width) || bin_width <= 0) {
        breaks <- grDevices::nclass.Sturges(x)
      } else {
        breaks <- ceiling((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / bin_width)
        if (is.na(breaks) || breaks < 1) breaks <- 1
      }
    }
  }

  # Crear intervalos
  intervalos <- base::cut(data_ord[[variable]], breaks = breaks, include.lowest = TRUE)
  data_ord$intervalos <- intervalos

  # Conteo
  conteo_intervalos <- base::table(data_ord$intervalos)
  conteo_intervalos_df <- as.data.frame(conteo_intervalos)
  colnames(conteo_intervalos_df) <- c("Intervalo", "Frecuencia")

  # Totales y acumuladas
  N_agre <- sum(conteo_intervalos_df$Frecuencia)

  conteo_intervalos_df <- dplyr::mutate(
    conteo_intervalos_df,
    Frecuencia_acum = cumsum(Frecuencia),
    Frecuencia_R = Frecuencia / N_agre,
    Frecuencia_R_acum = cumsum(Frecuencia_R)
  )

  # Tabla con kable
  tabla <- knitr::kable(
    conteo_intervalos_df,
    caption = paste("Distribución de frecuencias agrupadas en intervalos de", variable),
    col.names = c(
      "Intervalo",
      "Frecuencia absoluta n(i)",
      "Frecuencia absoluta acum. N(i)",
      "Frecuencia relativa f(i)",
      "Frecuencia relativa acum. F(i)"
    ),
    format.args = list(decimal.mark = ".", digits = 2)
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "bordered", "condensed"),
      position = "center",
      font_size = 11
    ) |>
    kableExtra::row_spec(0, bold = TRUE, align = "c") |>
    kableExtra::row_spec(1:nrow(conteo_intervalos_df), bold = FALSE, align = "c")

  # Histograma coherente con los intervalos
  histograma <- ggplot2::ggplot(data_ord, ggplot2::aes(x = .data[[variable]])) +
    ggplot2::geom_histogram(bins = breaks, colour = "red", fill = "orange", alpha = 0.7) +
    ggplot2::geom_vline(
      xintercept = mean(data_ord[[variable]], na.rm = TRUE),
      color = "darkblue",
      linewidth = 1.2,
      alpha = 0.8
    ) +
    ggplot2::ggtitle(paste("Histograma de", variable)) +
    ggplot2::xlab(variable) +
    ggplot2::ylab("Frecuencia")

  # Guardar en entorno global
  nombre_lista <- paste0(variable, "_intervalos_frecuencia")
  assign(nombre_lista, list(tabla = tabla, histograma = histograma), envir = .GlobalEnv)

  message(
    paste(
      "La tabla de frecuencias agrupadas y el histograma se guardaron en el entorno global con el nombre:",
      nombre_lista
    )
  )

  invisible(nombre_lista)
}
