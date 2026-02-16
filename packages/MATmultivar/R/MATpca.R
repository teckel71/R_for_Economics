#' MATpca: Analisis de Componentes Principales
#'
#' Realiza un Analisis de Componentes Principales (PCA) sobre variables metricas.
#' Devuelve tablas de resumen y graficos de autovalores y varianza acumulada.
#'
#' @param data Dataframe con los datos.
#' @param ... Variables especificas (sin comillas) a incluir en el analisis.
#' @return Lista con el objeto PCA (prcomp) y una lista 'info' con tablas (kable) y graficos.
#' @export
MATpca <- function(data, ...) {
  # Chequeos m\u00ednimos (sin instalar paquetes dentro de la funci\u00f3n)
  pkgs <- c("dplyr", "ggplot2", "patchwork", "knitr", "kableExtra", "rlang")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("Falta el paquete '%s'. Inst\u00e1lalo con install.packages('%s').", p, p), call. = FALSE)
    }
  }

  variables <- rlang::quos(...)

  selected_data <- if (length(variables) == 0) {
    dplyr::select(data, dplyr::where(is.numeric))
  } else {
    dplyr::select(data, !!!variables)
  }

  if (ncol(selected_data) < 2) {
    stop("Se necesitan al menos dos variables num\u00e9ricas para realizar el PCA.", call. = FALSE)
  }

  pca_result <- stats::prcomp(selected_data, scale. = TRUE)

  # Resumen
  prop_varianza <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
  summary_df <- rbind(
    "Desviaci\u00f3n t\u00edpica" = pca_result$sdev,
    "Proporci\u00f3n de varianza" = prop_varianza,
    "Varianza acumulada" = cumsum(prop_varianza)
  )
  colnames(summary_df) <- paste0("PC", seq_len(ncol(summary_df)))

  # En las pr\u00e1cticas se indica expl\u00edcitamente que estas tablas son HTML.
  tabla_resumen <- knitr::kable(
    summary_df,
    caption = "Resumen de Componentes",
    format = "html",
    format.args = list(decimal.mark = ".", digits = 4)
  ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "bordered", "condensed"),
      full_width = FALSE,
      position = "center"
    ) |>
    kableExtra::row_spec(0, bold = TRUE, align = "c") |>
    kableExtra::column_spec(1, bold = TRUE, extra_css = "text-align: left;")

  # Cargas
  cargas <- as.data.frame(pca_result$rotation)
  tabla_cargas <- knitr::kable(
    cargas,
    caption = "Cargas de las Componentes",
    format = "html",
    format.args = list(decimal.mark = ".", digits = 4)
  ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "bordered", "condensed"),
      full_width = FALSE,
      position = "center"
    ) |>
    kableExtra::row_spec(0, bold = TRUE, align = "c") |>
    kableExtra::column_spec(1, bold = TRUE, extra_css = "text-align: left;")

  # Autovalores y varianza acumulada
  autovalores <- data.frame(
    Componente = seq_along(pca_result$sdev),
    Autovalor = pca_result$sdev^2
  )

  autograph <- ggplot2::ggplot(autovalores, ggplot2::aes(x = Componente, y = Autovalor)) +
    ggplot2::geom_bar(stat = "identity", colour = "red", fill = "orange", alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = autovalores$Componente) +
    ggplot2::geom_hline(yintercept = 1, colour = "darkblue") +
    ggplot2::geom_text(ggplot2::aes(label = round(Autovalor, 2)), vjust = -0.5, colour = "darkblue", size = 3) +
    ggplot2::ggtitle("Autovalores de las Componentes") +
    ggplot2::xlab("N\u00famero de Componente") +
    ggplot2::ylab("Autovalor")

  autovalores$Varianza_Acumulada <- cumsum(autovalores$Autovalor) / sum(autovalores$Autovalor) * 100

  vacumgraph <- ggplot2::ggplot(autovalores, ggplot2::aes(x = Componente, y = Varianza_Acumulada)) +
    ggplot2::geom_bar(stat = "identity", colour = "red", fill = "lightblue", alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = autovalores$Componente) +
    ggplot2::geom_text(ggplot2::aes(label = round(Varianza_Acumulada, 2)), vjust = -0.5, colour = "darkblue", size = 3) +
    ggplot2::ggtitle("Varianza Acumulada por Componentes") +
    ggplot2::xlab("N\u00famero de Componente") +
    ggplot2::ylab("Varianza Acumulada (%)")

  plot_combinado <- patchwork::wrap_plots(autograph, vacumgraph, ncol = 1)

  resultado_info <- list(
    resumen_componentes = tabla_resumen,
    cargas_componentes = tabla_cargas,
    graficos = plot_combinado
  )

  list(pca = pca_result, info = resultado_info)
}
