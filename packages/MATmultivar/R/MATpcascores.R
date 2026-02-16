#' MATpcascores: Obtencion de puntuaciones de componentes principales
#'
#' Calcula y devuelve puntuaciones de componentes principales de un PCA previamente realizado.
#'
#' @param data Dataframe de entrada (puede ser diferente al original).
#' @param pca_object Objeto resultante de stats::prcomp().
#' @param Nscores Numero de componentes principales a extraer (por defecto todas).
#' @param Rank Numero de casos a mostrar en tablas y graficos (por defecto 10).
#' @return Lista con el dataframe de puntuaciones y, para cada componente, una tabla (kable) y un grafico.
#' Ademas, por compatibilidad con las practicas del curso, guarda esta lista en el entorno global con un nombre
#' del tipo: scores_<data>_<pca>_info.
#' @export
MATpcascores <- function(data, pca_object, Nscores = NULL, Rank = 10) {
  pkgs <- c("dplyr", "ggplot2", "knitr", "kableExtra")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("Falta el paquete '%s'. Inst\u00e1lalo con install.packages('%s').", p, p), call. = FALSE)
    }
  }

  if (!inherits(pca_object, "prcomp")) {
    stop("El objeto PCA no es v\u00e1lido (debe ser de clase 'prcomp').", call. = FALSE)
  }

  pca_vars <- rownames(pca_object$rotation)

  missing_vars <- setdiff(pca_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(paste("Faltan variables en el nuevo dataframe:", paste(missing_vars, collapse = ", ")), call. = FALSE)
  }

  scores <- stats::predict(pca_object, newdata = data[pca_vars])

  if (is.null(Nscores)) {
    Nscores <- ncol(scores)
  } else {
    Nscores <- min(Nscores, ncol(scores))
  }

  scores <- as.data.frame(scores[, seq_len(Nscores), drop = FALSE])
  data_with_scores <- cbind(data, scores)

  na_cases <- sum(apply(scores, 1, function(x) any(is.na(x))))
  if (na_cases > 0) {
    message("No se pudieron calcular las puntuaciones para ", na_cases, " casos.")
  }

  resultados <- list()

  for (i in seq_len(Nscores)) {
    comp_name <- colnames(scores)[i]
    sorted_data <- dplyr::arrange(data_with_scores, dplyr::desc(.data[[comp_name]]))
    sorted_data <- dplyr::slice(sorted_data, seq_len(min(Rank, nrow(sorted_data))))

    case_id <- rownames(sorted_data)
    if (is.null(case_id)) case_id <- as.character(seq_len(nrow(sorted_data)))
    sorted_data$.case_id <- case_id

    # En las pr\u00e1cticas se trabaja con tablas HTML (kable + kableExtra).
    tabla <- sorted_data |>
      dplyr::select(dplyr::all_of(c(".case_id", comp_name, pca_vars))) |>
      knitr::kable(
        caption = paste("Top", min(Rank, nrow(sorted_data)), "casos para", comp_name),
        format = "html",
        format.args = list(decimal.mark = ".", digits = 4)
      ) |>
      kableExtra::kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "bordered", "condensed"),
        position = "center",
        font_size = 11
      )

    grafico <- ggplot2::ggplot(
      sorted_data,
      ggplot2::aes(x = stats::reorder(.case_id, .data[[comp_name]]), y = .data[[comp_name]])
    ) +
      ggplot2::geom_bar(stat = "identity", color = "red", fill = "orange", alpha = 0.6) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data[[comp_name]])), hjust = 1.2, size = 3, color = "black") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Top", min(Rank, nrow(sorted_data)), "casos en", comp_name), x = "Casos", y = paste("Valor en", comp_name)) +
      ggplot2::theme_minimal()

    resultados[[comp_name]] <- list(tabla = tabla, grafico = grafico)
  }

  out <- list(scores_df = data_with_scores, resultados = resultados)

  # --- Compatibilidad con gu\u00eda/pr\u00e1cticas ---
  # En los materiales docentes, la llamada MATpcascores(seleccion_sm_so, componentes$pca, ...)
  # crea un objeto llamado: scores_seleccion_sm_so_componentes_info
  df_name <- deparse(substitute(data))
  pca_expr <- deparse(substitute(pca_object))
  pca_base <- sub("\\$.*", "", pca_expr)
  assign(paste0("scores_", df_name, "_", pca_base, "_info"), out, envir = .GlobalEnv)

  out
}
