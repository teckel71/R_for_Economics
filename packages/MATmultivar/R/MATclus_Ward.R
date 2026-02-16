# Archivo: R/MATclus_Ward.R

#' MATclus_Ward: Clustering jerarquico con metodo de Ward
#'
#' Realiza un analisis de cluster jerarquico usando el metodo de Ward y, opcionalmente,
#' sugiere el numero de grupos con silueta.
#'
#' @param data Dataframe con los datos.
#' @param ... Variables especificas (sin comillas) a incluir en el clustering.
#' @param k Numero de clusters a formar. Si k = 0 no se asignan grupos (solo dendrograma).
#' @param silueta Booleano; si TRUE, calcula una sugerencia de k mediante silueta.
#' @return Lista con resultados (distancias, hclust, dendrograma, y opcionalmente tablas y graficos por grupo).
#' @export
MATclus_Ward <- function(data, ..., k = 0, silueta = FALSE) {
  pkgs <- c("factoextra", "ggplot2", "dplyr", "cluster", "knitr", "kableExtra", "patchwork", "rlang")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("Falta el paquete '%s'. Inst\u00e1lalo con install.packages('%s').", p, p), call. = FALSE)
    }
  }

  # Seleccionar variables
  vars <- rlang::quos(...)
  data_selected <- if (length(vars) == 0) {
    dplyr::select(data, dplyr::where(is.numeric))
  } else {
    dplyr::select(data, !!!vars)
  }

  if (ncol(data_selected) < 2) {
    stop("Se necesitan al menos dos variables num\u00e9ricas para el clustering.", call. = FALSE)
  }

  # Tipificar y distancias
  data_scaled <- scale(data_selected)
  dist_matrix <- stats::dist(data_scaled, method = "euclidean")

  out <- list()
  # En las pr\u00e1cticas se documenta como salida principal: heatmap, hclust, dendrogram y (opcionalmente) silhouette.
  out$heatmap <- factoextra::fviz_dist(dist_matrix)

  hclust_result <- stats::hclust(dist_matrix, method = "ward.D2")
  out$hclust <- hclust_result

  if (isTRUE(silueta)) {
    sil_result <- factoextra::fviz_nbclust(data_scaled, FUNcluster = factoextra::hcut, method = "silhouette")
    # l\u00ednea en el m\u00e1ximo (si existe la columna y)
    if (!is.null(sil_result$data$y)) {
      sil_result <- sil_result + ggplot2::geom_vline(
        xintercept = which.max(sil_result$data$y),
        linetype = "dashed",
        colour = "red"
      )
    }
    out$silhouette <- sil_result
  }

  # Dendrograma (con o sin k)
  if (k == 0) {
    out$dendrogram <- factoextra::fviz_dend(
      hclust_result,
      cex = 0.6,
      rect = FALSE,
      labels_track_height = 5.5,
      ggtheme = ggplot2::theme_gray()
    )
    return(out)
  }

  # Con k: asignaci\u00f3n de grupos + tablas y gr\u00e1ficos
  out$dendrogram <- factoextra::fviz_dend(
    hclust_result,
    cex = 0.6,
    k = k,
    k_colors = "black",
    labels_track_height = 5.5,
    rect = TRUE,
    rect_border = "black",
    rect_fill = TRUE,
    ggtheme = ggplot2::theme_gray()
  )

  group_assignments <- stats::cutree(hclust_result, k = k)

  # data_groups: datos originales + asignaci\u00f3n de grupo (sin columnas auxiliares)
  data_with_groups <- dplyr::mutate(
    data_selected,
    GRUPO = factor(paste("GRUPO", group_assignments))
  )

  out$data_groups <- data_with_groups

  summary_table <- data_with_groups |>
    dplyr::group_by(.data$GRUPO) |>
    dplyr::summarise(
      Obs = dplyr::n(),
      dplyr::across(dplyr::where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 3)),
      .groups = "drop"
    )

  out$group_summary <- knitr::kable(summary_table, format = "html", caption = "GRUPOS Y CENTROIDES") |>
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "bordered", "condensed"),
      position = "center",
      font_size = 11
    )

  # Tablas por grupo
  group_tables <- list()
  for (grupo in unique(data_with_groups$GRUPO)) {
    # Identificador de caso: si hay rownames las usamos (p.ej. nombres de empresas);
    # si no, usamos un \u00edndice.
    group_df <- data_with_groups |>
      dplyr::filter(.data$GRUPO == grupo)
    rn <- rownames(group_df)
    group_df$.case_id <- if (is.null(rn)) as.character(seq_len(nrow(group_df))) else rn
    group_data <- group_df |>
      dplyr::select(.data$.case_id, dplyr::everything(), - .data$GRUPO) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))

    group_tables[[as.character(grupo)]] <- knitr::kable(group_data, format = "html", caption = paste("Casos en", grupo)) |>
      kableExtra::kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "bordered", "condensed"),
        position = "center",
        font_size = 11
      )
  }
  out$group_tables <- group_tables

  # Gr\u00e1ficos de barras de medias
  bar_plots <- list()
  variables <- colnames(data_selected)
  for (var in variables) {
    grafico <- ggplot2::ggplot(summary_table, ggplot2::aes(x = GRUPO, y = .data[[var]])) +
      ggplot2::geom_bar(stat = "identity", colour = "red", fill = "orange", alpha = 0.7) +
      ggplot2::ggtitle(paste0(var, ". Media por grupos."), subtitle = "An\u00e1lisis de Cl\u00faster") +
      ggplot2::xlab("Grupo") +
      ggplot2::ylab(var)
    bar_plots[[paste0("barplot_", var)]] <- grafico
  }
  # (Las pr\u00e1cticas solo exponen bar_patchworks; evitamos a\u00f1adir elementos extra que puedan confundir.)

  # Dispersi\u00f3n por pares
  scatter_plots <- list()
  variable_combinations <- utils::combn(colnames(data_selected), 2, simplify = FALSE)

  for (comb in variable_combinations) {
    var1 <- comb[1]
    var2 <- comb[2]
    scatter_plot <- ggplot2::ggplot(data_with_groups, ggplot2::aes(x = .data[[var1]], y = .data[[var2]], color = GRUPO)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::geom_point(data = summary_table, ggplot2::aes(x = .data[[var1]], y = .data[[var2]], color = GRUPO), size = 2, shape = 16) +
      ggplot2::labs(title = paste("GR\u00c1FICO", var1, "-", var2), subtitle = "Funci\u00f3n: MATclus_Ward") +
      ggplot2::xlab(var1) +
      ggplot2::ylab(var2) +
      ggplot2::scale_color_brewer(palette = "Set1")
    scatter_plots[[paste0("scatter_", var1, "_", var2)]] <- scatter_plot
  }
  # (Las pr\u00e1cticas solo exponen scatter_patchworks.)

  # Composiciones (sin usar operadores patchwork)
  out$bar_patchworks <- patchwork::wrap_plots(bar_plots, ncol = 2)
  out$scatter_patchworks <- patchwork::wrap_plots(scatter_plots, ncol = 2)

  out
}
