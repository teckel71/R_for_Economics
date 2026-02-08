#' MATfexcel: Importar datos desde una hoja de Excel
#'
#' Esta función importa datos desde una hoja de cálculo Excel, convierte la primera columna
#' en nombres de fila, y muestra un resumen de las variables. Además, si `viz = TRUE`,
#' genera una visualización con `gtExtras::gt_plt_summary()` y la guarda en el entorno global.
#'
#' Si la llamada se realiza como `obj <- MATfexcel(...)`, la visualización se guarda como `obj_viz`.
#' Si no se detecta el nombre del objeto de salida, se guarda como `MATfexcel_viz`.
#'
#' @param file_path Ruta al archivo Excel.
#' @param sheet_name Nombre o índice de la hoja de Excel a importar.
#' @param na_values Vector de caracteres que representan valores que deben tratarse como `NA`.
#'   Por defecto, las celdas vacías se consideran `NA`.
#' @param viz Lógico. Si es TRUE, se genera una visualización de las variables. Por defecto FALSE.
#'
#' @return Un dataframe con los datos importados (primera columna convertida a rownames).
#'
#' @examples
#' \dontrun{
#' ola <- MATfexcel("datos.xlsx", "Datos")
#' ola <- MATfexcel("datos.xlsx", "Datos", na_values = c("n.d.", "s.d."))
#' ola <- MATfexcel("datos.xlsx", "Datos", viz = TRUE)
#' # Visualización en ola_viz (si se detecta el nombre)
#' }
#'
#' @export
MATfexcel <- function(file_path, sheet_name, na_values = NULL, viz = FALSE) {

  # Validaciones básicas
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` debe ser una ruta (character) de longitud 1.")
  }
  if (!file.exists(file_path)) {
    stop(paste0("No se encuentra el archivo: ", file_path))
  }

  # Configurar NA: siempre tratamos "" como NA, y añadimos na_values si vienen
  na_vec <- ""
  if (!is.null(na_values)) {
    if (!is.character(na_values)) stop("`na_values` debe ser NULL o un vector character.")
    na_vec <- unique(c("", na_values))
  }

  # Leer Excel
  x <- readxl::read_excel(path = file_path, sheet = sheet_name, na = na_vec)

  # Convertir a data.frame
  data <- as.data.frame(x)

  # Comprobar que hay al menos 1 columna para usarla como rownames
  if (ncol(data) < 1) {
    stop("La hoja importada no contiene columnas.")
  }

  # Primera columna -> rownames
  rn <- data[[1]]
  data <- data[, -1, drop = FALSE]

  # Si la primera columna tiene NA o duplicados, lo resolvemos de forma segura
  rn <- as.character(rn)
  rn[is.na(rn) | rn == ""] <- paste0("row_", seq_len(length(rn))[is.na(rn) | rn == ""])
  rn <- make.unique(rn)

  rownames(data) <- rn

  # Resumen
  print(summary(data))

  # Visualización opcional
  if (isTRUE(viz)) {
    viz_output <- gtExtras::gt_plt_summary(data)

    # Intentar detectar el nombre del objeto asignado: obj <- MATfexcel(...)
    viz_name <- "MATfexcel_viz"
    parent_call <- tryCatch(sys.call(-1), error = function(e) NULL)

    if (!is.null(parent_call) && length(parent_call) >= 3) {
      # Caso típico: `<-`(lhs, MATfexcel(...))  o  `=`(lhs, MATfexcel(...))
      fun_name <- as.character(parent_call[[1]])
      if (fun_name %in% c("<-", "=")) {
        lhs <- parent_call[[2]]
        if (is.symbol(lhs)) {
          viz_name <- paste0(as.character(lhs), "_viz")
        }
      }
    }

    assign(viz_name, viz_output, envir = .GlobalEnv)
  }

  return(data)
}
