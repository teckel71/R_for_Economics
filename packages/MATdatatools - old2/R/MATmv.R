#' MATmv: Manejo de datos con casos completos y NA
#'
#' Esta función selecciona variables de un dataframe, identifica casos completos y crea un resumen
#' con información sobre los datos faltantes. Si no se especifican variables, se utilizan todas las columnas.
#'
#' @param dataframe Un dataframe que contiene las variables.
#' @param ... Variables a seleccionar. Pueden ingresarse directamente sin comillas (ej. mpg, hp, wt).
#'
#' @return El nombre (character) del nuevo dataframe con los casos completos: "<dataframe>_sm".
#' @details
#' La función genera:
#' 1) Un nuevo dataframe con casos completos (según las columnas seleccionadas) en el entorno global:
#'    - "<dataframe>_sm"
#' 2) Una lista de información también en el entorno global:
#'    - "<dataframe>_sm_info"
#'    Contiene:
#'      - tabla_na: tabla HTML con los casos con NA (o NULL si no hay)
#'      - grafico_vis_miss: gráfico de visdat::vis_miss para las columnas seleccionadas
#'
#' @examples
#' # Usar todas las variables del dataframe:
#' MATmv(mtcars)
#'
#' # Usar varias variables sin comillas:
#' MATmv(mtcars, mpg, hp, wt)
#'
#' @export
MATmv <- function(dataframe, ...) {

  # Validación básica
  if (!is.data.frame(dataframe)) {
    stop("El primer argumento debe ser un dataframe.")
  }

  # Obtener los nombres de columnas pasadas en ...
  columnas <- as.character(substitute(list(...)))[-1]

  # Si no se especifican columnas, usar todas
  if (length(columnas) == 0) {
    columnas <- colnames(dataframe)
  }

  # Verificar que las columnas existen
  columnas_no_validas <- setdiff(columnas, colnames(dataframe))
  if (length(columnas_no_validas) > 0) {
    stop(
      paste(
        "Las siguientes columnas no existen en el dataframe:",
        paste(columnas_no_validas, collapse = ", ")
      )
    )
  }

  # Seleccionar variables y conservar nombres de fila originales
  df_seleccion <- dataframe[, columnas, drop = FALSE]
  rownames(df_seleccion) <- rownames(dataframe)

  # Casos completos respecto a df_seleccion, pero devolviendo TODAS las columnas del dataframe original
  df_sm <- dataframe[stats::complete.cases(df_seleccion), , drop = FALSE]

  # Nombre del nuevo dataframe
  nombre_df_sm <- paste0(deparse(substitute(dataframe)), "_sm")
  assign(nombre_df_sm, df_sm, envir = .GlobalEnv)

  # Casos con NA (solo en las columnas seleccionadas)
  df_na <- df_seleccion[!stats::complete.cases(df_seleccion), , drop = FALSE]

  # Lista de info
  lista_info <- list()

  if (nrow(df_na) > 0) {
    # Tabla HTML con casos con NA
    tabla_na <- knitr::kable(
      df_na,
      format = "html",
      caption = "Casos con datos faltantes"
    ) |>
      kableExtra::kable_styling(full_width = FALSE)

    lista_info$tabla_na <- tabla_na
    message("Se han encontrado casos con datos faltantes. La información está almacenada en la lista.")
  } else {
    message("No hay casos con datos faltantes en las variables seleccionadas.")
    lista_info$tabla_na <- NULL
  }

  # Gráfico de valores faltantes
  lista_info$grafico_vis_miss <- visdat::vis_miss(df_seleccion)

  # Guardar lista en entorno global
  nombre_lista <- paste0(nombre_df_sm, "_info")
  assign(nombre_lista, lista_info, envir = .GlobalEnv)

  # Devolver el nombre del nuevo dataframe
  return(nombre_df_sm)
}
